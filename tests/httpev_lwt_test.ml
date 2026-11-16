open Printf
open Devkit

let ( let* ) = Lwt.bind

let generator_ran = ref false

let answer _server (req : Httpev.request) =
  match req.meth, req.path with
  | `GET, "/hello" -> Httpev.Answer.text "hello"
  | `GET, "/no_reply" -> Lwt.return `No_reply
  | `HEAD, "/chunks" ->
    Lwt.return
      (`Chunks
        ( `Ok,
          [],
          fun push ->
            generator_ran := true;
            push "ab" ))
  | `POST, "/echo" ->
    Httpev.Answer.text (sprintf "%s|a=%s" req.body (Option.default "-" (Httpev.Param.get req "a")))
  | `GET, "/chunks" ->
    Lwt.return
      (`Chunks
        ( `Ok,
          [ "Content-Type", "text/plain" ],
          fun push ->
            let* () = push "ab" in
            push "cd" ))
  | _ -> Httpev.Answer.not_found "not found"

type response = {
  status : int;
  headers : (string * string) list;
  body : string;
}

let read_response ?(head = false) ic =
  let* status_line = Lwt_io.read_line ic in
  let status = Scanf.sscanf status_line "HTTP/1.%_d %d" Fun.id in
  let rec read_headers acc =
    let* line = Lwt_io.read_line ic in
    if line = "" then Lwt.return (List.rev acc)
    else
      match String.index_opt line ':' with
      | None -> failwith (sprintf "bad header line %S" line)
      | Some i ->
        let name = String.lowercase_ascii (String.sub line 0 i) in
        let value = String.trim (String.sub line (i + 1) (String.length line - i - 1)) in
        read_headers ((name, value) :: acc)
  in
  let* headers = read_headers [] in
  let read_exactly n =
    let b = Bytes.create n in
    let* () = Lwt_io.read_into_exactly ic b 0 n in
    Lwt.return (Bytes.to_string b)
  in
  let* body =
    if head then Lwt.return "" else
    match List.assoc_opt "content-length" headers, List.assoc_opt "transfer-encoding" headers with
    | Some n, _ -> read_exactly (int_of_string n)
    | None, Some "chunked" ->
      let buf = Buffer.create 16 in
      let rec chunks () =
        let* size_line = Lwt_io.read_line ic in
        match int_of_string ("0x" ^ String.trim size_line) with
        | 0 ->
          let* _ = Lwt_io.read_line ic in
          Lwt.return (Buffer.contents buf)
        | n ->
          let* data = read_exactly n in
          Buffer.add_string buf data;
          let* _ = Lwt_io.read_line ic in
          chunks ()
      in
      chunks ()
    | None, _ -> Lwt_io.read ic
  in
  Lwt.return { status; headers; body }

let with_server ?(config = Fun.id) ?(addr = Unix.ADDR_INET (Unix.inet_addr_loopback, 0)) f =
  let stop, signal_stop = Lwt.wait () in
  let listen = Lwt_unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
  let* () = Lwt_unix.bind listen addr in
  Lwt_unix.listen listen 16;
  let addr = Lwt_unix.getsockname listen in
  let config = config { Httpev.default with connection = addr; exit_thread = Some stop; access_log_enabled = false } in
  let server = Httpev.setup_fd_lwt listen config answer in
  Lwt.finalize
    (fun () -> f addr)
    (fun () ->
      Lwt.wakeup_later signal_stop ();
      let* () = server in
      Lwt_unix.close listen)

let with_connection addr f =
  let fd = Lwt_unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
  let* () = Lwt_unix.connect fd addr in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.input fd in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.output fd in
  Lwt.finalize (fun () -> f ic oc) (fun () -> Lwt_unix.close fd)

let send oc s =
  let* () = Lwt_io.write oc s in
  Lwt_io.flush oc

let get path = sprintf "GET %s HTTP/1.1\r\nHost: localhost\r\n\r\n" path

let post_echo body =
  sprintf
    "POST /echo HTTP/1.1\r\nHost: localhost\r\nContent-Type: application/x-www-form-urlencoded\r\nContent-Length: %d\r\n\r\n%s"
    (String.length body) body

let exchange request =
  with_server @@ fun addr ->
  with_connection addr @@ fun ic oc ->
  let* () = send oc request in
  read_response ic

let run f = Lwt_main.run (Lwt_unix.with_timeout 5. f)

let assert_response ?headers ~status ~body r =
  OUnit2.assert_equal ~printer:string_of_int status r.status;
  OUnit2.assert_equal ~printer:(sprintf "%S") body r.body;
  Option.may
    (List.iter (fun (k, v) -> OUnit2.assert_equal ~msg:k ~printer:(sprintf "%S") v (Option.default "" (List.assoc_opt k r.headers))))
    headers

let test_get _ =
  run @@ fun () ->
  let* r = exchange (get "/hello") in
  assert_response ~status:200 ~body:"hello" ~headers:[ "content-type", "text/plain" ] r;
  Lwt.return_unit

let test_post _ =
  run @@ fun () ->
  let* r = exchange (post_echo "a=1&b=2") in
  assert_response ~status:200 ~body:"a=1&b=2|a=1" r;
  Lwt.return_unit

let test_chunked _ =
  run @@ fun () ->
  let* r = exchange (get "/chunks") in
  assert_response ~status:200 ~body:"abcd" ~headers:[ "transfer-encoding", "chunked" ] r;
  Lwt.return_unit

let test_keep_alive _ =
  run @@ fun () ->
  with_server @@ fun addr ->
  with_connection addr @@ fun ic oc ->
  let* () = send oc (get "/hello") in
  let* r1 = read_response ic in
  assert_response ~status:200 ~body:"hello" ~headers:[ "connection", "" ] r1;
  let* () = send oc (post_echo "a=2") in
  let* r2 = read_response ic in
  assert_response ~status:200 ~body:"a=2|a=2" r2;
  Lwt.return_unit

let assert_closed ic =
  let* rest =
    Lwt.catch (fun () -> Lwt_io.read ic) (function
      | Unix.Unix_error (Unix.ECONNRESET, _, _) -> Lwt.return ""
      | exn -> Lwt.reraise exn)
  in
  OUnit2.assert_equal ~msg:"bytes after the response" ~printer:(sprintf "%S") "" rest;
  Lwt.return_unit

let test_no_reply _ =
  run @@ fun () ->
  with_server ~config:(fun c -> { c with max_clients = 1 }) @@ fun addr ->
  let* () =
    with_connection addr @@ fun ic oc ->
    let* () = send oc (get "/no_reply") in
    assert_closed ic
  in
  let* () = Lwt_unix.sleep 0.1 in
  with_connection addr @@ fun ic oc ->
  let* () = send oc (get "/hello") in
  let* r = read_response ic in
  assert_response ~status:200 ~body:"hello" r;
  Lwt.return_unit

let test_setup_failure_closes _ =
  run @@ fun () ->
  let path = Filename.temp_file "httpev_lwt_test" ".sock" in
  Sys.remove path;
  Lwt.finalize
    (fun () ->
      with_server ~addr:(Unix.ADDR_UNIX path) ~config:(fun c -> { c with nodelay = true }) @@ fun addr ->
      with_connection addr @@ fun ic _oc -> assert_closed ic)
    (fun () -> (try Sys.remove path with Sys_error _ -> ()); Lwt.return_unit)

let test_single _ =
  run @@ fun () ->
  with_server ~config:(fun c -> { c with single = true }) @@ fun addr ->
  let* () =
    with_connection addr @@ fun ic oc ->
    let* () = send oc (get "/hello") in
    let* r = read_response ic in
    assert_response ~status:200 ~body:"hello" ~headers:[ "connection", "close" ] r;
    assert_closed ic
  in
  with_connection addr @@ fun ic oc ->
  let* () = send oc (get "/hello") in
  let* r = read_response ic in
  assert_response ~status:200 ~body:"hello" r;
  Lwt.return_unit

let test_head_chunks _ =
  run @@ fun () ->
  generator_ran := false;
  with_server @@ fun addr ->
  with_connection addr @@ fun ic oc ->
  let* () = send oc "HEAD /chunks HTTP/1.1\r\nHost: localhost\r\n\r\n" in
  let* r = read_response ~head:true ic in
  OUnit2.assert_equal ~printer:string_of_int 200 r.status;
  OUnit2.assert_bool "generator ran for HEAD" (not !generator_ran);
  Lwt.return_unit

let test_head_timeout_drop _ =
  run @@ fun () ->
  with_server ~config:(fun c -> { c with max_time = { c.max_time with headers = 0.2 }; on_recv_timeout = `Drop_connection })
  @@ fun addr ->
  with_connection addr @@ fun ic oc ->
  let* () = send oc "GET /hello HTTP/1.1\r\n" in
  assert_closed ic

let test_body_timeout _ =
  run @@ fun () ->
  with_server ~config:(fun c -> { c with max_time = { c.max_time with body = 0.3 } }) @@ fun addr ->
  with_connection addr @@ fun ic oc ->
  let* () = send oc (post_echo "a=1") in
  let* r = read_response ic in
  assert_response ~status:200 ~body:"a=1|a=1" r;
  let* () = Lwt_unix.sleep 0.2 in
  let since = Unix.gettimeofday () in
  let* () = send oc "POST /echo HTTP/1.1\r\nHost: localhost\r\nContent-Length: 10\r\n\r\na=1" in
  let* r = read_response ic in
  let elapsed = Unix.gettimeofday () -. since in
  assert_response ~status:400 ~body:"" ~headers:[ "connection", "close" ] r;
  if elapsed < 0.25 || elapsed > 1. then OUnit2.assert_failure (sprintf "body timeout after %.3f s, not 0.3 s" elapsed);
  Lwt.return_unit

let test_malformed _ =
  run @@ fun () ->
  let* r = exchange "GARBAGE\r\n\r\n" in
  assert_response ~status:400 ~body:"" ~headers:[ "connection", "close" ] r;
  Lwt.return_unit

let live_words () =
  Gc.full_major ();
  (Gc.stat ()).live_words

let check_keep_alive_timers_bounded ~request ~body =
  let requests = 10_000 in
  let previous = Lwt_engine.get () in
  Lwt_engine.set ~destroy:false (new Lwt_engine.select);
  let growth =
    Fun.protect ~finally:(fun () -> Lwt_engine.set previous) @@ fun () ->
    Lwt_main.run @@ Lwt_unix.with_timeout 60. @@ fun () ->
    with_server @@ fun addr ->
    let live_sleeper = Lwt_unix.sleep 20. in
    with_connection addr @@ fun ic oc ->
    let rec exchanges n =
      if n = 0 then Lwt.return_unit
      else
        let* () = send oc request in
        let* r = read_response ic in
        assert_response ~status:200 ~body r;
        exchanges (n - 1)
    in
    let* () = exchanges (requests / 10) in
    let before = live_words () in
    let* () = exchanges requests in
    let growth = live_words () - before in
    Lwt.cancel live_sleeper;
    Lwt.return growth
  in
  if growth > requests then OUnit2.assert_failure (sprintf "heap grew by %d words over %d requests" growth requests)

let test_keep_alive_get_timers_bounded _ = check_keep_alive_timers_bounded ~request:(get "/hello") ~body:"hello"

let test_keep_alive_post_timers_bounded _ = check_keep_alive_timers_bounded ~request:(post_echo "a=1") ~body:"a=1|a=1"

let () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  OUnit2.run_test_tt_main
    OUnit2.(
      "httpev_lwt"
      >::: [
             "get" >:: test_get;
             "post" >:: test_post;
             "chunked" >:: test_chunked;
             "keep_alive" >:: test_keep_alive;
             "malformed" >:: test_malformed;
             "no_reply" >:: test_no_reply;
             "setup_failure_closes" >:: test_setup_failure_closes;
             "single" >:: test_single;
             "head_chunks" >:: test_head_chunks;
             "head_timeout_drop" >:: test_head_timeout_drop;
             "body_timeout" >:: test_body_timeout;
             "keep_alive_get_timers_bounded" >:: test_keep_alive_get_timers_bounded;
             "keep_alive_post_timers_bounded" >:: test_keep_alive_post_timers_bounded;
           ])
