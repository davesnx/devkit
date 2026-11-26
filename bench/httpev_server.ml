open Devkit

let json =
  let item i = Printf.sprintf {|{"id":%d,"name":"item %d","tags":["a","b","c"],"score":%d.5}|} i i (i * 7) in
  "[" ^ String.concat "," (List.init 200 item) ^ "]"

let answer _server (req : Httpev.request) =
  match req.path with
  | "/hello" -> Httpev.Answer.text "hello"
  | "/json" -> Httpev.Answer.json json
  | _ -> Httpev.Answer.not_found "not found"

let () =
  match Sys.argv with
  | [| _; port |] ->
    Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
    Httpev.server_lwt
      {
        Httpev.default with
        connection = Unix.ADDR_INET (Unix.inet_addr_loopback, int_of_string port);
        access_log_enabled = false;
        exit_thread = None;
        backlog = 4096;
      }
      answer
  | _ -> prerr_endline "usage: httpev_server.exe PORT"; exit 2
