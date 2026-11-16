(** Very simple and incomplete HTTP server *)

open Printf
open ExtLib
module Ev = Libevent

open Prelude

(* hide log *)
module Hidden = struct let log = Log.from "httpev" end
open Hidden

(** {2 Server} *)

type max_time = { headers : Time.t; body : Time.t; send : Time.t; }

(** server configuration *)
type config =
  {
    connection : Unix.sockaddr;
    backlog : int;
    log_epipe : bool;
    mutable debug : bool; (** more logging *)
    events : Ev.event_base;
    access_log : out_channel ref;
    access_log_enabled : bool;
    name : string;
    max_request_size : int;
    auth : (string * string * string) option;
    max_clients : int; (** limit on total number of requests in processing at any point of time *)
    max_data_childs : int;
    max_data_waiting : int;
    yield : bool;
      (** do [Lwt_unix.yield ()] after accepting connection to give other lwt threads chance to run (set to [true] when http requests
          processing causes other threads to stuck) *)
    single : bool; (** only process one request at a time (intended for preforked workers) *)
    exit_thread : unit Lwt.t option;
      (** if set, stop accepting connections as soon as exit_thread terminates (defaults to [Daemon.should_exit_lwt]) *)
    reuseport : bool;
    nodelay : bool;
    strict_args : bool;
      (** default false, if enabled - will in particular fail on "/path?arg1&arg2", why would anyone want that? *)
    max_time : max_time;
    cors_allow_all : bool; (* default false, if enabled - automatically set `Access-Control-Allow-Origin: *` for simple requests *)
    on_recv_timeout : [ `Reply_bad_request | `Drop_connection ]; (** what to do on receive timeout, [`Reply_bad_request] by default *)
  }

let default_max_time = {
  headers = Time.seconds 30;
  body = Time.minutes 1;
  send = Time.minutes 2;
}

let default =
  {
    connection = ADDR_INET (Unix.inet_addr_loopback, 8080);
    backlog = 100;
    log_epipe = false;
    debug = false;
    events = Ev.Global.base;
    name = "HTTP server";
    max_request_size = 256 * 1024;
    auth = None;
    max_clients = 10_000;
    access_log = ref stdout;
    access_log_enabled = true;
    max_data_childs = 50;
    max_data_waiting = 200;
    yield = true;
    single = false;
    exit_thread = Some Daemon.should_exit_lwt;
    reuseport = false;
    nodelay = false;
    strict_args = false;
    max_time = default_max_time;
    cors_allow_all = false;
    on_recv_timeout = `Reply_bad_request;
  }

include Httpev_common

type partial_body = {
  line1 : string;
  content_length : int option;
  parsed_headers : (string * string) list;
  buf : Buffer.t;
}

type request_state = Headers of Buffer.t | Body of partial_body | Body_lwt of int | Ready of request

(** client state *)
type client = {
  fd : Unix.file_descr;
  req_id : int;
  time_conn : Time.t; (** time when client connected *)
  sockaddr : Unix.sockaddr;
  mutable req : request_state;
  server : server;
}

(** server state *)
and server = {
  listen_socket : Unix.file_descr;
  mutable total : int;
  mutable active : int;
  mutable errors : int;
  mutable reject : int;
  reqs : (int,request) Hashtbl.t;
  clients : (int,client) Hashtbl.t;
  config : config;
  digest_auth : Digest_auth.t option;
  h_childs : (int,unit) Hashtbl.t; (** currently running forked childs *)
  q_wait : (unit -> unit) Stack.t; (** the stack of requests to fork *)
}

let incr_active s = s.active <- s.active + 1
let decr_active s = s.active <- s.active - 1
let incr_total  s = s.total <- s.total + 1
let incr_errors s = s.errors <- s.errors + 1
let incr_reject s = s.reject <- s.reject + 1

let make_server_state fd config =
  let digest_auth =
    match config.auth with
    | Some (realm,user,password) -> Some (Digest_auth.init ~realm ~user ~password ())
    | None -> None
  in
  {
    total = 0;
    active = 0;
    errors = 0;
    reject = 0;
    reqs = Hashtbl.create 10;
    clients = Hashtbl.create 10;
    config;
    digest_auth;
    listen_socket = fd;
    h_childs = Hashtbl.create 16;
    q_wait = Stack.create ();
  }

let show_socket_error fd =
  try
    match Unix.getsockopt_error fd with
    | None -> ""
    | Some err -> sprintf ", %s" (Unix.error_message err)
  with _ -> ""

let show_peer c =
  sprintf "%s (%s%s)"
    (Nix.show_addr c.sockaddr)
    (Time.duration_str (Time.now () -. c.time_conn))
    (show_socket_error c.fd)

let show_client c =
  match c.req with
  | Headers b -> sprintf "%s headers %s" (show_peer c) (Action.bytes_string @@ Buffer.length b)
  | Body b -> sprintf "%s body %s" (show_peer c) (Action.bytes_string @@ Buffer.length b.buf)
  | Body_lwt n -> sprintf "%s body %s" (show_peer c) (Action.bytes_string n)
  | Ready req -> show_request req

type ('a,'b) result = [ `Ok of 'a | `Error of 'b ]

let space = Pcre2.regexp "[ \t]+"

type reason = Url | Version | Method | Header | RequestLine | Split | Extra | NotAcceptable
exception Parse of reason * string
let failed reason s =
  let name =
  match reason with
  | Url -> "url" | Version -> "version" | Method -> "method"
  | RequestLine -> "RequestLine" | Split -> "split" | Header -> "header"
  | Extra -> "Extra"
  | NotAcceptable -> "Not Acceptable"
  in
  let s = Stre.shorten 1024 s in
  raise (Parse (reason, sprintf "%s : %S" name s))

let get_content_length headers =
  match Exn.catch (List.assoc "content-length") headers with
  | None -> None
  | Some s -> try Some (int_of_string s) with _ -> failed Header (sprintf "content-length %S" s)

let decode_args s =
  try Ocamlnet_lite.Netencoding.Url.dest_url_encoded_parameters s with exn -> Exn.fail ~exn "decode_args : %S" s

let acceptable_encoding headers =
  let split s c = List.map (String.strip ~chars:" \t\r\n") @@ Stre.nsplitc s c in
  match Exn.catch (List.assoc "accept-encoding") headers with
  | Some str ->
    let encodings = split str ',' |> List.filter_map begin fun s ->
      match split (String.lowercase_ascii s) ';' with
      | [ enc ] -> Some (enc, None)
      | [ enc; q ] -> Some (enc, Some q)
      | _ -> log #warn "bad accept-encoding record, ignoring : %S" s; None
    end in
    let test_available s =
      if List.mem (s, Some "q=0") encodings then false else
      if List.exists (fun (enc,_) -> enc = s) encodings then true else
      if List.mem ("*", Some "q=0") encodings then false else
      if List.exists (fun (enc,_) -> enc = "*") encodings then true else
      s = "identity" (* identity is always accepted, unless prohibited *)
    in
    if test_available "gzip" then Gzip else
    if test_available "identity" then Identity
    else Exn.fail "not acceptable : %S" str
  | None -> Identity

let make_request_exn ~line1 ~headers ~body c =
  match Pcre2.split ~rex:space line1 with
  | [meth;url;version] ->
    if url.[0] <> '/' then (* abs_path *)
      failed Url url;
    let version =
      try
        match String.split version "/" with
        | "HTTP", version -> apply2 int_of_string (String.split version ".")
        | _ -> raise Not_found
      with
        _ -> failed Version version
    in
    let meth = try method_of_string meth with Failure _ -> failed Method meth in
    let (path,args) = try String.split url "?" with _ -> url,"" in
    if version = (1,1) && not (List.mem_assoc "host" headers) then failed Header "Host is required for HTTP/1.1";
    let decode_args = if c.server.config.strict_args then decode_args else Web.parse_url_args $ String.strip in
    let args = decode_args args in
    let cont_type = try List.assoc "content-type" headers with _ -> "" in
    let args = if cont_type = "application/x-www-form-urlencoded" then List.append args @@ decode_args body else args in
    let encoding = try acceptable_encoding headers with Failure s -> failed NotAcceptable s in
    {
      url; path; args; headers; body; meth;
      id = c.req_id;
      addr = c.sockaddr;
      conn = c.time_conn;
      recv = Time.get ();
      version;
      blocking = None;
      line = line1;
      socket = c.fd;
      encoding;
    }
  | _ -> failed RequestLine line1

let extract_header s =
  let open String in
  try let (n,v) = split s ":" in lowercase_ascii (strip n), strip v with _ -> failed Header s

let extract_headers data =
  match String.nsplit data "\r\n" with
  | [] -> failed Split data
  | line1::xs -> line1, List.map extract_header xs

let is_body_ready { line1; content_length; parsed_headers=_; buf; } final =
  match content_length, Buffer.contents buf with
  | None, "" -> true
  | None, body -> failed Extra body
  | Some length, body ->
    let body_len = String.length body in
    match body_len - length with
    | 0 -> true
    (* workaround MSIE 6 *)
    | 2 when Stre.starts_with line1 "POST" && body.[body_len - 2] = '\r' && body.[body_len - 1] = '\n' ->
      Buffer.clear buf;
      Buffer.add_string buf (Stre.slice ~last:(-2) body);
      true
    | n when final || n > 0 -> Exn.fail "wrong content-length : %d <> %d" length body_len
    | _ -> false

(* let int_of_fd : Unix.file_descr -> int = Obj.magic *)

let teardown fd =
(*   Log.info "close %u" (int_of_fd fd); *)
  Exn.suppress (Unix.shutdown fd) Unix.SHUTDOWN_ALL;
  Unix.close fd

let finish ?(shutdown=true) c =
  decr_active c.server;
  Hashtbl.remove c.server.clients c.req_id;
  if shutdown then teardown c.fd else Unix.close c.fd;
  match c.req with
  | Headers _ | Body _ | Body_lwt _ -> ()
  | Ready req ->
    Hashtbl.remove c.server.reqs req.id;
    if c.server.config.debug then
      log #info "finished %s" (show_request req) ~structured_pairs:(pairs_of_request req)

let write_f c (data,ack) ev fd _flags =
  let finish () = finish c; Ev.del ev in
  let rec loop l ack =
    match l with
    | [] -> finish (); ([],0)
    | s::xs when String.length s = 0 -> loop xs 0 (* skip empty strings *)
    | s::xs ->
      try
        let len = Unix.single_write_substring fd s ack (String.length s - ack) in
        let ack = ack + len in
        if ack = String.length s then
          loop xs 0
        else
          loop l ack
      with
      | Unix.Unix_error (Unix.EAGAIN,_,_) -> (l,ack)
  in
  try
    let (l,a) = loop !data !ack in
    data := l;
    ack := a
  with
  exn ->
    incr_errors c.server;
    finish ();
    match c.server.config.log_epipe, exn with
    | false, Unix.Unix_error (Unix.EPIPE,_,_) -> ()
    | _ -> log #warn ~exn "write_f %s" (show_client c)

let log_access_apache ch code size ?(background=false) req =
  try
    let now = Time.now () in
    fprintf ch "%s - - [%s] %S %d %dB . %S %S %.3f %s %S%s\n%!"
      (show_client_addr req) (Time.to_string now) req.line code size
      (header_referer req) (header_safe req "user-agent") (now -. req.conn) (header_safe req "host")
      (header_safe req "x-request-id")
      (if background then " (BG)" else "")
  with exn ->
    log #warn ~exn "access log : %s" (show_request req) ~structured_pairs:(pairs_of_request req)

let log_access_logfmt ch code size ?(background=false) req =
  try
    let now = Time.now () in
    let msg = Logfmt.to_string [
      "time", Time.to_string ~gmt:!Log.State.utc_timezone ~ms:true now;
      "level", "info";
      "facil", "httpev";
      "msg", "served";
      "req_id", string_of_int req.id;
      "client_addr", show_client_addr req;
      "http_duration", sprintf "%.4f" (now -. req.conn);
      "http_recv_duration", sprintf "%.4f" (req.recv -. req.conn);
      "http_host", header_safe req "host";
      "url", req.url;
      "http_user_agent", header_safe req "user-agent";
      "http_req_id", header_safe req "x-request-id";
      "http_request_line", req.line;
      "http_status", string_of_int code;
      "size", string_of_int size;
      "http_referer", header_referer req;
      "background", string_of_bool background;
    ] in
    fprintf ch "%s\n%!" msg
  with exn ->
    log #warn ~exn "access log : %s" (show_request req) ~structured_pairs:(pairs_of_request req)

let emit_accesslog ch status size ?background req =
  let code = match status with
    | `No_reply -> None
    | `Code code -> Some code
    | #reply_status as code -> Some (status_code code)
  in
  match code with
  | None -> ()
  | Some code ->
    if Log.State.is_structured_format ()
    then log_access_logfmt ch code size ?background req
    else log_access_apache ch code size ?background req

(** Wait until [fd] becomes readable and close it (for eventfd-backed notifications) *)
let wait base fd k =
  Async.simple_event base fd [Ev.READ] begin fun ev fd _ ->
    Ev.del ev;
    Exn.suppress Unix.close fd;
    k ()
  end

let write_some fd s =
  let slen = String.length s in
  if slen = 0 then `Done else
  try
    let len = Unix.write_substring fd s 0 (String.length s) in
    if len = slen then `Done else `Some len
  with
  | Unix.Unix_error (Unix.EAGAIN,_,_) -> `Some 0

(** close transport connection, count as error *)
let abort c exn msg =
  incr_errors c.server;
  finish c;
  match c.server.config.log_epipe, exn with
  | false, Unix.Unix_error (Unix.EPIPE,_,_) -> ()
  | _ ->
    log #warn ~exn "abort %s %s" msg (show_client c)

let write_reply c l =
  let rec loop l =
    match l with
    | [] -> finish c
    | s::tl ->
      match write_some c.fd s with
      | `Some n -> Async.setup_simple_event c.server.config.events c.fd [Ev.WRITE] (write_f c (ref l,ref n))
      | `Done -> loop tl
  in
  try loop l with exn -> abort c exn "write_reply"

let write_reply_blocking c s =
  try
    let n = Unix.write_substring c.fd s 0 (String.length s) in
    assert (n = String.length s)
  with
    exn -> abort c exn "write_reply_blocking"

let set_blocking req =
  let ch = Unix.out_channel_of_descr req.socket in
  let encode = match req.encoding with Identity -> id | Gzip -> Gzip_io.output in
  let io = encode @@ IO.output_channel ch in
  req.blocking <- Some io;
  io

let make_request_headers ~version code hdrs =
  let b = Buffer.create 1024 in
  let put s = Buffer.add_string b s; Buffer.add_string b "\r\n" in
  put (show_http_reply ~version code);
  List.iter (fun (n,v) -> bprintf b "%s: %s\r\n" n v) hdrs;
  put "Connection: close";
  put "";
  Buffer.contents b

open struct
  let has_header_value name value hdrs =
    List.exists (fun (k,v) -> Stre.iequal k name && Stre.iequal (String.strip v) value) hdrs

  let is_application_gzip hdrs =
    List.exists begin fun (k,v) ->
      Stre.iequal k "content-type" && Stre.iequal (String.strip @@ Stre.before v ";") "application/gzip"
    end hdrs

  (* compress large bodies, if not already compressed *)
  let maybe_gzip encoding hdrs body =
    match encoding with
    | Gzip when String.length body > 128 &&
        not (has_header_value "content-encoding" "gzip" hdrs || is_application_gzip hdrs) ->
      ("Content-Encoding", "gzip") :: hdrs, Gzip_io.string body
    | _ -> hdrs, body
end

let send_reply_async c encoding (code,hdrs,body) =
  try
    let (hdrs,body) = maybe_gzip encoding hdrs body in
    let hdrs = ("Content-Length", string_of_int (String.length body)) :: hdrs in
    (* do not transfer body for HEAD requests *)
    let body = match c.req with Ready { meth = `HEAD; _ } -> "" | _ -> body in
    let version = `Http_1_0 in
    let headers = make_request_headers ~version code hdrs in
    if c.server.config.debug then
      log #info "will answer to %s with %d+%d bytes"
        (show_peer c)
        (String.length headers)
        (String.length body);
    write_reply c [headers;body]
  with
  | exn -> abort c exn "send_reply_async"

let send_reply_blocking c ~version (code,hdrs) =
  try
    write_reply_blocking c @@ make_request_headers ~version code hdrs
  with
    exn -> abort c exn "send_reply_blocking"; raise exn

let maybe_allow_cors c h =
  match c.req with
  | Ready req ->
    begin match c.server.config.cors_allow_all, req.meth, List.mem_assoc "origin" req.headers, List.mem_assoc "access-control-allow-origin" h with
    | true, (`GET|`HEAD|`POST), true, false -> ("Access-Control-Allow-Origin","*")::h
    | _ -> h
    end
  | _ -> h

(* this function is called back by user to actually send data *)
let send_reply_user c req (code,hdrs,body) =
  match code with
  | `No_reply -> finish ~shutdown:false c
  | #reply_status as code ->
  let hdrs =
    match hdrs with
    (* hack for answer_forked, which logs on its own *)
    | ("X-Disable-Log", "true") :: hs -> hs
    | _ ->
      if c.server.config.access_log_enabled then emit_accesslog !(c.server.config.access_log) code (String.length body) req;
      hdrs
  in
  let hdrs = maybe_allow_cors c hdrs in
  let blocking = Option.is_some req.blocking in
  (* filter headers *)
  let hdrs = hdrs |> List.filter begin fun (k,v) ->
    let open Stre in
    let forbidden =
      (iequal k "content-length" && not blocking) || (* httpev will calculate *)
      (iequal k "connection") ||
      (iequal k "content-encoding" && not (iequal (String.strip v) "gzip")) (* none of the user's business *)
    in
    not forbidden
  end in
  match blocking with
  | true ->
    (* this is forked child, events are gone, so write to socket with blocking *)
    Unix.clear_nonblock c.fd;
    let hdrs = match req.encoding with Identity -> hdrs | Gzip -> ("Content-Encoding", "gzip") :: hdrs in
    send_reply_blocking c ~version:`Http_1_0 (code,hdrs);
  | false ->
    send_reply_async c req.encoding (code,hdrs,body)

let make_error = function
| Parse (what,msg) ->
  let error = match what with
  | Url | RequestLine | Header | Split | Version | Extra -> `Bad_request
  | Method -> `Not_implemented
  | NotAcceptable -> `Not_acceptable
  in
  error, msg
| Failure s -> `Bad_request, s
| exn -> `Bad_request, Exn.str exn

let send_reply_limit c n =
  log #info "request too large from %s : %s" (show_client c) (Action.bytes_string n);
  send_reply_async c Identity (`Request_too_large,[],"request entity too large")

let handle_request c body answer =
  let req =
    let { line1; parsed_headers=headers; buf; _ } = body in
    make_request_exn c ~line1 ~headers ~body:(Buffer.contents buf)
  in
  Hashtbl.replace c.server.reqs req.id req;
  c.req <- Ready req;
  try
    match req.version with
    | (1,_) ->
      let auth = match c.server.digest_auth with
      | Some auth -> Digest_auth.check auth req
      | None -> `Ok
      in
      let k = send_reply_user c req in
      begin match auth with
      | `Unauthorized header -> k (`Unauthorized, [header], "Unauthorized")
      | `Ok -> answer c.server req k
      end
    | _ ->
      log #info "version %u.%u not supported from %s" (fst req.version) (snd req.version) (show_request req) ~structured_pairs:(pairs_of_request req);
      send_reply_async c Identity (`Version_not_supported,[],"HTTP/1.0 is supported")
  with exn ->
    log #error ~exn "answer %s" (show_request req) ~structured_pairs:(pairs_of_request req);
    match req.blocking with
    | None -> send_reply_async c Identity (`Not_found,[],"Not found")
    | Some _ -> Exn.suppress teardown c.fd

let rec process_chunk c ev answer data final =
  match c.req with
  | Body_lwt _ -> assert false
  | Headers buf | Body { buf; _ } when String.length data + Buffer.length buf > c.server.config.max_request_size ->
    Ev.del ev;
    send_reply_limit c (String.length data + Buffer.length buf)
  | Headers buf ->
    Buffer.add_string  buf data;
    let input = Buffer.contents buf in
    begin match try Some (String.split input "\r\n\r\n") with _ -> None with
    | None -> if final then failed Split input (* continue reading headers *)
    | Some (headers,part) ->
      let (line1,headers) = extract_headers headers in
      let content_length = get_content_length headers in
      (* TODO transfer-encoding *)
      if List.mem_assoc "transfer-encoding" headers then Exn.fail "Transfer-Encoding not supported";
      Buffer.clear buf;
      let body = { line1; parsed_headers=headers; content_length; buf; } in
      c.req <- Body body;
      process_chunk c ev answer part final
    end
  | Body body ->
    Buffer.add_string body.buf data;
    if is_body_ready body final then
      (Ev.del ev; handle_request c body answer)
    else if final then
      failed Split (Buffer.contents body.buf)
  | Ready req ->
    if data = "" && final = true then
      (log #warn "STRANGE %s %B" (show_request req) final; failed Split "")
    else
      failed Extra data

let handle_client c answer =
  Async.setup_simple_event c.server.config.events c.fd [Ev.READ] begin fun ev fd _ ->
    try
      match Async.read_available ~limit:c.server.config.max_request_size fd with
      | `Limit s -> Ev.del ev; send_reply_limit c (String.length s)
      | `Chunk (data,final) -> process_chunk c ev answer data final
    with exn ->
      Ev.del ev;
      let (http_error,msg) = make_error exn in
      log #warn "error for %s : %s" (show_client c) msg;
      send_reply_async c Identity (http_error,[],"")
  end

module Tcp = struct

open Unix

let listen ~name ?(backlog=100) ?(reuseport=false) addr =
  let domain = domain_of_sockaddr addr in
  let fd = socket ~cloexec:true domain SOCK_STREAM 0 in
  try
    setsockopt fd SO_REUSEADDR true;
    begin match domain, reuseport with
    | PF_UNIX, true -> log #warn "TCP.listen %s : reuseport doesn't make sense for unix socket, ignoring" (Nix.show_addr addr)
    | PF_INET, true -> U.setsockopt fd SO_REUSEPORT true
    | _ -> ()
    end;
    bind fd addr;
    listen fd backlog;
    log #info "%s listen TCP %s" name (Nix.show_addr addr);
    fd
  with exn -> log #warn ~exn "%s listen TCP %s failed" name (Nix.show_addr addr); close fd; raise exn

let handle events fd k =
  set_nonblock fd;
  let rec setup () =
  Async.setup_simple_event events fd [Ev.READ] begin fun ev fd _ ->
    try
      while true do (* accept as much as possible, socket is nonblocking *)
        let peer = accept ~cloexec:true fd in
        try
          k peer
        with
          exn -> log #error ~exn "accepted (%s)" (Nix.show_addr (snd peer))
      done
    with
    | Unix_error(EAGAIN,_,_) -> ()
    | exn ->
(*
      log #error ~exn "accept (total requests %d)" (Hashtbl.length status.reqs);
      Hashtbl.iter (fun _ req -> log #error "%s" (show_request req)) status.reqs;
*)
      match exn with
      | Unix_error(EMFILE,_,_) ->
        let tm = 2. in
        log #error "disable listening socket for %s " (Time.duration_str tm);
        Ev.del ev;
        let timer = Ev.create () in
        Ev.set_timer events timer ~persist:false (fun () ->
          Ev.del timer; log #info "reenabling listening socket"; setup ());
        Ev.add timer (Some tm)
      | _ -> ()
  end
  in
  setup ()

end

let check_hung_requests server =
  let now = Time.now () in
  server.reqs |> Hashtbl.iter begin fun _ req ->
    if req.recv -. now > Time.minutes 30 then
      log #warn "request takes too much time to process : %s" (show_request req) ~structured_pairs:(pairs_of_request req)
  end

let check_waiting_requests srv =
  while not (Stack.is_empty srv.q_wait) && Hashtbl.length srv.h_childs < srv.config.max_data_childs do
    let f = Stack.pop srv.q_wait in
    begin try let () = f () in () with exn -> log #warn ~exn "q_wait" end
  done

let finish_child srv pid =
(*     log #info "child %d reaped" pid; *)
    match Hashtbl.find_option srv.h_childs pid with
    | Some () ->
      Hashtbl.remove srv.h_childs pid;
      check_waiting_requests srv
    | None -> log #warn "no handler for child %d" pid

let reap_orphans srv =
  let rec loop () =
    match Exn.catch (Unix.waitpid [Unix.WNOHANG]) 0 with
    | None | Some (0,_) -> ()
    | Some (pid,st) -> log #info "reaped orphan %d %S" pid (Std.dump st); finish_child srv pid; loop ()
  in loop ()

let start_listen config =
  Tcp.listen ~name:config.name ~backlog:config.backlog ~reuseport:config.reuseport config.connection

let setup_server_fd fd config answer =
  let server = make_server_state fd config in
  Async.setup_periodic_timer_wait config.events (Time.minutes 1) (fun () -> check_hung_requests server);
  Async.setup_periodic_timer_now config.events 10. (fun () -> reap_orphans server);
  Tcp.handle config.events fd begin fun (fd,sockaddr) ->
    incr_total server;
    let req_id = server.total in
    match server.active >= config.max_clients with
    | true ->
      incr_reject server;
      if config.debug then log #info "rejected #%d %s" req_id (Nix.show_addr sockaddr);
      teardown fd
    | false ->
      incr_active server;
      let client = { fd; req_id; sockaddr; time_conn=Time.get (); server; req=Headers (Buffer.create 1024); } in
      Hashtbl.replace server.clients req_id client;
      Unix.set_nonblock fd;
      if config.debug then log #info "accepted #%d %s" req_id (Nix.show_addr sockaddr);
      handle_client client answer
  end;
  server

let setup_server config answer =
  let fd = start_listen config in
  setup_server_fd fd config answer

let setup_fd fd config answer = let (_:server) = setup_server_fd fd config answer in ()
let setup config answer = let (_:server) = setup_server config answer in ()

let server config answer =
  setup config answer;
  Ev.dispatch config.events

let header n v = n,v
let forbidden = `Forbidden, [], "forbidden"
let not_found = `Not_found, [], "not found"
let found url = `Found,[header "Location" url], "found"
let moved url = `Moved,[header "Location" url], "moved permanently"
let cache_no = [header "Pragma" "no-cache"; header "Cache-Control" "max-age=0"]
let cache_yes t = [header "Last-Modified" (Time.to_rfc2822 t)]

(*
let answer st url =
  match url with
  | "/test" ->
    let body = sprintf "answer %s\n%s" url (String.create 102400) in
    `Ok,[],answer
  | _ -> not_found

let () =
  server (Unix.ADDR_INET (Unix.inet_addr_any, 8081)) answer
*)

(** {2 Utilities} *)

(** get request params *)
module Param = struct
  let get_exn req name = List.assoc name req.args
  exception Bad of string
  let get req = Exn.catch (get_exn req)
  let get_int req = Exn.catch (int_of_string $ get_exn req)
  let make f req ?default name =
    match get req name, default with
    | None, None -> raise (Bad name)
    | None, Some s -> s
    | Some s, _ -> try f s with _ -> raise (Bad name)
  let str = make id
  let int64 = make Int64.of_string
  let int = make int_of_string
  let float = make float_of_string
  let bool = make bool_of_string
  let array req name =
    let name = name ^ "[]" in
    req.args |> List.filter (fun (name',_) -> name = name') |> List.map snd
end

module type Args = sig

  val req : request

  exception Bad of string

  val get : string -> string option
  (** Get optional parameter. @return None if parameter is missing *)

  val str : ?default:string -> string -> string
  (** Get required parameter. @raise Bad if parameter is missing and no [default] provided *)

  val get_int : string -> int option
  (** Get optional integer parameter *)

  val int : ?default:int -> string -> int
  (** Get integer parameter. @raise Bad if parameter is missing and no [default] provided *)

  val bool : ?default:bool -> string -> bool
  (** Get boolean parameter. @return [true] when parameter value is ["true"], [false] otherwise. @raise Bad if missing and no [default] provided. *)

  val float : ?default:float -> string -> float
  val int64 : ?default:int64 -> string -> int64
  val array : string -> string list
  (** @param name array name without brackets e.g. [array "x"] to extract [x] from /request?x[]=1&x[]=2 *)
end

(** functor version of {!Param} because somebody thought it is good idea *)
module Args(T : sig val req : request end) : Args =
struct
  include Param
  let req = T.req
  let get = get req
  let get_int = get_int req
  let str = str req
  let int64 = int64 req
  let int = int req
  let float = float req
  let bool = bool req
  let array = array req
end

let noclose_io io =
  IO.create_out
    ~write:(IO.write io)
    ~output:(IO.output io)
    ~flush:(fun () -> IO.flush io)
    ~close:(fun () -> ())

(** Buffers all output *)
let output (f : 'a IO.output -> unit) =
  let out = IO.output_string () in
  f @@ noclose_io out;
  IO.close_out out

let serve (_req : request) ?status ?(extra=[]) ctype data =
  Option.default `Ok status, ("Content-Type",ctype) :: extra, data

let serve_io (req : request) ?status ?extra ctype (f : 'a IO.output -> unit) =
  serve req ?status ?extra ctype (output f)

let serve_text_io req ?status =
  serve_io req ?status "text/plain"

let serve_gzip_io req ?status f =
  serve_io req ?status "application/gzip" (fun io ->
    Control.with_output (Gzip_io.output io) f)

let serve_text req ?status text =
  serve req ?status "text/plain" text

(** Return a body that's already gzip-encoded, add corresponding content-encoding *)
let serve_encoded_gzip req ?status ?(extra=[]) ctype data =
  match req.encoding with
  | Gzip -> serve req ?status ~extra:(("Content-Encoding", "gzip") :: extra) ctype data
  | Identity -> Exn.fail "client does not accept gzip encoding"

let run ?(ip=Unix.inet_addr_loopback) port answer =
  server { default with connection = ADDR_INET (ip, port) } answer

let run_unix path answer =
  server { default with connection = ADDR_UNIX path } answer

(** {2 Forked workers} *)

let check_req req = match Unix.getsockopt_error req.socket with None -> `Ok | Some err -> `Error err
let check_req_exn req = match check_req req with `Ok -> () | `Error err -> Exn.fail "%s" (Unix.error_message err)

exception Continue of (unit -> unit)

let answer_blocking ?(debug=false) srv req answer k =
  let count = ref 0L in
  let (code, continue) =
    try
      if debug then Printexc.record_backtrace true;
      Control.with_output (set_blocking req) begin fun io -> (* TODO EPIPE closes fd, EBADF afterwards *)
        let io = Action.count_bytes_to count io in
        let pre h =
          k (`Ok, ("X-Disable-Log", "true") :: h,"");
          check_req_exn req
        in
        check_req_exn req;
        let () = answer pre io in
        200, None
      end
    with
    | Continue continue -> 200, Some continue
    | exn ->
      let saved_backtrace = Exn.get_backtrace () in
      log #warn ~exn ~backtrace:debug ~saved_backtrace "answer forked %s" (show_request req) ~structured_pairs:(pairs_of_request req);
      -1, None
  in
  if srv.config.access_log_enabled then
    emit_accesslog !(srv.config.access_log) (`Code code) (Int64.to_int !count) ~background:(continue <> None) req;
  call_me_maybe continue ()

let stats = new Var.typ "httpev.forks" "k"

let nr_forked = stats#count "forked"
let nr_queued = stats#count "queued"
let nr_rejected = stats#count "rejected"

let answer_forked ?debug srv req answer k =
  let do_fork () =
    match check_req req with
    | `Error err -> Exn.fail "pre fork %s : %s" (show_request req) (Unix.error_message err)
    | `Ok ->
    begin match Nix.fork () with
    | `Child ->
      Exn.suppress Unix.close srv.listen_socket;
      begin try
          answer_blocking ?debug srv req answer k;
        with exn ->
          log #error ~exn "unhandled exception in continuation callback"
      end;
      U.sys_exit 0
    | `Forked pid ->
      log #info "forked %d : %s" pid (show_request req) ~structured_pairs:(pairs_of_request req);
      k (`No_reply,[],""); (* close socket in parent immediately *)
      Hashtbl.add srv.h_childs pid ()
    end
  in
  let do_fork () =
    try
      do_fork ()
    with
      exn ->
        log #warn ~exn "answer fork failed %s" (show_request req) ~structured_pairs:(pairs_of_request req);
        k (`Internal_server_error,[],"")
  in
  if Hashtbl.length srv.h_childs < srv.config.max_data_childs then
  begin
    incr nr_forked;
    do_fork ()
  end
  else if Stack.length srv.q_wait < srv.config.max_data_waiting then
  begin
    incr nr_queued;
    Stack.push do_fork srv.q_wait
  end
  else
  begin
    incr nr_rejected;
    log #info "rejecting, overloaded : %s" (show_request req) ~structured_pairs:(pairs_of_request req);
    k (`Service_unavailable, ["Content-Type", "text/plain"], "overloaded")
  end

(** {2 Lwt support} *)

let filter_reply_headers hdrs =
  hdrs |> List.filter begin fun (k,v) ->
    let open Stre in
    let forbidden =
      (iequal k "content-length") || (* httpev will calculate *)
      (iequal k "connection") ||
      (iequal k "transfer-encoding") ||
      (iequal k "content-encoding" && not (iequal (String.strip v) "gzip")) (* none of the user's business *)
    in
    not forbidden
  end

let ache_status = function
  | `Custom line ->
    begin
      try Scanf.sscanf line "HTTP/%_d.%_d %d %[^\r\n]" (fun code reason -> Ache.Status.of_code code, Some reason)
      with _ ->
        log #warn "bad custom status line %S" line;
        `Internal_server_error, None
    end
  | #status_code as code -> Ache.Status.of_code (status_code code), None

let ache_response ?(close=false) code (hdrs : (string * string) list) =
  let status, reason = ache_status code in
  let hdrs = if close then hdrs @ ["Connection", "close"] else hdrs in
  Ache.Response.create ?reason ~headers:(Ache.Headers.of_list hdrs) status

let respond_empty ?close reqd code = Ache.Reqd.respond_with_string reqd (ache_response ?close code []) ""

exception Drop_connection

let drop_connection reqd = Ache.Reqd.report_exn reqd Drop_connection

let flush_writer writer =
  let (p,u) = Lwt.wait () in
  Ache.Body.Writer.flush writer (function
    | `Written -> Lwt.wakeup_later u ()
    | `Closed -> Lwt.wakeup_later_exn u (Failure "connection closed"));
  p

let read_body reqd limit =
  match Ache.Request.body_length (Ache.Reqd.request reqd) with
  | `Fixed n when Int64.compare n (Int64.of_int limit) > 0 -> Lwt.return (Error `Too_large)
  | _ ->
    let body = Ache.Reqd.request_body reqd in
    let buf = Buffer.create 0 in
    let (p,u) = Lwt.wait () in
    let rec on_read bs ~off ~len =
      if Buffer.length buf + len > limit then begin
        Ache.Body.Reader.close body;
        Lwt.wakeup_later u (Error `Too_large)
      end
      else begin
        Buffer.add_string buf (Ache.Bigstring.substring bs ~off ~len);
        Ache.Body.Reader.schedule_read body ~on_eof ~on_error ~on_read
      end
    and on_eof () = Lwt.wakeup_later u (Ok (Buffer.contents buf))
    and on_error () = Lwt.wakeup_later u (Error `Incomplete) in
    Ache.Body.Reader.schedule_read body ~on_eof ~on_error ~on_read;
    p

type body_deadline = {
  queue : body_deadlines;
  mutable at : float;
  mutable prev : body_deadline;
  mutable next : body_deadline;
  mutable expired : unit Lwt.t * unit Lwt.u;
}

and body_deadlines = { seconds : float; root : body_deadline; mutable sleeping : bool }

let new_body_deadlines seconds =
  let rec queue = { seconds; root; sleeping = false }
  and root = { queue; at = infinity; prev = root; next = root; expired = Lwt.wait () } in
  queue

let new_body_deadline queue =
  let rec d = { queue; at = 0.; prev = d; next = d; expired = Lwt.wait () } in
  d

let stop_body_deadline d =
  d.prev.next <- d.next;
  d.next.prev <- d.prev;
  d.prev <- d;
  d.next <- d

let rec expire_body_deadlines queue () =
  let now = Unix.gettimeofday () in
  let rec expire () =
    let d = queue.root.next in
    if d != queue.root && d.at <= now then begin
      stop_body_deadline d;
      Lwt.wakeup_later (snd d.expired) ();
      expire ()
    end
  in
  expire ();
  let front = queue.root.next in
  if front == queue.root then queue.sleeping <- false
  else Lwt.on_success (Lwt_unix.sleep (front.at -. now)) (expire_body_deadlines queue)

let wait_body_deadline d =
  let queue = d.queue in
  stop_body_deadline d;
  if not (Lwt.is_sleeping (fst d.expired)) then d.expired <- Lwt.wait ();
  d.at <- Unix.gettimeofday () +. queue.seconds;
  d.prev <- queue.root.prev;
  d.next <- queue.root;
  queue.root.prev.next <- d;
  queue.root.prev <- d;
  if not queue.sleeping then begin
    queue.sleeping <- true;
    Lwt.on_success (Lwt_unix.sleep queue.seconds) (expire_body_deadlines queue)
  end;
  Lwt.protected (fst d.expired)

let send_reply c reqd req reply =
  let (code,hdrs,body) = match reply with
  | `Body (code,hdrs,s) -> code, hdrs, `Body s
  | `Chunks (code,hdrs,gen) -> code, hdrs, `Chunks gen
  in
  let body = match req.meth with `HEAD -> `Body "" | _ -> body in
  let close = c.server.config.single in
  let hdrs = maybe_allow_cors c hdrs in
  let size = match body with `Body s -> String.length s | `Chunks _ -> 0 in
  if c.server.config.access_log_enabled then emit_accesslog !(c.server.config.access_log) code size req;
  let hdrs = filter_reply_headers hdrs in
  match body with
  | `Body s ->
    let (hdrs,s) = match code with `Ok -> maybe_gzip req.encoding hdrs s | _ -> hdrs, s in
    Ache.Reqd.respond_with_string reqd (ache_response ~close code hdrs) s;
    Lwt.return_unit
  | `Chunks gen ->
    let writer = Ache.Reqd.respond_with_streaming reqd (ache_response ~close code hdrs) in
    let push = function
    | "" -> Lwt.return_unit
    | s -> Ache.Body.Writer.write_string writer s; flush_writer writer
    in
    try%lwt
      let%lwt () = gen push in
      Ache.Body.Writer.close writer;
      Lwt.return_unit
    with exn ->
      (* do not write trailer on error - let the peer notice the breakage *)
      log #warn ~exn "generate failed";
      Ache.Reqd.report_exn reqd exn;
      Lwt.return_unit

let handle_request_lwt c req answer =
  let return x = Lwt.return @@ `Body x in
  match req.version with
  | (1,_) ->
    let auth = match c.server.digest_auth with
    | Some auth -> Digest_auth.check auth req
    | None -> `Ok
    in
    begin match auth with
    | `Unauthorized header -> return (`Unauthorized, [header], "Unauthorized")
    | `Ok ->
      try%lwt
        answer c.server req
      with exn ->
        log #error ~exn "answer %s" (show_request req) ~structured_pairs:(pairs_of_request req);
        return (`Not_found,[],"Not found")
    end
  | _ ->
    log #info "version %u.%u not supported from %s" (fst req.version) (snd req.version) (show_request req) ~structured_pairs:(pairs_of_request req);
    return (`Version_not_supported,[],"HTTP/1.0 is supported")

let handle_request_ache server fd deadline sockaddr answer reqd =
  let cfg = server.config in
  let ache_req = Ache.Reqd.request reqd in
  incr_total server;
  let req_id = server.total in
  let client = { fd = Lwt_unix.unix_file_descr fd; req_id; sockaddr; time_conn = Time.get (); server; req = Body_lwt 0 } in
  Hashtbl.replace server.clients req_id client;
  let line1 =
    sprintf "%s %s HTTP/%d.%d"
      (Ache.Method.to_string ache_req.meth) ache_req.target ache_req.version.major ache_req.version.minor
  in
  let headers = List.map (fun (n,v) -> String.lowercase_ascii n, String.strip v) (Ache.Headers.to_list ache_req.headers) in
  let process () =
    let%lwt body =
      match Ache.Request.body_length ache_req with
      | `Fixed 0L -> Lwt.return (Ok "")
      | _ ->
        Lwt.pick [
          read_body reqd cfg.max_request_size;
          (let%lwt () = wait_body_deadline deadline in Lwt.return (Error `Timeout));
        ]
    in
    match body with
    | Error `Incomplete -> Lwt.return_unit
    | Error `Too_large ->
      incr_errors server;
      log #info "request too large from %s" (show_peer client);
      respond_empty ~close:true reqd `Request_too_large;
      Lwt.return_unit
    | Error `Timeout ->
      incr_errors server;
      log #info "body timeout from %s" (show_peer client);
      begin match cfg.on_recv_timeout with
      | `Reply_bad_request -> respond_empty ~close:true reqd `Bad_request
      | `Drop_connection -> drop_connection reqd
      end;
      Lwt.return_unit
    | Ok body ->
      match make_request_exn ~line1 ~headers ~body client with
      | exception exn ->
        incr_errors server;
        let (http_error,msg) = make_error exn in
        log #warn "error for %s : %s" (show_peer client) msg;
        respond_empty ~close:cfg.single reqd http_error;
        Lwt.return_unit
      | req ->
        client.req <- Ready req;
        Hashtbl.replace server.reqs req.id req;
        let%lwt reply = (handle_request_lwt client req answer)[%lwt.finally Hashtbl.remove server.reqs req.id; Lwt.return_unit] in
        match reply with
        | `No_reply -> drop_connection reqd; Lwt.return_unit
        | `Body _ | `Chunks _ as reply -> send_reply client reqd req reply
  in
  Lwt.async begin fun () ->
    begin
      try%lwt process () with exn ->
        log #warn ~exn "request %s" (show_peer client);
        Ache.Reqd.report_exn reqd exn;
        Lwt.return_unit
    end [%lwt.finally
      Hashtbl.remove server.clients req_id;
      if cfg.debug then log #info "finished #%d %s" req_id (show_peer client);
      Lwt.return_unit
    ]
  end

let ache_error_handler server sockaddr ?request:_ error start_response =
  match error, server.config.on_recv_timeout with
  | `Exn Drop_connection, _ -> raise Drop_connection
  | `Request_timeout, `Drop_connection ->
    incr_errors server;
    log #info "dropped %s : head timeout" (Nix.show_addr sockaddr);
    raise Drop_connection
  | _ ->
    incr_errors server;
    let reason = match error with `Exn exn -> Exn.str exn | #Ache.Status.standard as s -> Ache.Status.to_string s in
    log #warn "error for %s : %s" (Nix.show_addr sockaddr) reason;
    Ache.Body.Writer.close (start_response Ache.Headers.empty)

let accept_lwt config fd k =
  match%lwt Exn_lwt.map (Lwt_unix.accept ~cloexec:true) fd with
  | `Exn (Unix.Unix_error (Unix.EMFILE,_,_)) ->
    let pause = 2. in
    log #error "too many open files, disabling accept for %s" (Time.duration_str pause);
    Lwt_unix.sleep pause
  | `Exn (Lwt.Canceled as exn) -> log #info "canceling accept loop"; Lwt.reraise exn
  | `Exn exn -> log #warn ~exn "accept"; Lwt.return_unit
  | `Ok (fd,addr) ->
    let task =
      try%lwt k (fd,addr)
      with exn -> log #warn ~exn "accepted (%s)" (Nix.show_addr addr); Lwt.return_unit
    in
    if config.single then
      task
    else
    begin
      Lwt.async (fun () -> task); (* "fork" processing *)
      Lwt.return_unit
    end

let accept_hook = ref (fun () -> ())

let handle_lwt config fd k =
  let rec loop () =
    !accept_hook ();
    let%lwt () = accept_lwt config fd k in
    let%lwt () = if config.yield then Lwt.pause () else Lwt.return_unit in
    loop ()
  in
  let%lwt () =
    match config.exit_thread with
    | Some exit -> Lwt.pick [ exit; loop (); ]
    | None -> loop ()
  in
  log #info "%s %s exit" config.name (Nix.show_addr config.connection);
  Lwt.return_unit

let setup_fd_lwt fd config answer =
  let server = make_server_state (Lwt_unix.unix_file_descr fd (* will not be used *) ) config in
  let ache_config = { Ache.Config.default with max_header_size = config.max_request_size } in
  let timeout = if config.max_time.headers > 0. then Some config.max_time.headers else None in
  let body_deadlines = new_body_deadlines config.max_time.body in
  Lwt.ignore_result begin
    while%lwt true do
      let%lwt () = Lwt_unix.sleep @@ Time.minutes 1 in
      Lwt.wrap1 check_hung_requests server
    done
  end;
  handle_lwt config fd begin fun (fd,sockaddr) ->
    match server.active >= config.max_clients with
    | true ->
      incr_reject server;
      if config.debug then log #info "rejected %s" (Nix.show_addr sockaddr);
      Exn.suppress (Lwt_unix.shutdown fd) Unix.SHUTDOWN_ALL;
      Lwt_unix.close fd
    | false ->
      if config.debug then log #info "accepted %s" (Nix.show_addr sockaddr);
      incr_active server;
      let deadline = new_body_deadline body_deadlines in
      begin
        if config.nodelay then Lwt_unix.setsockopt fd TCP_NODELAY true;
        Ache_lwt.Server.create_connection_handler ~config:ache_config ?idle_timeout:timeout ?head_timeout:timeout
          ~request_handler:(fun sockaddr reqd -> handle_request_ache server fd deadline sockaddr answer reqd)
          ~error_handler:(ache_error_handler server) sockaddr fd
      end [%lwt.finally
        stop_body_deadline deadline;
        decr_active server;
        match Lwt_unix.state fd with
        | Lwt_unix.Closed -> Lwt.return_unit
        | Opened | Aborted _ -> Exn.suppress (Lwt_unix.shutdown fd) Unix.SHUTDOWN_ALL; Lwt_unix.close fd
      ]
  end

let setup_lwt config answer =
  let fd = Lwt_unix.of_unix_file_descr ~blocking:false @@ start_listen config in
  (setup_fd_lwt fd config answer)[%lwt.finally Lwt_unix.close fd]

let server_lwt config answer =
  Lwt_main.run @@ setup_lwt config answer

module Answer = struct

let return ?(status=`Ok) ?(extra=[]) ~typ data : [> `Body of reply_status reply' ] Lwt.t =
  Lwt.return @@ `Body (status, ("Content-Type",typ) :: extra, data)

let text = return ~typ:"text/plain"
let binary = return ~typ:"application/octet-stream"
let printf ?status ?extra fmt = ksprintf (fun s -> text ?status ?extra s) fmt
let json = return ~typ:"application/json"
let yojson ?status ?extra x = json ?status ?extra (Yojson.Safe.to_string x)

(** Return a body that's already gzip-encoded, add corresponding content-encoding *)
let encoded_gzip req ?status ?(extra=[]) ~typ data =
  match req.encoding with
  | Gzip -> return ?status ~extra:(("Content-Encoding", "gzip") :: extra) ~typ data
  | Identity -> Exn.fail "client does not accept gzip encoding"

let error status s = text ~status s
let not_found = error `Not_found
let bad_request = error `Bad_request
let internal_error = error `Internal_server_error

exception No_path

let random_ref () = Random.int64 Int64.max_int

let rest ~show_exn req answer =
  try%lwt
    let module Arg = Args(struct let req = req end) in
    answer (module Arg : Args) (req.meth,req.path)
  with
  | No_path -> bad_request @@ sprintf "unrecognized path %s %s" (show_method req.meth) req.url
  | Arg.Bad s -> bad_request @@ sprintf "bad parameter %s in %s" s req.url
  | exn ->
    let ref = random_ref () in
    log#warn ~exn "failed ref:%Ld %s" ref (show_request req) ~structured_pairs:(pairs_of_request req);
    if show_exn then
      internal_error @@ sprintf "internal error ref:%Ld : %s" ref (match exn with Failure s -> s | _ -> Exn.str exn)
    else
      internal_error @@ sprintf "undisclosed internal error ref:%Ld" ref

end
