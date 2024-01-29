open Eio

module File = struct
  type t = { directory : Fs.dir_ty Path.t; name : Uri.t -> string }

  let v directory name = { directory :> Fs.dir_ty Path.t; name }
  let directory t = t.directory

  let take_all_and_report report t =
    try
      while true do
        let old = Buf_read.buffered_bytes t in
        Buf_read.ensure t (Buf_read.buffered_bytes t + 1);
        report (Buf_read.buffered_bytes t - old)
      done;
      assert false
    with End_of_file ->
      let data = Cstruct.to_string (Buf_read.peek t) in
      Buf_read.consume t (Buf_read.buffered_bytes t);
      data

  let with_progress_bar length = function
    | None -> Progress.with_reporter (Progress.Line.noop ())
    | Some p ->
        let progress_bar = p length in
        Progress.with_reporter progress_bar

  let resolve ?progress http t uri =
    let f = Cohttp_eio.Client.get http uri in
    Switch.run @@ fun sw ->
    let response, body = f ~sw in
    let length = Http.Response.content_length response in
    with_progress_bar (Option.map (fun v -> (t.name uri, v)) length) progress
    @@ fun progress ->
    let file = Path.(t.directory / t.name uri) in
    Path.with_open_out ~create:(`If_missing 0o644) file @@ fun oc ->
    Buf_write.with_flow oc @@ fun w ->
    let buf_r = Buf_read.of_flow ~max_size:max_int body in
    progress (Buf_read.buffered_bytes buf_r);
    let buf = take_all_and_report progress buf_r in
    Buf_write.string w buf;
    t

  let equal a b =
    String.equal (Path.native_exn a.directory) (Path.native_exn b.directory)

  let pp ppf t = Path.pp ppf t.directory
end

type _ Vurl.Resolver.t += File : File.t -> File.t Vurl.Resolver.t

let equal_file (type a b) :
    a Vurl.Resolver.t ->
    b Vurl.Resolver.t ->
    (a Vurl.Resolver.t, b Vurl.Resolver.t) Type.eq option =
 fun a b ->
  match (a, b) with
  | File f, File f' -> if File.equal f f' then Some Type.Equal else None
  | _ -> None

let null_auth ?ip:_ ~host:_ _ =
  Ok None (* Warning: use a real authenticator in your code! *)

let https ~authenticator =
  let tls_config = Tls.Config.client ~authenticator () in
  fun uri raw ->
    let host =
      Uri.host uri
      |> Option.map (fun x -> Domain_name.(host_exn (of_string_exn x)))
    in
    Tls_eio.client_of_flow ?host tls_config raw

(* TODO: we can do better *)
let name uri =
  let params = Uri.path_and_query uri in
  String.split_on_char '/' params |> String.concat "-"

let file_resolver ?(name = name) ?progress (net : _ Net.t) (dir : _ Path.t) =
  let http =
    Cohttp_eio.Client.make ~https:(Some (https ~authenticator:null_auth)) net
  in
  let directory = File.v dir name in
  let resolver = File directory in
  let resolve = File.resolve ?progress http directory in
  Vurl.Resolver.register resolver ~resolve
    ~equal:{ Vurl.Resolver.equal = equal_file }
    ~pp:File.pp;
  resolver
