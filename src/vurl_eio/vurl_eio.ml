open Eio
open Capnp_rpc_lwt

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

  (* let equal a b =
     String.equal (Path.native_exn a.directory) (Path.native_exn b.directory) *)

  (* let pp ppf t = Path.pp ppf t.directory *)
end

let of_file (fs : _ Path.t) (vurl : Vurl.Resource.File.t) =
  let path = Path.(fs / vurl.path) in
  let parent = Path.(path / "..") in
  File.{ directory = (parent :> Fs.dir_ty Path.t); name = (fun _ -> "default") }

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

let cid_of_file path = Path.load path |> Cid.of_string |> Result.get_ok

let file_resolver ?(name = name) ?progress (net : _ Net.t) (dir : _ Path.t) :
    Vurl.Resource.File.t Vurl.Resolver.handler =
 fun req ->
  let http =
    Cohttp_eio.Client.make ~https:(Some (https ~authenticator:null_auth)) net
  in
  let directory = File.v dir name in
  let uri = req.vurl |> Vurl.intentional_uri |> Option.get in
  let filename = name uri in
  let _resolve = File.resolve ?progress http directory uri in
  let cid = cid_of_file Path.(dir / filename) in
  let vurl =
    Vurl.encapsulate req.vurl cid
      (Uri.make ~scheme:"file" ~path:Path.(native_exn (dir / filename)) ())
  in
  Logs.info (fun f -> f "Vurl: %a" Vurl.pp vurl);
  (vurl, Vurl.Resource.File)

let git_resolver _next_handler _req = failwith "TODO1"

let resolve_impl handler =
  let module X = Vurl.Rpc.Service.Resolver in
  X.local
  @@ object
       inherit X.service

       method resolve_impl params release_param_caps =
         let open X.Resolve in
         let vurl = Params.vurl_get params |> Vurl.of_string_exn in
         let resource = Params.resource_get params in
         release_param_caps ();
         let req = Vurl.Resolver.{ vurl; resource } in
         match resource with
         | Vurl.Rpc.Resource_16038180360818139020.File ->
             let v, _ = handler Vurl.Resolver.{ vurl; resource = File } in
             let response, results =
               Service.Response.create Results.init_pointer
             in
             Results.vurl_set results (Vurl.to_string v);
             Service.return response
         | Git ->
             let v, _ = handler Vurl.Resolver.{ vurl; resource = Git } in
             let response, results =
               Service.Response.create Results.init_pointer
             in
             Results.vurl_set results (Vurl.to_string v);
             Service.return response
         | _ -> failwith "TODO"
     end

let run ~secret_key ~sw ~listen_address ~net handler =
  let config = Capnp_rpc_unix.Vat_config.create ~secret_key listen_address in
  let service_id = Capnp_rpc_unix.Vat_config.derived_id config "main" in
  let uri = Capnp_rpc_unix.Vat_config.sturdy_uri config service_id in
  Logs.info (fun f -> f "URI: %a" Uri.pp uri);
  let service = resolve_impl handler in
  Switch.on_release sw (fun () -> Capability.dec_ref service);
  let restore = Capnp_rpc_net.Restorer.single service_id service in
  let vat = Capnp_rpc_unix.serve ~sw ~net ~restore config in
  Capnp_rpc_unix.Vat.sturdy_uri vat service_id

let connect_exn ~sw net url =
  let vat = Capnp_rpc_unix.client_only_vat ~sw net in
  match Capnp_rpc_unix.Vat.import vat url with
  | Ok sr -> Capnp_rpc_lwt.Sturdy_ref.connect_exn sr
  | Error (`Msg m) -> failwith m
