open Eio
open Vurl_eio

let () =
  Logs.set_level (Some Logs.Info);
  Logs.set_reporter (Logs_fmt.reporter ())

let srtm =
  Vurl.of_uri
    "https://srtm.csi.cgiar.org/wp-content/uploads/files/srtm_5x5/TIFF/srtm_19_03.zip"

let resolve fs vurl =
  let vurl, file = Vurl.file vurl in
  let () =
    match Vurl.decapsulate vurl with
    | `Segment (seg, _) ->
        Logs.info (fun f ->
            f "URI: %a, CID: %a" Uri.pp seg.uri Cid.pp_human seg.cid)
    | `URI _ -> ()
  in
  let dir = Vurl_eio.of_file fs file |> File.directory in
  Eio.Path.read_dir dir

let () =
  Eio_main.run @@ fun env ->
  let fs = Stdenv.fs env in
  Vurl_eio.with_cap ~net:(Stdenv.net env) Path.(fs / "example.cap") @@ fun () ->
  let files = resolve fs srtm in
  Logs.info (fun f -> f "Downloaded %a" Fmt.(list string) files)
