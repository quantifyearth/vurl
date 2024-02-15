open Eio
open Vurl_eio

let srtm =
  Vurl.of_uri
    "https://srtm.csi.cgiar.org/wp-content/uploads/files/srtm_5x5/TIFF/srtm_19_03.zip"

let resolve fs vurl =
  let file = Vurl.file vurl in
  let dir = Vurl_eio.of_file fs file |> File.directory in
  Eio.Path.read_dir dir

(* Could be abstracted into a library *)
let vurl_run fn =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let uri = Path.(load (env#fs / "example.cap")) |> Uri.of_string in
  let cap = Vurl_eio.connect_exn ~sw (Stdenv.net env) uri in
  Vurl.add_resolver cap;
  fn env

let () =
  vurl_run @@ fun env ->
  let fs = Stdenv.fs env in
  let files = resolve fs srtm in
  Fmt.pr "Downloaded %a" Fmt.(list string) files
