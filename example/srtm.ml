open Eio

let srtm =
  Vurl.v
    "https://srtm.csi.cgiar.org/wp-content/uploads/files/srtm_5x5/TIFF/srtm_19_03.zip"

let resolve net directory vurl =
  let progress = function
    | None -> Progress.Line.noop ()
    | Some (name, total) ->
        let open Progress.Line in
        list [ const name; spinner (); bar total; percentage_of total ]
  in
  let name uri =
    Uri.path uri |> String.split_on_char '/' |> List.rev |> List.hd
  in
  let resolver = Vurl_eio.file_resolver ~name ~progress net directory in
  let file = Vurl.Resolver.resolve resolver vurl in
  let dir = Vurl_eio.File.directory file in
  Eio.Path.read_dir dir

let () =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  let net = Stdenv.net env in
  let fs = Stdenv.fs env in
  let temp_dir =
    match Sys.argv.(1) with
    | dir -> Eio.Path.(fs / dir)
    | exception Invalid_argument _ -> Eio.Path.(fs / Filename.temp_dir "" "")
  in
  let files = resolve net temp_dir srtm in
  Fmt.pr "Downloaded %a to %a" Fmt.(list string) files Path.pp temp_dir
