let () =
  Logs.set_level (Some Logs.Info);
  Logs.set_reporter (Logs_fmt.reporter ())

let example_org_html = Vurl.of_uri "https://example.org/index.html"

let () =
  let vurl, _file =
    Eio_main.run @@ fun env ->
    Vurl_eio.with_default ~net:(Eio.Stdenv.net env) (Eio.Stdenv.cwd env)
    @@ fun () -> Vurl.file example_org_html
  in
  Fmt.pr "vurl: %a" Vurl.pp vurl
