let () =
  Logs.set_level (Some Logs.Info);
  Logs.set_reporter (Logs_fmt.reporter ())

let yaml_git = Vurl.of_uri "https://github.com/patricoferris/ppx_deriving_yaml"

let git_contents path =
  let open Lwt.Syntax in
  Logs.info (fun f -> f "Git store at %s" path);
  (* TODO: Fix this path problem! *)
  let path = Fpath.v ("./" ^ path) in
  let* store = Git_unix.Store.v path in
  match store with
  | Error e -> Fmt.failwith "%a" Git_unix.Store.pp_error e
  | Ok store -> (
      let+ main = Git_unix.Store.Ref.resolve store Git.Reference.main in
      match main with
      | Error e -> Fmt.failwith "%a" Git_unix.Store.pp_error e
      | Ok d -> Logs.info (fun f -> f "main: %a" Digestif.SHA1.pp d))

let () =
  Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun _ ->
  let vurl, Vurl.Resource.Git.{ path } =
    Vurl_eio.with_default ~net:(Eio.Stdenv.net env) (Eio.Stdenv.cwd env)
    @@ fun () -> Vurl.git yaml_git
  in
  Logs.info (fun f -> f "vurl: %a" Vurl.pp vurl);
  Lwt_eio.Promise.await_lwt (git_contents path)
