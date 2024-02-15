(* An example resolver that can resolve HTTP(S) to
   files and git respositories. *)

open Eio
open Vurl

let () =
  Logs.set_level (Some Logs.Info);
  Logs.set_reporter (Logs_fmt.reporter ())

let () =
  Eio_main.run @@ fun env ->
  let fs = Stdenv.fs env in
  let net = Stdenv.net env in
  let uri =
    Eio.Switch.run @@ fun sw ->
    Vurl_eio.run ~sw ~secret_key:`Ephemeral ~net
      ~listen_address:(`Unix "/tmp/resolver.eio")
    @@ Resolver.logger
    @@ Resolver.Handlers.handlers
         [
           (Resource.File, Vurl_eio.file_resolver net fs);
           (Resource.Git, Vurl_eio.git_resolver fs);
         ]
    @@ Resolver.not_found
  in
  Eio.traceln "Resolver running: %a" Uri.pp uri
