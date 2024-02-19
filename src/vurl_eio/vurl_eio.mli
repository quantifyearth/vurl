open Eio

module File : sig
  type t
  (** A resolver that targets files. *)

  val directory : t -> Eio.Fs.dir_ty Eio.Path.t
  (** The directory where all the files are stored. *)
end

(** {1 Vurl Interfaces} *)

val of_file : _ Path.t -> Vurl.Resource.File.t -> File.t

(** {1 Resolver Services} *)

val doi : _ Eio.Net.t -> Vurl.Resolver.middleware
(** Handles intermediate DOI resolution *)

val file_resolver :
  ?name:(Uri.t -> string) ->
  ?progress:((string * int) option -> int Progress.Line.t) ->
  _ Eio.Net.t ->
  _ Eio.Path.t ->
  Vurl.Resolver.handler
(** A new file resolver for a particular directory. [name] is used to convert a
    URI to a file name and [progress] can be used to optionally report progress
    made by the download. *)

val git_resolver : _ Eio.Path.t -> Vurl.Resolver.handler

val run :
  secret_key:[< `Ephemeral | `File of string | `PEM of string ] ->
  sw:Eio.Switch.t ->
  listen_address:Capnp_rpc_unix.Network.Location.t ->
  net:_ Eio.Net.t ->
  Vurl.Resolver.handler ->
  Uri.t
(** Run a resolver service *)

val connect_exn :
  sw:Switch.t -> _ Net.t -> Uri.t -> 'a Capnp_rpc_lwt.Capability.t
(** A client-only connection to a capability *)

val with_cap : net:_ Net.t -> _ Path.t -> (unit -> 'a) -> 'a
(** Simple helper to set up a capability to a resolver from a cap URI in a file.*)
