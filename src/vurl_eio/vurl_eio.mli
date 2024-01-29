module File : sig
  type t
  (** A resolver that targets files. *)

  val directory : t -> Eio.Fs.dir_ty Eio.Path.t
  (** The directory where all the files are stored. *)
end

val file_resolver :
  ?name:(Uri.t -> string) ->
  ?progress:((string * int) option -> int Progress.Line.t) ->
  _ Eio.Net.t ->
  _ Eio.Path.t ->
  File.t Vurl.Resolver.t
(** A new file resolver for a particular directory. [name] is used to convert a
    URI to a file name and [progress] can be used to optionally report progress
    made by the download. *)
