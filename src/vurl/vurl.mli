type t = Vurl_intf.t
(** A versioned URL *)

val pp : Format.formatter -> t -> unit
(** A pretty printer for Vurls. *)

val to_string : t -> string
(** Serialises a Vurl to their canonical format *)

val of_string_exn : string -> t
(** Deserialises a Vurl.

    @raises Invalid_argument if malformatted. *)

val intentional_uri : t -> Uri.t option
(** The original URI that specified the intent for a Vurl *)

val cid : t -> Cid.t
(** The lowest CID of the Vurl. *)

type segment = { uri : Uri.t; cid : Cid.t }

val decapsulate : t -> (segment * t) option
(** Removes a layer of the Vurl. If only the intentional URI is left
    then returns [None]. *)

val encapsulate : t -> Cid.t -> Uri.t -> t
(** Adds another layer of precision to an existing Vurl. *)

val of_uri : string -> t
(** Makes a new Vurl from an intentional URI. *)

module Resource = Resource
module Resolver = Resolver
module Rpc = Rpc

val add_resolver : Rpc.t -> unit
val file : t -> Resource.File.t
val ptr : t -> Resource.Ptr.t
val git : t -> Resource.Git.t
