type 'target t = ..
(** A resolver that will resolve URIs to targets *)

type equality = { equal : 'a 'b. 'a t -> 'b t -> ('a t, 'b t) Type.eq option }
(** A higher-ranked polymorphic, type equality record. See {! register} for how
    to contruct one for your resolver. *)

val register :
  'a t ->
  resolve:(Uri.t -> 'a) ->
  equal:equality ->
  pp:(Format.formatter -> 'a -> unit) ->
  unit
(** [register r ~resolve ~equal ~pp] registers a new resolver for [r] that uses
    [resolve] to resolve a URI.*)

val resolve : 'target t -> Uri.t -> 'target
(** [resolve resolver uri] resolves a [uri] to a target which is resolver
    specifc. *)
