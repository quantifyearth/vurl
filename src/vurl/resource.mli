module File : sig
  type t = { path : string }
end

module Git : sig
  type t
end

module Ptr : sig
  type t =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
end

module Error : sig
  type t = { description : string }
end

type _ t =
  | File : File.t t
  | Git : Git.t t
  | Ptr : Ptr.t t
  | Unit : unit t
  | Error : Error.t t

val equal : 'a t -> 'b t -> ('a, 'b) Type.eq option
