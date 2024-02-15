module File = struct
  type t = { path : string }
end

module Git = struct
  type t
end

module Ptr = struct
  type t =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
end

module Error = struct
  type t = { description : string }
end

type _ t =
  | File : File.t t
  | Git : Git.t t
  | Ptr : Ptr.t t
  | Unit : unit t
  | Error : Error.t t

let equal (type a b) : a t -> b t -> (a, b) Type.eq option =
 fun a b ->
  match (a, b) with
  | File, File -> Some Type.Equal
  | Git, Git -> Some Type.Equal
  | Ptr, Ptr -> Some Type.Equal
  | Unit, Unit -> Some Type.Equal
  | Error, Error -> Some Type.Equal
  | _ -> None
