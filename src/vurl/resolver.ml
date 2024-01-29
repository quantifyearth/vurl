type _ t = ..

module type S = sig
  type t

  val resolve : Uri.t -> t
  val pp : Format.formatter -> t -> unit
end

type equality = { equal : 'a 'b. 'a t -> 'b t -> ('a t, 'b t) Type.eq option }

module Register = struct
  type existential_resolver =
    | Resolver : 'a t * (module S with type t = 'a) -> existential_resolver

  let resolvers : existential_resolver list ref = ref []
  let equal : equality ref = ref { equal = (fun _ _ -> None) }

  let register_resolver (type a) :
      a t -> equality -> (module S with type t = a) -> unit =
   fun (resolver : a t) f (module R : S with type t = a) ->
    resolvers :=
      Resolver (resolver, (module R : S with type t = a)) :: !resolvers;
    let old_equal = !equal in
    equal :=
      {
        equal =
          (fun a b ->
            match f.equal a b with
            | Some refl -> Some refl
            | None -> old_equal.equal a b);
      }
end

let resolve (type a) : a t -> Uri.t -> a =
 fun r uri ->
  let eq = !Register.equal in
  let rec loop : _ list -> a = function
    | Register.Resolver (r', resolver) :: rest -> (
        match eq.equal r r' with
        | Some Type.Equal ->
            let (module R) = resolver in
            R.resolve uri
        | None -> loop rest)
    | [] -> failwith "No resolver could not be found"
  in
  loop !Register.resolvers

let register :
    type a b c.
    a t ->
    resolve:(Uri.t -> a) ->
    equal:equality ->
    pp:(Format.formatter -> a -> unit) ->
    unit =
 fun r ~resolve ~equal ~pp ->
  let module R : S with type t = a = struct
    type t = a

    let resolve = resolve
    let pp = pp
  end in
  Register.register_resolver r equal (module R)
