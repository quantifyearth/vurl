open Capnp_rpc_lwt

type t = { intentional_uri : Uri.t; segments : segment list }
and segment = { uri : Uri.t; cid : Cid.t }

let to_string t =
  let i = Uri.to_string t.intentional_uri in
  match t.segments with
  | [] -> i
  | segments ->
      i
      ^ List.fold_left
          (fun acc seg ->
            let u = Uri.to_string seg.uri in
            let c = Cid.to_string seg.cid in
            acc ^ ";" ^ u ^ "!" ^ c)
          "" (List.rev segments)

let of_string_exn s =
  Logs.info (fun f -> f "Got %s" s);
  match String.split_on_char ';' s with
  | [] -> failwith "Not a valid VURL"
  | iuri :: rest ->
      let rec loop_rest acc = function
        | [] -> List.rev acc
        | r :: rs -> (
            match String.split_on_char '!' r with
            | [ u; c ] ->
                let uri = Uri.of_string u in
                let cid =
                  Cid.of_string c |> function
                  | Ok v -> v
                  | Error (`Msg m) -> failwith m
                  | Error (`Unsupported _) -> failwith "CID Unsupported"
                in
                loop_rest ({ uri; cid } :: acc) rs
            | [ "" ] -> acc
            | s -> failwith ("Not a valid Vurl: " ^ String.concat "!" s))
      in
      let intentional_uri = Uri.of_string iuri in
      let segments = loop_rest [] rest |> List.rev in
      { intentional_uri; segments }

let intentional_uri t = Some t.intentional_uri

let decapsulate t =
  match t.segments with
  | [] -> `URI t.intentional_uri
  | s :: segments -> `Segment (s, { t with segments })

let next_uri t =
  match decapsulate t with `Segment (s, _) -> s.uri | `URI uri -> uri

let encapsulate t cid uri = { t with segments = { cid; uri } :: t.segments }
let pp ppf f = Format.fprintf ppf "%s" (to_string f)
let of_uri uri = { intentional_uri = Uri.of_string uri; segments = [] }
let resolvers : Rpc.Client.Resolver.t Capability.t list ref = ref []
let add_resolver c = resolvers := c :: !resolvers

let fold_resolver f =
  match !resolvers with
  | [] -> failwith "No resolvers registered"
  | res ->
      let loop = function [] -> failwith "TODO" | r :: _ -> f r in
      loop res

let file vurl =
  let open Rpc.Client.Resolver.Resolve in
  let vurl = to_string vurl in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.vurl_set params vurl;
  Params.resource_set params Rpc.Resource_16038180360818139020.File;
  let fn t =
    Capability.call_for_value_exn t method_id request
    |> Results.vurl_get |> of_string_exn
  in
  let file_vurl = fold_resolver fn in
  Logs.debug (fun f -> f "Vurl recv: %a" pp file_vurl);
  let file_path = (List.hd file_vurl.segments).uri |> Uri.path in
  (file_vurl, Resource.File.{ path = file_path })

let ptr _ = failwith "TODO"
let git _ = failwith "TODO"

let cid ?(codec = `Https) buf =
  let hash = Multihash_digestif.of_cstruct `Sha2_256 buf |> Result.get_ok in
  Cid.v ~version:`Cidv1 ~base:`Base32 ~codec ~hash
