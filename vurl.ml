type t = uri

val cons : t -> uri -> t
val head : t -> uri

val make : string -> t (* toplevel uri *)

module Res = struct
  type t
  type chains = t list
  val v : ~name:string -> -> resolver
end

type rr =
  | Git
  | Https
  | Fd
  | Ptr

val recurse : uri -> Res.t -> t
val iterate : uri -> Res.t -> uri
val backtrack : uri -> uri option

val to_string : t -> string


(* example *)

let jrc = make "https://domain/publishedid" (* declaring the data source *)

(* mwd is building for first time *)
let resolver = 4c_resolver ~context:Refresh ()  (* declaring who we are, security creds *)
let file : File.t result = recurse ~resolver File jrc (* 4c cluster *)
let work = gdal_yirga file
let manifest = Res.manifest resolver

(* a few days later, tom overrides *)
let resolver2 = 4c_resolver ~context:Replay ~pins:["jrc..."] ()
let file : File.t result = recurse ~resolver File jrc (* same code ... *)
let work = gdal_yirga file

(* we promote it to CI *)
let ci_resolver = 4c_resolver ~context:Immutable ~pins:manifest ()
let file : File.t result = recurse ~resolver File jrc (* same code ... *)
let work = gdal_yirga file

(* the UI *)
let tooltip = backtrack file ~resolver

