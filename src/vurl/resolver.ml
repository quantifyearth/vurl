let src = Logs.Src.create "vurl.resolver" ~doc:"Vurl resolver"

module Log = (val Logs.src_log src : Logs.LOG)

type 'a request = { vurl : Vurl_intf.t; resource : 'a Resource.t }
and 'a response = Vurl_intf.t * 'a Resource.t
and 'a handler = 'a request -> 'a response
and 'a middleware = 'a handler -> 'a handler

let logger : _ middleware =
 fun next_handler request ->
  Log.info (fun f -> f ~header:"vurl" "%a" Vurl_intf.pp request.vurl);
  next_handler request

let not_found req = (req.vurl, Resource.Error)

module Handlers = struct
  type t = [] : t | ( :: ) : ('a Resource.t * 'a handler) * t -> t

  let run_middlewares (type a) : t -> a handler -> a request -> a response =
   fun ms next_handler req ->
    let rec loop : t -> a response = function
      | [] -> next_handler req
      | (kind, f) :: fs -> (
          match Resource.equal req.resource kind with
          | Some Type.Equal -> f req
          | None -> loop fs)
    in
    loop ms

  let handlers middlewares : _ middleware =
   fun next_handler request ->
    let ms = run_middlewares middlewares next_handler request in
    ms
end
