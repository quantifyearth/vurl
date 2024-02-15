let src = Logs.Src.create "vurl.resolver" ~doc:"Vurl resolver"

module Log = (val Logs.src_log src : Logs.LOG)

type request = { vurl : Vurl_intf.t; resource : Resource.t }
and response = Vurl_intf.t * Resource.t
and handler = request -> response
and middleware = handler -> handler

let logger : middleware =
 fun next_handler request ->
  Log.info (fun f -> f ~header:"vurl" "%a" Vurl_intf.pp request.vurl);
  next_handler request

let not_found req = (req.vurl, Resource.Error)

type route = Resource.t * handler

let file handler = (Resource.File, handler)
let git handler = (Resource.Git, handler)

let routes (routes : route list) : handler =
 fun req ->
  let rec loop = function
    | [] -> not_found req
    | (r, h) :: rest -> if req.resource = r then h req else loop rest
  in
  loop routes
