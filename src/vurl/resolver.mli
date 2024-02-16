type request = { vurl : Vurl_intf.t; resource : Resource.t }
and response = Vurl_intf.t * Resource.t
and handler = request -> response
and middleware = handler -> handler

val logger : middleware
(** A simple logger that logs all incoming requests to the resolver *)

val not_found : handler
(** The [not_found] handler, typically used to close off a collection of
    handlers and middleware. *)

type route

val file : handler -> route
val git : handler -> route
val routes : route list -> handler
