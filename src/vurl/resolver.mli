type 'a request = { vurl : Vurl_intf.t; resource : 'a Resource.t }
and 'a response = Vurl_intf.t * 'a Resource.t
and 'a handler = 'a request -> 'a response
and 'a middleware = 'a handler -> 'a handler

val logger : _ middleware
(** A simple logger that logs all incoming requests to the resolver *)

val not_found : Resource.Error.t handler
(** The [not_found] handler, typically used to close off a collection of
    handlers and middleware. *)

module Handlers : sig
  type t = [] : t | ( :: ) : ('a Resource.t * 'a handler) * t -> t

  val handlers : t -> _ middleware
end
