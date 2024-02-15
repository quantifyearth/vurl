include Schema.MakeRPC (Capnp_rpc_lwt)

type t = Client.Resolver.t Capnp_rpc_lwt.Capability.t
