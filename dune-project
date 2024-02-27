(lang dune 3.8)
(generate_opam_files true)
(using mdx 0.4)

(name vurl)

(package
  (name vurl)
  (depends
    uri
    (mdx :with-test)))


(package
  (name vurl-eio)
  (depends
   vurl
   eio
   cohttp-eio
   git-unix
   http-cookie
   tls-eio
   capnp-rpc-lwt
   capnp-rpc-unix
   yojson
   (eio_main :with-test)))