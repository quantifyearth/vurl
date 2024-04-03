## Vurl

Vurls are an attempt to add versioning to URI resolution. For example, what should happen when we request `https://doi.org/10.1109/SASOW.2012.14`?

<!-- $MDX non-deterministic=command -->
```ocaml
# Eio_main.run @@ fun env ->
  Vurl_eio.with_default ~net:env#net env#cwd @@ fun () ->
  let vurl = Vurl.of_uri "https://doi.org/10.1109/SASOW.2012.14" in
  let vurl, file = Vurl.file vurl in
  Vurl.pp Format.std_formatter vurl;;

{
  "intentional_uri": "https://doi.org/10.1109/SASOW.2012.14",
  "segments": [
    {
      "uri": "file:./_data/document-6498375",
      "cid": "bag5qgera4k6fjhqzfv5ey4kgwmymrntwup4pysm23khvgmadwkgv5cwrtq3q"
    },
    {
      "uri": "http://ieeexplore.ieee.org/document/6498375/",
      "cid": "bag5qgerap5iaobunfnlovfzv4jeq2ygp6ltszlrreaskyh3mseky5osh2boq"
    },
    {
      "uri": "http://ieeexplore.ieee.org/document/6498375/",
      "cid": "bag5qgeratl2hddmmes3ooewxrpyquhutmd2jinw5n3lny6x6sbth6esmrg7q"
    }
  ]
}
- : unit = ()
```
