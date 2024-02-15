let cid = "zb2rhe5P4gXftAwvA4eXQ5HJwsER2owDyS9sKaQRRVQPn93bA"

let segment =
  Alcotest.of_pp (fun ppf (p : Vurl.segment) ->
      Fmt.pf ppf "uri: %a, cid: %a" Uri.pp p.uri Cid.pp_human p.cid)

let vurl = Alcotest.of_pp Vurl.pp

let vurls () =
  let uri = "https://example.org" in
  let uri2 = "file:///tmp/my/file.txt" in
  let ov = Vurl.of_string_exn uri in
  let v' = Vurl.intentional_uri ov |> Option.get |> Uri.to_string in
  Alcotest.(check string) "same uri" uri v';
  let uri_with_file = "https://example.org;" ^ uri2 ^ "!" ^ cid in
  let v = Vurl.of_string_exn uri_with_file in
  let v' = Vurl.intentional_uri v |> Option.get |> Uri.to_string in
  Alcotest.(check string) "same uri" uri v';
  let f = Vurl.decapsulate v in
  let expect =
    { Vurl.uri = Uri.of_string uri2; cid = Cid.of_string cid |> Result.get_ok }
  in
  Alcotest.(check (option (pair segment vurl)))
    "same vurl"
    (Some (expect, ov))
    f

let () =
  Alcotest.run "vurl" [ ("vurls", [ Alcotest.test_case "vurls" `Quick vurls ]) ]
