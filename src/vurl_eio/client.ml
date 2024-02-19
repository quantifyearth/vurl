open Eio
open Cohttp_eio
module Cookie = Http_cookie

let get http uri fn =
  let rec loop ?headers acc uri =
    Switch.run @@ fun sw ->
    let response, body = Client.get ~sw ?headers http uri in
    match response.status with
    | `Found ->
        let set_cookies =
          List.filter_map
            (function
              | "Set-Cookie", c -> Some (Cookie.of_set_cookie c |> Result.get_ok)
              | _ -> None)
            (Http.Header.to_list response.headers)
        in
        let cookies =
          List.map
            (fun c -> ("Cookie", Cookie.name c ^ "=" ^ Cookie.value c))
            set_cookies
        in
        let s = Buf_read.take_all (Buf_read.of_flow ~max_size:max_int body) in
        let new_uri =
          Http.Header.get response.headers "location"
          |> Option.get |> Uri.of_string
        in
        let new_uri =
          match Uri.host new_uri with
          | Some _ -> new_uri
          | None -> Uri.with_path uri (Uri.path new_uri)
        in
        let headers =
          match (cookies, headers) with
          | [], _ -> headers
          | cookies, None -> Some (Http.Header.of_list cookies)
          | cookies, Some h -> Some (Http.Header.add_list h cookies)
        in
        loop ?headers ((uri, Vurl.cid (Cstruct.of_string s)) :: acc) new_uri
    | `OK ->
        let length = Http.Response.content_length response in
        fn acc length response body
    | _ -> failwith ""
  in
  loop [] uri
