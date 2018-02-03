open Lwt.Infix

let executable_query (query, kvariables, parse) =
  fun ~token -> (
    kvariables (fun variables ->
        let uri = Uri.of_string "https://api.github.com/graphql" in
        let headers = Cohttp.Header.of_list [
          "Authorization", "bearer " ^ token;
          "User-Agent", "andreas/ppx_graphql";
        ] in
        let body = `Assoc [
          "query", `String query;
          "variables", variables;
        ] in
        let serialized_body = Yojson.Basic.to_string body in
        Cohttp_lwt_unix.Client.post ~headers ~body:(`String serialized_body) uri >>= fun (rsp, body) ->
        Cohttp_lwt.Body.to_string body >|= fun body' ->
        match Cohttp.Code.(code_of_status rsp.status |> is_success) with
        | false ->
            Error body'
        | true ->
            try
              Ok (Yojson.Basic.from_string body' |> parse)
            with Yojson.Json_error err ->
              Error err
    )
  )

let find_repository = executable_query [%graphql {|
  query FindRepository($owner: String!, $name: String!) {
    repository(owner: $owner, name: $name) {
      id
      description
    }
  }
|}]

let search_repositories = executable_query [%graphql {|
  query SearchRepositories($query: String!) {
    search(query: $query, type: REPOSITORY, first: 5) {
      nodes {
        __typename
        ...on Repository {
          nameWithOwner
        }
      }
    }
  }
|}]

let main () =
  let token = Unix.getenv "GITHUB_TOKEN" in
  search_repositories ~token ~query:"topic:mirageos" () >|= function
  | Ok rsp ->
    begin match rsp#search#nodes with
    | Some nodes ->
      List.iter (function
        | Some (`Repository r) -> Format.printf "Repo: %s\n" r#nameWithOwner
        | _ -> ()
      ) nodes
    | None ->
      Format.printf "Empty search result"
    end
  | Error err ->
      Format.printf "Error! %s" err

let () =
  Lwt_main.run (main ())
