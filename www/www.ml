open Lwt.Syntax

let path = Sys.argv.(1)
let source_file = path ^ "/source.txt"
let ancient_file = path ^ "/ancient.db"
let db = Db.db_open_in ~source:source_file ~db:ancient_file
let () = Link.load "static/urls.tsv"

let api ~cursor query =
  let+ results, stop = Search.api ~cursor query in
  Present.present ~query ~start:cursor ~stop results

let api ~cursor query =
  if query = "" then Lwt.return Ui.empty_propaganda else api ~cursor query

let get_query params = Option.value ~default:"" (Dream.query "q" params)

let get_cursor params =
  match Dream.query "ofs" params with
  | None -> Db.cursor_empty ~db
  | Some shard_offset -> Db.cursor_of_string ~db shard_offset

let root fn params =
  let query = get_query params in
  let cursor = get_cursor params in
  let* result = fn ~cursor query in
  Dream.html result

let string_of_tyxml html = Format.asprintf "%a" (Tyxml.Html.pp ()) html
let string_of_tyxml' html = Format.asprintf "%a" (Tyxml.Html.pp_elt ()) html

let cache : string -> Dream.middleware =
 fun max_age f req ->
  let+ response = f req in
  Dream.add_header "Cache-Control" ("public, max-age=" ^ max_age) response

let main ~max_age =
  Dream.run ~interface:"127.0.0.1" ~port:8888
  @@ Dream.logger
  @@ cache max_age
  @@ Dream.router
       [ Dream.get
           "/"
           (root (fun ~cursor q ->
                let+ result = api ~cursor q in
                string_of_tyxml @@ Ui.template q result))
       ; Dream.get
           "/api"
           (root (fun ~cursor q ->
                let+ result = api ~cursor q in
                string_of_tyxml' result))
       ; Dream.get "/s.css" (Dream.from_filesystem "static" "style.css")
       ; Dream.get "/robots.txt" (Dream.from_filesystem "static" "robots.txt")
       ; Dream.get "/favicon.ico" (Dream.from_filesystem "static" "favicon.svg")
       ]
  @@ Dream.not_found

let () =
  let max_age = Sys.argv.(2) in
  main ~max_age
