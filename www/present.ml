open Lwt.Syntax
open Tyxml.Html
open Search

let group_results_by_filename results =
  let rec go current_filename acc = function
    | [] -> [ List.rev acc ]
    | r :: rs when r.filename = current_filename -> go current_filename (r :: acc) rs
    | r :: rs -> List.rev acc :: go1 r rs
  and go1 r rs = go r.filename [ r ] rs in
  match results with
  | [] -> []
  | r :: rs -> go1 r rs

let indentation str =
  let rec go i = if i < String.length str && str.[i] = ' ' then go (i + 1) else i in
  go 0

let find_context_line ~previous m =
  let line_indent = indentation m.line in
  if line_indent = 0
  then []
  else (
    let cursor = m.cursor in
    let rec go nb_found line_num line_indent =
      if nb_found >= 3 || line_indent = 0 || line_num <= previous
      then []
      else (
        let line = Db.read_line ~cursor line_num in
        let new_indent = indentation line in
        if new_indent < String.length line && new_indent < line_indent
        then (line_num, line) :: go (nb_found + 1) (line_num - 1) new_indent
        else go nb_found (line_num - 1) line_indent)
    in
    go 0 (m.offset - 1) line_indent)

let present_match ~previous m =
  let mark h = mark [ a ~a:[ a_href (Link.url m.filename (m.line_num + 1)) ] h ] in
  let hlmatch = Colorize.to_html_highlight ~mark m.line m.range in
  let contexts_list = find_context_line ~previous m in
  let contexts = List.map (fun (ln, str) -> ln, Colorize.to_html str) contexts_list in
  let results = (m.offset, hlmatch) :: contexts in
  m.offset, results

let present_line_numbers ~filename matches =
  let rec fold prev_line_num = function
    | [] -> []
    | (ln, hlmatch) :: rest ->
      let cont_above = prev_line_num + 1 = ln in
      let cont_below =
        match rest with
        | (ln', _) :: _ -> ln + 1 = ln'
        | _ -> false
      in
      Ui.result_html ~cont_above ~cont_below filename ln hlmatch :: fold ln rest
  in
  fold (-2) matches

let present_file ~db ~filename ~start_of_file matches =
  let _, matches =
    List.fold_left
      (fun (previous, acc) m ->
        let ofs, lines = present_match ~previous m in
        ofs, List.rev_append lines acc)
      (start_of_file, [])
      (List.rev matches)
  in
  let matches = List.map (fun (ln, str) -> ln - start_of_file, str) matches in
  let header = Ui.link_file filename in
  header :: present_line_numbers ~filename matches

let present_file ~db matches =
  match matches with
  | [] -> assert false
  | hd :: _ ->
    let _, start_of_file = Db.fileloc_of_line ~cursor:hd.cursor hd.offset in
    present_file ~db ~filename:hd.filename ~start_of_file matches

let present_results ~db results =
  let groups = group_results_by_filename results in
  List.concat @@ List.map (present_file ~db) groups

let estimated_count ~start ~stop count =
  let total = Db.total_lines ~cursor:start in
  let upto_stop =
    match stop with
    | None -> total
    | Some stop -> Db.total_lines_upto ~cursor:stop
  in
  let seen = upto_stop in
  if seen < total
  then true, total * (start.count + count) / seen
  else false, start.count + count

let pretty_count (is_approx, count) =
  let m = 1_000_000 in
  let k = 1_000 in
  if count / m >= 1
  then Printf.sprintf "%.1fm results" (float count /. float m)
  else if count / k >= 1
  then Printf.sprintf "%.1fk results" (float count /. float k)
  else if is_approx
  then Printf.sprintf "~%i results" count
  else if count = 0
  then "no results"
  else if count = 1
  then "1 result"
  else Printf.sprintf "%i results" count

let btn_more ~cursor query =
  p
    [ a
        ~a:
          [ a_class [ "btn" ]
          ; a_href ("/?q=" ^ Uri.pct_encode query ^ "&ofs=" ^ Db.string_of_cursor cursor)
          ]
        [ txt "More »" ]
    ]

let present ~query ~start ~stop results =
  let nb_results = List.length results in
  let results = present_results ~db:start.Db.db results in
  let more_link =
    match stop with
    | None -> []
    | Some cursor ->
      let cursor = { cursor with Db.count = start.count + nb_results } in
      [ btn_more ~cursor query ]
  in
  let count = estimated_count ~start ~stop nb_results in
  let title =
    [ txt (Printf.sprintf "%s for " (pretty_count count)); code [ txt query ] ]
  in
  div ((h2 title :: results) @ more_link)
