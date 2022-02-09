open Lwt.Syntax
module Int_set = Db.Int_set

let max_to_find = 200

type q =
  | QInt_set of Int_set.t
  | QTrue
  | QFalse
  | QAnd of q list
  | QOr of q list

let is_int_set = function
  | QInt_set _ -> true
  | _ -> false

let cardinal = function
  | QInt_set s -> Int_set.cardinal s
  | _ -> assert false

let sort lst = List.sort (fun a b -> compare (cardinal a) (cardinal b)) lst

let rec compile ~shard = function
  | Regex.True -> QTrue
  | False -> QFalse
  | Exact str ->
    (try QInt_set (Db.find_ngram shard str) with
    | Not_found ->
      (match String.length str with
      | 0 -> QTrue
      | 1 when Db.Alphabet.is_alpha str.[0] -> QTrue
      | _ -> QFalse))
  | And xs ->
    let xs = List.map (compile ~shard) xs in
    let xs = List.filter (( <> ) QTrue) xs in
    if xs = []
    then QTrue
    else if List.mem QFalse xs
    then QFalse
    else (
      let xs0, xs1 = List.partition is_int_set xs in
      QAnd (sort xs0 @ xs1))
  | Or xs ->
    let xs = List.map (compile ~shard) xs in
    let xs = List.filter (( <> ) QFalse) xs in
    if xs = []
    then QFalse
    else if List.mem QTrue xs
    then QTrue
    else (
      let xs0, xs1 = List.partition is_int_set xs in
      QOr (sort xs0 @ xs1))

let rec query_successor i = function
  | QTrue -> i
  | QFalse -> raise Not_found
  | QAnd [] -> i
  | QInt_set bs -> Int_set.successor bs i
  | QAnd xs ->
    let rec go = function
      | [] -> i
      | x :: xs ->
        let j = query_successor i x in
        if j > i then j else go xs
    in
    go xs
  | QOr [] -> raise Not_found
  | QOr (x :: xs) ->
    let rec go acc = function
      | [] ->
        (match acc with
        | None -> raise Not_found
        | Some j -> j)
      | x :: xs ->
        let j = query_successor_opt i x in
        let acc =
          match acc, j with
          | Some acc, Some j -> Some (min acc j)
          | None, t | t, None -> t
        in
        go acc xs
    in
    go (query_successor_opt i x) xs

and query_successor_opt i q =
  try Some (query_successor i q) with
  | Not_found -> None

let stream_of_query ~from q =
  let shard = ref from in
  let go () =
    let rec search i =
      match query_successor i q with
      | j when j = i ->
        shard := i + 1 ;
        Lwt.return (Some j)
      | j ->
        let* () = Lwt_main.yield () in
        search j
      | exception Not_found -> Lwt.return None
    in
    search !shard
  in
  Lwt_stream.from go

let search ~cursor ~shard query =
  let from = cursor.Db.offset in
  stream_of_query ~from (compile ~shard query)

type query =
  { regex : Str.regexp
  ; ngrams : Regex.query
  }

let query_of_string s =
  let q = Regex.parse s in
  let regex = Regex.to_regex q in
  let ngrams = Regex.to_query q in
  { regex; ngrams }

let is_match query str =
  match Str.search_forward query.regex str 0 with
  | exception Not_found -> None
  | start_at ->
    let end_at = Str.match_end () in
    Some (start_at, end_at)

type result =
  { line : string
  ; range : int * int
  ; filename : string
  ; line_num : int
  ; offset : int
  ; cursor : Db.cursor
  }

let search_results ~cursor ~shard query =
  let candidates = search ~cursor ~shard query.ngrams in
  Lwt_stream.filter_map_s
    (fun offset ->
      let line = Db.read_line ~cursor offset in
      match is_match query line with
      | None -> Lwt.return None
      | Some (hl_start_at, hl_end_at) ->
        let filename, start_of_file = Db.fileloc_of_line ~cursor offset in
        let cursor = { cursor with Db.offset } in
        let line_num = offset - start_of_file in
        let result =
          { line; range = hl_start_at, hl_end_at; filename; line_num; offset; cursor }
        in
        Lwt.return (Some result))
    candidates

let search_results ~cursor query =
  Lwt_stream.concat
  @@ Lwt_stream.map (fun (cursor, shard) -> search_results ~cursor ~shard query)
  @@ Db.shards_stream ~cursor

let api ~cursor query =
  let query = query_of_string query in
  let stream = search_results ~cursor query in
  let+ results = Lwt_stream.nget (max_to_find + 1) stream in
  let nb_results = List.length results in
  let last_cursor, results =
    match List.rev results with
    | last :: results when nb_results >= max_to_find ->
      let c = { last.cursor with Db.count = cursor.count + nb_results - 1 } in
      Some c, List.rev results
    | _ -> None, results
  in
  results, last_cursor
