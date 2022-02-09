module Ngrams = Db.Ngrams
module Int_set = Db.Int_set

let max_lines_per_shard = 1_000_000

let string_drop_prefix ~prefix str =
  let n = String.length prefix in
  if String.length str < n
  then str
  else if String.sub str 0 n <> prefix
  then str
  else String.sub str n (String.length str - n)

type t =
  { prefix : string
  ; db : Db.writer
  ; sources : out_channel
  ; offset : int
  ; nb_lines : int
  ; shard : Db.t
  }

let make ~prefix ~ancient ~sources =
  { prefix
  ; db = Db.open_out ancient
  ; sources = open_out sources
  ; offset = 0
  ; nb_lines = 0
  ; shard = Db.make ()
  }

let set_file_ranges filename range t =
  let filename = string_drop_prefix ~prefix:t.prefix filename in
  { t with shard = Db.set_file_ranges filename range t.shard }

let output_line t str =
  Db.set_line_offset t.shard t.nb_lines t.offset ;
  output_string t.sources str ;
  output_char t.sources '\n' ;
  { t with offset = t.offset + String.length str + 1; nb_lines = t.nb_lines + 1 }

let store key value t =
  let bs =
    match Ngrams.find t.shard.ngrams key with
    | bs -> bs
    | exception Not_found ->
      let bs = Int_set.make () in
      Ngrams.add t.shard.ngrams key bs ;
      bs
  in
  Int_set.add bs value

let get_ngrams str =
  let ngrams = Ngrams.create (String.length str) in
  for i = 0 to String.length str - 1 do
    let chr = str.[i] in
    if not (Db.Alphabet.is_alpha chr)
    then (
      let single = String.make 1 chr in
      Ngrams.replace ngrams single ())
  done ;
  for len = 2 to 3 do
    for i = 0 to String.length str - len do
      let shingle = String.sub str i len in
      if Db.Alphabet.is_valid_string shingle then Ngrams.replace ngrams shingle ()
    done
  done ;
  ngrams

let fold_lines f acc filename =
  try
    let h = open_in filename in
    let rec go acc =
      match input_line h with
      | line ->
        let acc = f acc line in
        go acc
      | exception End_of_file ->
        close_in h ;
        acc
    in
    go acc
  with
  | Sys_error _ -> acc

let index filename t =
  let start_at = t.nb_lines in
  let t, _ =
    fold_lines
      (fun (t, parse_state) str ->
        let parse_state, str = Syntax.clean_line ~state:parse_state str in
        Ngrams.iter (fun ngram () -> store ngram t.nb_lines t) (get_ngrams str) ;
        let t = output_line t str in
        t, parse_state)
      (t, Syntax.empty)
      filename
  in
  if t.nb_lines > start_at then set_file_ranges filename (start_at, t.nb_lines) t else t

let save t =
  let () = Db.save ~db:t.db t.shard in
  Printf.printf "Created shard: %i lines\n%!" t.nb_lines ;
  let t = { t with shard = Db.make (); nb_lines = 0 } in
  Gc.compact () ;
  t

let rec fold_stdin f z =
  match read_line () with
  | filename ->
    let z = f z filename in
    fold_stdin f z
  | exception End_of_file -> z

let db_create_stdin t =
  let t =
    fold_stdin
      (fun t filename ->
        let t = if t.nb_lines < max_lines_per_shard then t else save t in
        index filename t)
      t
  in
  if t.nb_lines > 0 then save t else t

let () =
  let prefix = Sys.argv.(1) ^ "/" in
  let path = Sys.argv.(2) in
  let ancient = path ^ "/ancient.db" in
  let sources = path ^ "/source.txt" in
  let t0 = Unix.gettimeofday () in
  let t = make ~prefix ~ancient ~sources in
  let t = db_create_stdin t in
  Db.close_out t.db ;
  flush t.sources ;
  close_out t.sources ;
  let t1 = Unix.gettimeofday () in
  Printf.printf "Indexing in %fs\n%!" (t1 -. t0)
