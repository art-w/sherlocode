module Int_set = Int_set

module Ngrams = Hashtbl.Make (struct
  type t = string

  let hash = Hashtbl.hash
  let equal = String.equal
end)

module Int_tbl = Hashtbl.Make (struct
  type t = int

  let hash = Hashtbl.hash
  let equal = Int.equal
end)

module Int_map = Map.Make (struct
  type t = int

  let compare = compare
end)

module Alphabet = struct
  let valid_char c =
    (c >= 'a' && c <= 'z')
    || (c >= 'A' && c <= 'Z')
    || (c >= '0' && c <= '9')
    || String.contains " -+*%~#()[]{}$@!&|/\\`=^:;.,_'\"<>?\n" c

  let alphabet = Array.make 256 '\000'
  let indexes = Array.make 256 (-1)

  let length =
    let count = ref 0 in
    for i = 0 to Array.length indexes - 1 do
      if valid_char (Char.chr i)
      then (
        indexes.(i) <- !count ;
        alphabet.(!count) <- Char.chr i ;
        incr count)
    done ;
    !count

  let index c = indexes.(Char.code c)
  let _chr i = alphabet.(i)
  let is_valid c = index c >= 0
  let is_alpha chr = chr = ' ' || (chr >= 'a' && chr <= 'z')

  let is_valid_string s =
    let rec go i = if i >= String.length s then true else is_valid s.[i] && go (i + 1) in
    go 0
end

type t =
  { line_offsets : int Int_tbl.t
  ; ngrams : Int_set.t Ngrams.t
  ; file_map : (string * int) Int_map.t
  }

let make () =
  { line_offsets = Int_tbl.create 16
  ; ngrams =
      Ngrams.create (Alphabet.length * (1 + (Alphabet.length * (1 + Alphabet.length))))
  ; file_map = Int_map.empty
  }

let set_file_ranges filename (start_at, end_at) t =
  { t with file_map = Int_map.add end_at (filename, start_at) t.file_map }

let set_line_offset t line_num line_offset =
  Int_tbl.replace t.line_offsets line_num line_offset

let find_ngram t key = Ngrams.find t.ngrams key
let base_addr = 0x100000000000n

type writer =
  { mutable write_shard : int
  ; ancient : Ancient.md
  }

let open_out filename =
  let handle = Unix.openfile filename Unix.[ O_RDWR; O_TRUNC; O_CREAT ] 0o640 in
  let ancient = Ancient.attach handle base_addr in
  { write_shard = 0; ancient }

let save ~db (t : t) =
  ignore (Ancient.share db.ancient db.write_shard t) ;
  db.write_shard <- db.write_shard + 1

let close_out db = Ancient.detach db.ancient

module Bigstring : sig
  type t

  val of_file : string -> t
  val sub : t -> int -> int -> string
end = struct
  type t = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  let of_file filename =
    let h = Unix.openfile filename [ Unix.O_RDONLY ] 0 in
    let big = Unix.map_file h Bigarray.char Bigarray.c_layout false [| -1 |] in
    let big = Bigarray.array1_of_genarray big in
    Unix.close h ;
    big

  let sub t pos len = String.init len (fun i -> Bigarray.Array1.get t (pos + i))
end

type reader =
  { shards : t array
  ; source : Bigstring.t
  }

let load_shard md shard =
  match Ancient.get md shard with
  | t -> Some (Ancient.follow t)
  | exception _ -> None

let load_shards md =
  let rec go i =
    match load_shard md i with
    | None -> []
    | Some t -> t :: go (i + 1)
  in
  Array.of_list (go 0)

let db_open_in ~source ~db : reader =
  let filename = db in
  let handle = Unix.openfile filename Unix.[ O_RDWR ] 0o640 in
  let md = Ancient.attach handle base_addr in
  let source = Bigstring.of_file source in
  { shards = load_shards md; source }

type cursor =
  { db : reader
  ; shard : int
  ; offset : int
  ; count : int
  }

let cursor_empty ~db = { db; shard = 0; offset = 0; count = 0 }
let string_of_cursor c = Printf.sprintf "%i,%i,%i" c.count c.shard c.offset

let cursor_of_string ~db str =
  match String.split_on_char ',' str with
  | [ count; shard; offset ] ->
    let int_of_string s =
      try int_of_string s with
      | _ -> 0
    in
    { db
    ; shard = int_of_string shard
    ; offset = int_of_string offset
    ; count = int_of_string count
    }
  | _ -> cursor_empty ~db

let shards_stream ~cursor =
  let state = ref cursor in
  let f () =
    let cursor = !state in
    if cursor.shard >= Array.length cursor.db.shards
    then None
    else (
      state := { cursor with shard = cursor.shard + 1; offset = 0 } ;
      let shard = cursor.db.shards.(cursor.shard) in
      Some (cursor, shard))
  in
  Lwt_stream.from_direct f

let get_shard cursor = cursor.db.shards.(cursor.shard)

let fileloc_of_line ~cursor line =
  let shard = get_shard cursor in
  let _end_of_file, (filename, start_of_file) =
    Int_map.find_first (fun loc -> Int.compare loc line > 0) shard.file_map
  in
  filename, start_of_file

let read_line ~cursor line_num =
  let shard = get_shard cursor in
  let line_offsets = shard.line_offsets in
  let start_of_line = Int_tbl.find line_offsets line_num in
  let end_of_line = Int_tbl.find line_offsets (line_num + 1) in
  let len = end_of_line - start_of_line - 1 in
  let line = Bigstring.sub cursor.db.source start_of_line len in
  line

let total_lines ~cursor =
  Array.fold_left
    (fun acc shard ->
      let shard_size = Int_tbl.length shard.line_offsets in
      acc + shard_size)
    0
    cursor.db.shards

let total_lines_upto ~cursor =
  let total = ref 0 in
  for i = 0 to cursor.shard - 1 do
    let shard = cursor.db.shards.(i) in
    let shard_size = Int_tbl.length shard.line_offsets in
    total := !total + shard_size
  done ;
  !total + cursor.offset
