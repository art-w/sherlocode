module Ngrams : Hashtbl.S with type key = string
module Int_map : Map.S with type key = int
module Int_tbl : Hashtbl.S with type key = int

module Int_set : sig
  type t

  val cardinal : t -> int
  val make : unit -> t
  val add : t -> int -> unit
  val successor : t -> int -> int
end

module Alphabet : sig
  val is_valid : char -> bool
  val is_valid_string : string -> bool
  val is_alpha : char -> bool
end

type t =
  { line_offsets : int Int_tbl.t
  ; ngrams : Int_set.t Ngrams.t
  ; file_map : (string * int) Int_map.t
  }

val make : unit -> t
val set_file_ranges : string -> int * int -> t -> t
val set_line_offset : t -> int -> int -> unit
val find_ngram : t -> string -> Int_set.t

type writer

val open_out : string -> writer
val save : db:writer -> t -> unit
val close_out : writer -> unit

type reader

val db_open_in : source:string -> db:string -> reader

type cursor =
  { db : reader
  ; shard : int
  ; offset : int
  ; count : int
  }

val cursor_empty : db:reader -> cursor
val string_of_cursor : cursor -> string
val cursor_of_string : db:reader -> string -> cursor
val shards_stream : cursor:cursor -> (cursor * t) Lwt_stream.t
val fileloc_of_line : cursor:cursor -> int -> string * int
val read_line : cursor:cursor -> int -> string
val total_lines : cursor:cursor -> int
val total_lines_upto : cursor:cursor -> int
