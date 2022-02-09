type t

val parse : string -> t

type query =
  | True
  | False
  | Exact of string
  | And of query list
  | Or of query list

val to_query : t -> query
val to_regex : t -> Str.regexp
