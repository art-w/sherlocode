type t

val empty : t
val single : t
val alphabet : t
val prefix : char -> t -> t
val prefixes : Ast.Char_set.t -> t -> t
val truncate : int -> t -> t
val to_list : t -> string list
val union : t -> t -> t
val union_list : t list -> t
val compare_length_with : t -> int -> int
