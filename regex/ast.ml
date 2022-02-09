module Char_set = Set.Make (Char)

type t =
  | Empty
  | Any
  | Char of char
  | Char_set of Char_set.t
  | Or of t * t
  | And of t * t
  | Star0 of t
  | Star1 of t
  | Maybe of t
  | Line_start
  | Line_end
  | Word_boundary

let enum a b =
  let a, b = if a < b then a, b else b, a in
  Array.fold_left (fun acc x -> Char_set.add x acc) Char_set.empty
  @@ Array.init (Char.code b - Char.code a + 1) (fun i -> Char.chr (Char.code a + i))

let alphanum =
  List.fold_left
    Char_set.union
    (enum 'A' 'Z')
    [ enum 'a' 'z'; enum '0' '9'; Char_set.singleton '_'; Char_set.singleton '\'' ]
