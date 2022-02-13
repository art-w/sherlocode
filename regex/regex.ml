open Ast

type t = Ast.t

type query =
  | True
  | False
  | Exact of string
  | And of query list
  | Or of query list

let rec interleave xs ys =
  match xs with
  | [] -> ys
  | x :: xs -> x :: interleave ys xs

let conj a b =
  match a, b with
  | False, _ | _, False -> False
  | True, t | t, True -> t
  | And xs, And ys -> And (interleave xs ys)
  | And xs, t | t, And xs -> And (t :: xs)
  | _ -> And [ a; b ]

let conjunction = function
  | [] -> True
  | [ t ] -> t
  | xs -> And xs

let diff xs ys =
  let rec go shared xs_only ys_only xs ys =
    match xs, ys with
    | x :: xs, y :: ys when x = y -> go (x :: shared) xs_only ys_only xs ys
    | x :: xs, y :: ys when x < y -> go shared (x :: xs_only) ys_only xs (y :: ys)
    | x :: xs, y :: ys -> go shared xs_only (y :: ys_only) (x :: xs) ys
    | [], [] -> shared, xs_only, ys_only
    | [], ys -> shared, xs_only, interleave ys ys_only
    | xs, [] -> shared, interleave xs xs_only, ys_only
  in
  go [] [] [] (List.sort Stdlib.compare xs) (List.sort Stdlib.compare ys)

let disj a b =
  match a, b with
  | False, t | t, False -> t
  | True, _ | _, True -> True
  | Or xs, Or ys -> Or (interleave xs ys)
  | Or xs, t | t, Or xs -> Or (t :: xs)
  | And xs, And ys ->
    let shared, xs, ys = diff xs ys in
    conj (conjunction shared) (Or (interleave xs ys))
  | _ -> Or [ a; b ]

let disjunction = function
  | [] -> True
  | xs when List.mem True xs -> True
  | [ x ] -> x
  | x :: xs ->
    let xs = List.filter (( <> ) False) xs in
    List.fold_right disj xs x

let truncate cs = Trie.truncate 2 cs
let disj_of_trie cs = disjunction @@ List.map (fun str -> Exact str) @@ Trie.to_list cs

let query_of_cs cs =
  let rec go depth cs =
    if Trie.compare_length_with cs 111 < 0
    then disj_of_trie cs
    else if depth = 0
    then True
    else go (depth - 1) (Trie.truncate depth cs)
  in
  go 2 cs

let rec make_query previous t =
  match t with
  | Empty | Line_end | Word_boundary -> True, previous
  | Line_start -> True, Trie.single
  | Any -> True, Trie.alphabet
  | Maybe r ->
    let _, previous1 = make_query previous r in
    let previous = Trie.union previous previous1 in
    True, previous
  | Star0 r ->
    let _, previous1 = make_query previous r in
    let _, previous2 = make_query previous1 r in
    let _, previous3 = make_query previous2 r in
    let previous = Trie.union_list [ previous; previous1; previous2; previous3 ] in
    True, previous
  | Star1 r ->
    let s, previous1 = make_query previous r in
    let _, previous2 = make_query previous1 r in
    let _, previous3 = make_query previous2 r in
    let previous = Trie.union_list [ previous1; previous2; previous3 ] in
    s, previous
  | And (a, b) ->
    let req_a, previous = make_query previous a in
    let req_b, previous = make_query previous b in
    conj req_a req_b, previous
  | Or (a, b) ->
    let req_a, previous_a = make_query previous a in
    let req_b, previous_b = make_query previous b in
    let previous = Trie.union previous_a previous_b in
    disj req_a req_b, previous
  | Char c ->
    let req = Trie.prefix c previous in
    query_of_cs req, truncate req
  | Char_set cs ->
    let req = Trie.prefixes cs previous in
    query_of_cs req, truncate req

let to_query r =
  let q, _ = make_query Trie.single r in
  q

let parse s =
  try Parser.main Lexer.token (Lexing.from_string s) with
  | _ -> Empty

let reserved_chars = "$^\\.*+?[]"
let is_reserved c = String.index_from_opt reserved_chars 0 c <> None

let rec to_regex = function
  | Empty -> ""
  | Any -> "."
  | Char c when is_reserved c -> Printf.sprintf "\\%c" c
  | Char c -> Printf.sprintf "%c" c
  | Char_set cs ->
    let buf = Buffer.create (Char_set.cardinal cs) in
    Buffer.add_char buf '[' ;
    if Char_set.mem ']' cs then Buffer.add_char buf ']' ;
    Char_set.iter
      (fun c -> if not (c = ']' || c = '-' || c = '^') then Buffer.add_char buf c)
      cs ;
    if Char_set.mem '^' cs then Buffer.add_char buf '^' ;
    if Char_set.mem '-' cs then Buffer.add_char buf '-' ;
    Buffer.add_char buf ']' ;
    Buffer.contents buf
  | Or (a, b) -> "\\(" ^ to_regex a ^ "\\|" ^ to_regex b ^ "\\)"
  | And (a, b) -> to_regex a ^ to_regex b
  | Star0 r -> to_regex_parens r ^ "*"
  | Star1 r -> to_regex_parens r ^ "+"
  | Maybe r -> to_regex_parens r ^ "?"
  | Line_start -> "^"
  | Line_end -> "$"
  | Word_boundary -> "\\b"

and to_regex_parens r =
  match r with
  | Empty | Any | Char _ | Char_set _ -> to_regex r
  | _ -> "\\(" ^ to_regex r ^ "\\)"

let to_regex r = Str.regexp (to_regex r)
