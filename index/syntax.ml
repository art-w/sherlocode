type ctx = Comment
type t = ctx list

let empty : t = []
let char0 = Char.chr 0

module String = struct
  let length = String.length
  let get t i = if i >= String.length t then char0 else String.unsafe_get t i
end

let clean_line ~state line =
  let len = String.length line in
  let buf = Buffer.create len in
  let rec go state i =
    if i >= len
    then state
    else (
      match line.[i], line.[i + 1], line.[i + 2] with
      | '(', '*', ')' ->
        (match state with
        | [] -> Buffer.add_string buf "(*)"
        | _ -> ()) ;
        go state (i + 3)
      | '(', '*', _ -> go (Comment :: state) (i + 2)
      | '*', ')', _ ->
        (match state with
        | Comment :: state -> go state (i + 2)
        | [] ->
          Buffer.add_char buf '*' ;
          go state (i + 1))
      | chr, _, _ ->
        (match state with
        | [] when chr <> char0 -> Buffer.add_char buf chr
        | _ -> ()) ;
        go state (i + 1))
  in
  let state = go state 0 in
  state, Buffer.contents buf
