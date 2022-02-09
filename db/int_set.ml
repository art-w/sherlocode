let sparse_max_size = 32
let base = 32
let int_size = 63

type s =
  | Zeroes
  | Dense of int array
  | Sparse of int array
  | Branch of s array

type t =
  { mutable size : int
  ; mutable cardinal : int
  ; mutable s : s
  }

let cardinal t = t.cardinal
let make () = { size = base * int_size; cardinal = 0; s = Zeroes }

let array_insert t i =
  assert (t.(Array.length t - 1) < i) ;
  Array.concat [ t; [| i |] ]

let min_depth = base * int_size

let dense_of_array t =
  let arr = Array.make base 0 in
  Array.iter
    (fun i ->
      let j = i / int_size in
      let i = i mod int_size in
      arr.(j) <- arr.(j) lor (1 lsl i))
    t ;
  arr

let rec branch_of_array ~depth t =
  if Array.length t < sparse_max_size
  then Sparse t
  else if depth = min_depth
  then Dense (dense_of_array t)
  else (
    let children = Array.make base [] in
    let depth = depth / base in
    for i = Array.length t - 1 downto 0 do
      let j = i / depth in
      let i = i mod depth in
      children.(j) <- i :: children.(j)
    done ;
    let branches =
      Array.map
        (function
          | [] -> Zeroes
          | xs -> branch_of_array ~depth (Array.of_list xs))
        children
    in
    Branch branches)

let rec add_s ~depth t i =
  match t with
  | Zeroes -> Sparse [| i |]
  | Dense bs ->
    assert (depth = min_depth) ;
    let j = i / int_size in
    let b = bs.(j) in
    let i = i mod int_size in
    let b' = b lor (1 lsl i) in
    assert (b' <> b) ;
    bs.(j) <- b' ;
    t
  | Sparse arr ->
    let arr' = array_insert arr i in
    branch_of_array ~depth arr'
  | Branch arr ->
    assert (depth > 0) ;
    let depth = depth / Array.length arr in
    let j = i / depth in
    let child = arr.(j) in
    let i = i mod depth in
    let child' = add_s ~depth child i in
    arr.(j) <- child' ;
    t

let depth size = size

let rec grow ({ size; s; _ } as t) i =
  if i >= size
  then (
    let children = Array.make base Zeroes in
    children.(0) <- s ;
    t.s <- Branch children ;
    t.size <- base * size ;
    grow t i)

let add t i =
  grow t i ;
  t.s <- add_s ~depth:t.size t.s i ;
  t.cardinal <- t.cardinal + 1

exception Found of int

let rec successor_s ~depth t i =
  match t with
  | Zeroes -> ()
  | Dense bs ->
    for real_i = i to (Array.length bs * int_size) - 1 do
      let i = real_i in
      let j = i / int_size in
      let i = i mod int_size in
      let b = bs.(j) in
      let v = b land (1 lsl i) in
      let r = v <> 0 in
      if r
      then (
        let i = real_i in
        raise (Found i))
    done
  | Sparse arr -> Array.iter (fun v -> if v >= i then raise (Found v)) arr
  | Branch bs ->
    let depth = depth / Array.length bs in
    let j = i / depth in
    let i = i mod depth in
    let () =
      try successor_s ~depth bs.(j) i with
      | Found c ->
        let c = (j * depth) + c in
        raise (Found c)
    in
    for k = j + 1 to Array.length bs - 1 do
      try successor_s ~depth bs.(k) 0 with
      | Found c ->
        let c = (k * depth) + c in
        raise (Found c)
    done

let successor { size; s; _ } i =
  if i > size
  then raise Not_found
  else (
    let depth = depth size in
    try
      successor_s ~depth s i ;
      raise Not_found
    with
    | Found j -> j)
