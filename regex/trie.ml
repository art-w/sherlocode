module Char_set = Ast.Char_set
module Char_map = Map.Make (Char)

type t =
  { eos : bool
  ; children : t Char_map.t
  }

let empty = { eos = false; children = Char_map.empty }
let single = { eos = true; children = Char_map.empty }
let prefix chr t = { eos = false; children = Char_map.singleton chr t }

let prefixes cs t =
  { eos = false
  ; children =
      Char_set.fold (fun chr children -> Char_map.add chr t children) cs Char_map.empty
  }

let alphabet =
  let cs =
    List.fold_left (fun acc chr -> Char_set.add chr acc) Char_set.empty
    @@ List.filter Db.Alphabet.is_valid
    @@ List.init 256 Char.chr
  in
  prefixes cs single

let rec union a b =
  { eos = a.eos || b.eos
  ; children =
      Char_map.merge
        (fun _ ox oy ->
          match ox, oy with
          | None, t | t, None -> t
          | Some x, Some y -> Some (union x y))
        a.children
        b.children
  }

let union_list lst = List.fold_left union empty lst

let fold f t z =
  let rec go prefix z t =
    let z = if t.eos then f prefix z else z in
    Char_map.fold
      (fun chr child z ->
        let prefix = String.make 1 chr ^ prefix in
        go prefix z child)
      t.children
      z
  in
  go "" z t

let rec truncate n t =
  if n = 0
  then { eos = true; children = Char_map.empty }
  else (
    let n = n - 1 in
    { t with children = Char_map.map (fun t -> truncate n t) t.children })

let to_list t = fold (fun s z -> s :: z) t []

exception Abort

let compare_length_with t n =
  try
    let size =
      fold
        (fun _ z ->
          let z = z + 1 in
          if z > n then raise Abort else z)
        t
        0
    in
    compare size n
  with
  | Abort -> 1
