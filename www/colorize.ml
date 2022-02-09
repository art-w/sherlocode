module H = Tyxml.Html
module Higlo = Higlo.Lang

let span cl s = H.span ~a:[ H.a_class [ cl ] ] [ H.txt s ]

let html_of_token = function
  | Higlo.Text str -> H.txt str
  | Symbol (_, s) -> span "symbol" s
  | String s -> span "string" s
  | Numeric s -> span "numeric" s
  | Lcomment s -> span "comment" s
  | Bcomment s -> span "comment" s
  | Keyword (_, s) -> span "kw" s
  | Escape s -> span "escape" s
  | Directive s -> span "directive" s
  | Constant s -> span "constant" s
  | Id s -> span "ident" s

let string_of_token = function
  | Higlo.Text s
  | Symbol (_, s)
  | String s
  | Numeric s
  | Lcomment s
  | Bcomment s
  | Keyword (_, s)
  | Escape s
  | Directive s
  | Constant s
  | Id s -> s

let token_replace s = function
  | Higlo.Text _ -> Higlo.Text s
  | Symbol (n, _) -> Symbol (n, s)
  | String _ -> String s
  | Numeric _ -> Numeric s
  | Lcomment _ -> Lcomment s
  | Bcomment _ -> Bcomment s
  | Keyword (n, _) -> Keyword (n, s)
  | Escape _ -> Escape s
  | Directive _ -> Directive s
  | Constant _ -> Constant s
  | Id _ -> Id s

let string_split i str = String.sub str 0 i, String.sub str i (String.length str - i)

let rec take acc n = function
  | [] -> List.rev acc, []
  | t :: ts ->
    let txt = string_of_token t in
    let txt_len = String.length txt in
    if n > txt_len
    then take (t :: acc) (n - txt_len) ts
    else (
      let txt_before, txt_after = string_split n txt in
      let tok_before = token_replace txt_before t in
      let tok_after = token_replace txt_after t in
      List.rev (tok_before :: acc), tok_after :: ts)

let take n ts = take [] n ts

let to_html line =
  let tokens = Higlo.parse ~lang:"ocaml" line in
  List.map html_of_token tokens

let to_html_highlight ~mark line (start_at, end_at) =
  let tokens = Higlo.parse ~lang:"ocaml" line in
  let start, rest = take start_at tokens in
  let inside, rest = take (end_at - start_at) rest in
  List.map html_of_token start
  @ [ mark @@ List.map html_of_token inside ]
  @ List.map html_of_token rest
