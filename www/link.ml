let is_prefix ~prefix str =
  let n = String.length prefix in
  if String.length str < n then false else prefix = String.sub str 0 n

let link url filename line_number =
  match String.split_on_char '/' url with
  | "github.com" :: _ | "www.github.com" :: _ ->
    Printf.sprintf "https://%s/blob/master/%s#L%i" url filename line_number
  | domain :: _ when is_prefix ~prefix:"gitlab" domain ->
    Printf.sprintf "https://%s/-/blob/master/%s#L%i" url filename line_number
  | "bitbucket.org" :: _ ->
    Printf.sprintf "https://%s/src/master/%s#lines-%i" url filename line_number
  | "forge.ocamlcore.org" :: _ -> "https://forge.ocamlcore.org"
  | "erratique.ch" :: _ ->
    Printf.sprintf "https://%s/tree/%s#n%i" url filename line_number
  | "git.annexia.org" :: proj ->
    Printf.sprintf
      "https://%s/?p=%s.git;f=%s#l%i"
      url
      (String.concat "/" proj)
      filename
      line_number
  | "gforge.inria.fr" :: "git" :: user :: proj
  | "scm.gforge.inria.fr" :: user :: "git" :: proj ->
    Printf.sprintf
      "https://%s/%s/gitweb?p=%s.git;f=%s#l%i"
      url
      user
      (String.concat "/" proj)
      filename
      line_number
  | "gricad-gitlab.univ-grenoble-alpes.fr" :: _ | "git.frama-c.com" :: _ ->
    Printf.sprintf "https://%s/-/blob/master/%s#L%i" url filename line_number
  | "cavale.enseeiht.fr" :: _ -> ""
  | "framagit.org" :: _ -> ""
  | _ -> ""

module H = Hashtbl.Make (struct
  type t = string

  let hash = Hashtbl.hash
  let equal = String.equal
end)

let mapping = H.create 0

let parse_line line =
  match String.split_on_char '\t' line with
  | [ project; remote ] -> H.add mapping project remote
  | _ -> Printf.printf "Link.parse_line error: %S\n%!" line

let load filename =
  let h = open_in filename in
  let rec go () =
    match input_line h with
    | exception End_of_file -> ()
    | line ->
      parse_line line ;
      go ()
  in
  go () ;
  close_in h

let split_project filename =
  let at = String.index filename '/' in
  let project = String.sub filename 0 at in
  let at = at + 1 in
  let file = String.sub filename at (String.length filename - at) in
  project, file

let url filename line_number =
  try
    let project, filename = split_project filename in
    let remote = H.find mapping project in
    link remote filename line_number
  with
  | _ -> ""

let url filename line_number =
  match url filename line_number with
  | "" -> "#unknown"
  | x -> x
