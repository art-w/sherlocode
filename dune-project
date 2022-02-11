(lang dune 2.8)
(using menhir 2.0)
(generate_opam_files true)

(name sherlocode)
(source (github art-w/sherlocode))
(license MIT)
(authors "Arthur Wendling")
(maintainers "art.wendling@gmail.com")
(version 0.1)

(package
  (name sherlocode)
  (synopsis "Realtime grep for OCaml sources available on opam")
  (depends
    (ocaml (>= "4.08"))
    (ancient (>= "0.9.1"))
    (lwt (>= "4.5.2"))
    (higlo (>= "0.8"))
    (tyxml (>= "4.5.0"))
    dream))
