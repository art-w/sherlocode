open Tyxml.Html

let link_to url pattern =
  a ~a:[ a_class [ "example" ]; a_href url ] [ code [ txt pattern ] ]

let link_example pattern =
  link_to (Uri.to_string @@ Uri.of_string ("/?q=" ^ pattern)) pattern

let link_file file = h3 [ a ~a:[ a_href (Link.url file 0) ] [ txt file ] ]

let explain_regex_syntax =
  div
    [ br ()
    ; p
        [ txt {| The pattern is exact by default. Try |}
        ; link_example {|[@@deriving|}
        ; txt {| or |}
        ; link_example {|List.rev_append (List.rev|}
        ; txt " for example."
        ]
    ; p [ txt {|The antislash \ provides support for regular expressions: |} ]
    ; table
        [ tr
            [ td [ code [ txt "\\." ] ]
            ; td [ txt " to match any single character: "; link_example {|.(\.)|} ]
            ]
        ; tr
            [ td [ code [ txt "a\\?" ] ]
            ; td
                [ txt " to optionally match the character a: "
                ; link_example {|fold_left2\? ~f|}
                ; txt " or "
                ; link_example {|type (\?!'|}
                ]
            ]
        ; tr
            [ td [ code [ txt "a\\*" ] ]
            ; td [ txt " to match zero or more times: "; link_example {| 1_0\*|} ]
            ]
        ; tr
            [ td [ code [ txt "a\\+" ] ]
            ; td [ txt " to match one or more times: "; link_to {|/?q=>\%2B||} ">\\+|" ]
            ]
        ; tr
            [ td [ code [ txt "\\(a\\)" ] ]
            ; td [ txt " to group a pattern: "; link_example {|let\( rec\)\? fold_left|} ]
            ]
        ; tr
            [ td [ code [ txt "a\\|b" ] ]
            ; td [ txt " to match either a or b: "; link_example {|match\|function|} ]
            ]
        ; tr
            [ td [ code [ txt "\\[aA\\]" ] ]
            ; td
                [ txt " and ranges "
                ; code [ txt "\\[a-z\\]" ]
                ; txt " to match a character from a set: "
                ; link_example {|(val \[A-Z\]\[a-z_0-9\]\*|}
                ]
            ]
        ; tr
            [ td [ code [ txt "\\w" ] ]
            ; td
                [ txt " is a shortcut for any alphanumeric character "
                ; code [ txt "\\[A-Za-z0-9_'\\]" ]
                ; txt ". Try "
                ; link_example {|| \[A-Z\]\w\* : |}
                ; txt " to search for GADTs!"
                ]
            ]
        ]
    ]

let result_html ~cont_above ~cont_below filename line_num hlmatch =
  let line_num = line_num + 1 in
  let url = Link.url filename line_num in
  let classes =
    (if cont_below then [] else [ "break_below" ])
    @ if cont_above then [] else [ "break_above" ]
  in
  div
    ~a:[ a_class [ "result" ] ]
    [ a ~a:[ a_class ("ln" :: classes); a_href url ] [ txt @@ string_of_int line_num ]
    ; code hlmatch
    ]

let explain_indexing =
  p
    [ txt
        {|The search will proceed on all .ml and .mli, stripped of their comments, on either the package release or the git repository where available.|}
    ]

let github_icon =
  let open Tyxml.Svg in
  Tyxml.Html.svg
    ~a:[ a_width (16., None); a_height (16.0, None); a_viewBox (0., 0., 16., 16.) ]
    [ path
        ~a:
          [ a_d
              "M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 \
               0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 \
               1.08.58 1.23.82.72 1.21 1.87.87 \
               2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 \
               0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 \
               1.32-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 \
               1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 \
               3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.012 \
               8.012 0 0 0 16 8c0-4.42-3.58-8-8-8z"
          ]
        []
    ]

let link_to_repo =
  p
    ~a:[ a_class [ "ad" ] ]
    [ txt {|(* Read the source, fork and contribute to |}
    ; a
        ~a:[ a_href "https://github.com/art-w/sherlocode" ]
        [ github_icon; txt "art-w/sherlocode" ]
    ; txt " *)"
    ]

let frontpage =
  div
    [ h1 [ txt "Sherlocode" ]
    ; p
        ~a:[ a_class [ "hero" ] ]
        [ txt "Search across 17 million lines of OCaml available on opam!" ]
    ; explain_regex_syntax
    ; explain_indexing
    ; link_to_repo
    ]

let search_form query =
  div
    ~a:[ a_class [ "header" ] ]
    [ form
        [ input
            ~a:
              [ a_input_type `Text
              ; a_id "q"
              ; a_name "q"
              ; a_value query
              ; a_placeholder "Search..."
              ; a_autofocus ()
              ; a_autocomplete false
              ]
            ()
        ; input ~a:[ a_input_type `Submit; a_value "Search" ] ()
        ; a
            ~a:
              [ a_href "https://doc.sherlocode.com"
              ; a_title
                  "Fuzzy type/name search for OCaml documentation (Hoogle for odoc!)"
              ]
            [ txt "NEW: try Sherlodoc!" ]
        ]
    ]

let ajax_reload =
  {js|
    var latest = 0;
    var current = 0;
    document.getElementById('q').addEventListener('input', function(e) {

      var param = encodeURIComponent(e.target.value);
      ++latest;
      var self = latest;

      var req = new XMLHttpRequest();
      req.onreadystatechange = function() {
        if (this.readyState === 4 && current < self) {
          current = self;
          document.getElementById('results').innerHTML = this.response;
        }
      };
      req.open('GET', '/api?q=' + param, true);
      req.send();

      var url = param === '' ? '/' : '/?q=' + param;
      history.replaceState(null, 'Sherlocode', url);
    });
  |js}

let template query contents =
  let open Tyxml.Html in
  html
    ~a:[ a_lang "en" ]
    (head
       (title (txt "Sherlocode"))
       [ meta ~a:[ a_charset "UTF-8" ] ()
       ; meta ~a:[ a_name "viewport"; a_content "width=device-width, initial-scale=1" ] ()
       ; link ~rel:[ `Stylesheet ] ~href:"/s.css" ()
       ; link ~rel:[ `Icon ] ~href:"favicon.ico" ()
       ])
  @@ body
       [ search_form query
       ; script (Unsafe.data ajax_reload)
       ; div ~a:[ a_id "results" ] [ contents ]
       ]
