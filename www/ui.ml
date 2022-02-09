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

let empty_propaganda =
  div
    [ h1 [ txt "Sherlocode" ]
    ; p
        ~a:[ a_class [ "hero" ] ]
        [ txt "Search across 17 million lines of OCaml available on opam!" ]
    ; explain_regex_syntax
    ; explain_indexing
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
