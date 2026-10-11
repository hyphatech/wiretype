(* The house rules a reader of the source can check: run by [dune test], so
   CI and an agent find a broken rule as a failing test naming the line. *)

(* The libraries' directories. [src/gen/] is the programs the build runs to
   write the library's tables, which read their arguments and fail as a
   command does, and is held to the rules on opens and warnings alone. *)
let dirs = [ "src"; "ppx" ]
let command_dirs = [ "src/gen" ]

(* An [open] is refused but where the file names why: [Tuple] is written
   inside [Tuple.( ... )], as its [.mli] says, and a deriver reads Ppxlib's
   AST and matches its patterns, as every ppxlib rewriter does. *)
let allowed_opens =
  [
    ("src/wiretype.ml", "Tuple");
    ("ppx/ppx_wiretype.ml", "Ppxlib");
    ("ppx/ppx_wiretype.ml", "Ast_pattern");
  ]

(* A banned identifier allowed where the [.mli] documents it: a description
   is a constant written in source, so one that can mean nothing raises
   where it is built. *)
let allowed_banned = [ ("src/wiretype.ml", "invalid_arg") ]
let read path = In_channel.with_open_bin path In_channel.input_all

(* The source with comments, string literals and character literals blanked
   to spaces, newlines kept, so what is left is code and lines still count. *)
let code_of s =
  let n = String.length s in
  let out = Bytes.of_string s in
  let blank i = if not (Char.equal s.[i] '\n') then Bytes.set out i ' ' in
  let rec string i =
    if i >= n then i
    else (
      blank i;
      match s.[i] with
      | '\\' when i + 1 < n ->
          blank (i + 1);
          string (i + 2)
      | '"' -> i + 1
      | _ -> string (i + 1))
  in
  (* {id|...|id}, with [id] lowercase letters or underscores. *)
  let quoted i =
    let j = ref (i + 1) in
    while !j < n && match s.[!j] with 'a' .. 'z' | '_' -> true | _ -> false do
      incr j
    done;
    if !j < n && Char.equal s.[!j] '|' then (
      let close = "|" ^ String.sub s (i + 1) (!j - i - 1) ^ "}" in
      let k = ref (!j + 1) in
      while
        !k + String.length close <= n
        && not (String.equal (String.sub s !k (String.length close)) close)
      do
        incr k
      done;
      let stop = Int.min n (!k + String.length close) in
      for x = i to stop - 1 do
        blank x
      done;
      Some stop)
    else None
  in
  (* 'x', '\n', '\'' and '\123'; a type variable's quote is left alone. *)
  let char i =
    if i + 2 < n && Char.equal s.[i + 1] '\\' then (
      let j = ref (i + 2) in
      while !j < n && not (Char.equal s.[!j] '\'') do
        incr j
      done;
      if !j - i <= 5 then (
        for x = i to !j do
          blank x
        done;
        Some (!j + 1))
      else None)
    else if i + 2 < n && Char.equal s.[i + 2] '\'' then (
      for x = i to i + 2 do
        blank x
      done;
      Some (i + 3))
    else None
  in
  let rec comment depth i =
    if i >= n then i
    else if i + 1 < n && Char.equal s.[i] '(' && Char.equal s.[i + 1] '*' then (
      blank i;
      blank (i + 1);
      comment (depth + 1) (i + 2))
    else if i + 1 < n && Char.equal s.[i] '*' && Char.equal s.[i + 1] ')' then (
      blank i;
      blank (i + 1);
      if depth = 1 then i + 2 else comment (depth - 1) (i + 2))
    else if Char.equal s.[i] '"' then (
      blank i;
      comment depth (string (i + 1)))
    else (
      blank i;
      comment depth (i + 1))
  in
  let rec go i =
    if i >= n then ()
    else
      match s.[i] with
      | '(' when i + 1 < n && Char.equal s.[i + 1] '*' -> go (comment 0 i)
      | '"' ->
          blank i;
          go (string (i + 1))
      | '{' -> ( match quoted i with Some j -> go j | None -> go (i + 1))
      | '\'' -> ( match char i with Some j -> go j | None -> go (i + 1))
      | _ -> go (i + 1)
  in
  go 0;
  Bytes.to_string out

let is_ident_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' | '.' -> true
  | _ -> false

(* Every identifier, dotted path included, with its line. A field access
   ([t.open]) begins with a lowercase name and so never matches a banned
   path, and a path never ends in a dot. *)
let identifiers code =
  let n = String.length code in
  let rec go i line acc =
    if i >= n then List.rev acc
    else if Char.equal code.[i] '\n' then go (i + 1) (line + 1) acc
    else if is_ident_char code.[i] && not (Char.equal code.[i] '.') then (
      let j = ref i in
      while !j < n && is_ident_char code.[!j] do
        incr j
      done;
      let word = String.sub code i (!j - i) in
      let word =
        if String.ends_with ~suffix:"." word then
          String.sub word 0 (String.length word - 1)
        else word
      in
      go !j line ((line, word) :: acc))
    else go (i + 1) line acc
  in
  go 0 1 []

(* What the build writes beside the source: a preprocessed copy, and the
   tables [src/gen/] prints. *)
let generated file =
  String.ends_with ~suffix:".pp.ml" file
  || String.ends_with ~suffix:".pp.mli" file
  || List.mem file [ "idna_data.ml"; "powers.ml" ]

(* Every source file with the given suffix, as a path from the root. *)
let sources ?(dirs = dirs) suffix =
  List.concat_map
    (fun dir ->
      Sys.readdir (Filename.concat ".." dir)
      |> Array.to_list
      |> List.filter (fun file ->
          String.ends_with ~suffix file && not (generated file))
      |> List.sort String.compare
      |> List.map (Filename.concat dir))
    dirs

let read_source file = read (Filename.concat ".." file)

(* A name is read with a leading [Stdlib.] taken off, so [Stdlib.exit] is
   [exit]. *)
let unqualified word =
  match String.starts_with ~prefix:"Stdlib." word with
  | true -> String.sub word 7 (String.length word - 7)
  | false -> word

(* The names OCAML.md refuses, one a line in its block fenced as
   [refused], each with a leading [Stdlib.] taken off; none when the block
   is missing or empty. *)
let refused_of_markdown text =
  let rec outside = function
    | [] -> []
    | l :: rest when String.equal (String.trim l) "```refused" -> inside rest
    | _ :: rest -> outside rest
  and inside = function
    | [] -> []
    | l :: _ when String.equal (String.trim l) "```" -> []
    | l :: rest -> (
        match String.trim l with
        | "" -> inside rest
        | name -> name :: inside rest)
  in
  String.split_on_char '\n' text
  |> outside |> List.map unqualified
  |> List.sort_uniq String.compare

(* The shared refusals, read from the copy of OCAML.md at the root, so no
   list is kept here: a block that cannot be found fails every test that
   reads it rather than refusing nothing. *)
let refused () =
  match refused_of_markdown (read_source "OCAML.md") with
  | [] -> Alcotest.fail "OCAML.md has no names in a block fenced as refused"
  | names -> names

(* Each refused identifier in [source], a file's text, as a line naming it. *)
let banned_in ~refused file source =
  identifiers (code_of source)
  |> List.map (fun (line, word) -> (line, unqualified word))
  |> List.filter (fun (_, word) ->
      List.exists (String.equal word) refused
      && not
           (List.exists
              (fun (f, w) -> String.equal f file && String.equal w word)
              allowed_banned))
  |> List.map (fun (line, word) ->
      Printf.sprintf "%s:%d: %s -- refused by OCAML.md" file line word)

let no_banned_identifier () =
  let refused = refused () in
  let found =
    List.concat_map
      (fun file -> banned_in ~refused file (read_source file))
      (sources ".ml" @ sources ".mli")
  in
  Alcotest.(check (list string)) "banned identifiers" [] found

(* The block is read whole, and a refused name is found bare and behind
   [Stdlib.]. *)
let refusals_read_from_ocaml_md () =
  let refused = refused () in
  List.iter
    (fun name ->
      Alcotest.(check bool)
        (name ^ " is refused") true
        (List.exists (String.equal name) refused))
    [ "failwith"; "Unix.sleepf"; "compare"; "Eio.Time.Mono.sleep" ];
  Alcotest.(check (list string))
    "found"
    [ "row.ml:1: exit -- refused by OCAML.md" ]
    (banned_in ~refused "row.ml" "let _ = Stdlib.exit");
  Alcotest.(check (list string))
    "a document with no block" []
    (refused_of_markdown "# OCaml\n\n```ocaml\nfailwith\n```\n")

(* Each name ending in a unit, [_s], [_ms], [_us] or [_ns], given the type
   [float], with its line:
   [x_s : float], [~x_s:float] and [~x_s:(x : float)]. A time counted in a
   bare float is what OCAML.md's shared shapes refuse: a [Ptime.t] or an
   [Mtime.Span.t] says its unit by its type. *)
let floats_with_a_unit code =
  let n = String.length code in
  let is_name_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
    | _ -> false
  in
  let rec blanks i =
    if
      i < n
      && match code.[i] with ' ' | '\n' | '\t' | '\r' -> true | _ -> false
    then blanks (i + 1)
    else i
  in
  let rec name_end i =
    if i < n && is_name_char code.[i] then name_end (i + 1) else i
  in
  let float_at i =
    i + 5 <= n
    && String.equal (String.sub code i 5) "float"
    && (i + 5 = n || not (is_name_char code.[i + 5]))
  in
  (* A colon that is a type's, not [::] or [:=]. *)
  let colon_at i =
    i < n
    && Char.equal code.[i] ':'
    && not
         (i + 1 < n
         && (Char.equal code.[i + 1] ':' || Char.equal code.[i + 1] '='))
  in
  let typed_float i =
    colon_at i
    &&
    let j = blanks (i + 1) in
    float_at j
    || j < n
       && Char.equal code.[j] '('
       &&
       let k = blanks (name_end (blanks (j + 1))) in
       colon_at k && float_at (blanks (k + 1))
  in
  let rec go i line acc =
    if i >= n then List.rev acc
    else if Char.equal code.[i] '\n' then go (i + 1) (line + 1) acc
    else if is_name_char code.[i] && (i = 0 || not (is_name_char code.[i - 1]))
    then
      let j = name_end i in
      let name = String.sub code i (j - i) in
      let with_unit =
        List.exists
          (fun suffix -> String.ends_with ~suffix name)
          [ "_s"; "_ms"; "_us"; "_ns" ]
      in
      go j line
        (if with_unit && typed_float (blanks j) then (line, name) :: acc
         else acc)
    else go (i + 1) line acc
  in
  go 0 1 []

let no_float_with_a_unit () =
  let found =
    List.concat_map
      (fun file ->
        floats_with_a_unit (code_of (read_source file))
        |> List.map (fun (line, name) ->
            Printf.sprintf
              "%s:%d: %s is a float with a unit -- a time is a Ptime.t or an \
               Mtime.Span.t"
              file line name))
      (sources ".ml" @ sources ".mli")
  in
  Alcotest.(check (list string)) "floats with a unit" [] found

(* The forms OCAML.md's shared shapes refuse are each found, and a float
   that is no time, or a time that is no float, is not. *)
let every_float_with_a_unit () =
  Alcotest.(check (list string))
    "found"
    [ "wait_s"; "timeout_ms"; "delay_s"; "after_ms"; "tick_us"; "elapsed_ns" ]
    (List.map snd
       (floats_with_a_unit
          "val f : wait_s:float -> unit\n\
           let g (timeout_ms : float) = timeout_ms\n\
           let h ?delay_s:(d : float) () = d\n\
           type t = { after_ms : float }\n\
           val i : tick_us:float -> elapsed_ns:float -> unit\n\
           let ok (ratio : float) (n_s : int) xs_s = n_s :: xs_s\n\
           let r_s := 1.0"))

(* Each [Eio.Time.now] not written as [Ptime.of_float_s (Eio.Time.now
   clock)], with its line: OCAML.md makes that the one place a clock
   becomes an instant. *)
let clocks_read_bare code =
  let n = String.length code in
  let name = "Eio.Time.now" and wrap = "Ptime.of_float_s" in
  let is_name_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' | '.' -> true
    | _ -> false
  in
  let is_blank = function ' ' | '\n' | '\t' | '\r' -> true | _ -> false in
  let rec back_blanks i =
    if i > 0 && is_blank code.[i - 1] then back_blanks (i - 1) else i
  in
  let ends_at s i =
    let m = String.length s in
    i >= m
    && String.equal (String.sub code (i - m) m) s
    && (i = m || not (is_name_char code.[i - m - 1]))
  in
  let wrapped i =
    let j = back_blanks i in
    j > 0 && Char.equal code.[j - 1] '(' && ends_at wrap (back_blanks (j - 1))
  in
  let m = String.length name in
  let rec go i line acc =
    if i + m > n then List.rev acc
    else if Char.equal code.[i] '\n' then go (i + 1) (line + 1) acc
    else if
      String.equal (String.sub code i m) name
      && (i = 0 || not (is_name_char code.[i - 1]))
      && (i + m = n || not (is_name_char code.[i + m]))
    then go (i + m) line (if wrapped i then acc else line :: acc)
    else go (i + 1) line acc
  in
  go 0 1 []

let no_clock_read_bare () =
  let found =
    List.concat_map
      (fun file ->
        clocks_read_bare (code_of (read_source file))
        |> List.map (fun line ->
            Printf.sprintf
              "%s:%d: Eio.Time.now -- write Ptime.of_float_s (Eio.Time.now \
               clock)"
              file line))
      (sources ".ml" @ sources ".mli")
  in
  Alcotest.(check (list string)) "clocks read bare" [] found

(* The one form is let through, across a line break too, and any other
   is found. *)
let every_clock_read_bare () =
  Alcotest.(check (list int))
    "found" [ 4; 5 ]
    (clocks_read_bare
       "let a = Ptime.of_float_s (Eio.Time.now clock)\n\
        let b = Ptime.of_float_s\n\
       \ (Eio.Time.now clock)\n\
        let c = Eio.Time.now clock\n\
        let d = Float.of_int 1 +. Eio.Time.now clock")

(* Every local open, [M.(...)], [M.[...]] or [M.{...}], with its line and
   module: a bracket after a path whose last name is capitalised, with any
   blanks between, since ocamlformat may break the line after the dot. An
   array's or a string's index follows a value's name, which is not. An
   operator reached by its path, [M.( + )], is matched too; bind it to a
   name. *)
let local_opens code =
  let n = String.length code in
  let is_name_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
    | _ -> false
  in
  let rec bracket_after i =
    i < n
    &&
    match code.[i] with
    | ' ' | '\n' | '\t' | '\r' -> bracket_after (i + 1)
    | '(' | '[' | '{' -> true
    | _ -> false
  in
  let rec go i line acc =
    if i + 1 >= n then List.rev acc
    else if Char.equal code.[i] '\n' then go (i + 1) (line + 1) acc
    else if Char.equal code.[i] '.' && bracket_after (i + 1) then (
      let j = ref i in
      while !j > 0 && is_name_char code.[!j - 1] do
        decr j
      done;
      match code.[!j] with
      | 'A' .. 'Z' when !j < i ->
          go (i + 1) line ((line, String.sub code !j (i - !j)) :: acc)
      | _ -> go (i + 1) line acc)
    else go (i + 1) line acc
  in
  go 0 1 []

let no_open_but_the_named () =
  let allowed file name =
    List.exists
      (fun (f, m) -> String.equal f file && String.equal m name)
      allowed_opens
  in
  let found =
    List.concat_map
      (fun file ->
        let code = code_of (read_source file) in
        let local =
          List.filter_map
            (fun (line, name) ->
              if allowed file name then None
              else
                Some
                  (Printf.sprintf "%s:%d: %s.( ... ) -- use a module alias" file
                     line name))
            (local_opens code)
        in
        let rec opens = function
          | (line, "open") :: (_, name) :: rest ->
              if allowed file name then opens rest
              else
                Printf.sprintf "%s:%d: open %s -- use a module alias" file line
                  name
                :: opens rest
          | _ :: rest -> opens rest
          | [] -> []
        in
        local @ opens (identifiers code))
      (sources ~dirs:(dirs @ command_dirs) ".ml" @ sources ".mli")
  in
  Alcotest.(check (list string)) "opens" [] found

let no_silenced_warning () =
  let found =
    List.concat_map
      (fun file ->
        let code = code_of (read_source file) in
        String.split_on_char '\n' code
        |> List.mapi (fun i l -> (i + 1, l))
        |> List.filter_map (fun (line, l) ->
            let rec has i =
              i + 8 <= String.length l
              && (String.equal (String.sub l i 8) "@warning" || has (i + 1))
            in
            if has 0 then
              Some
                (Printf.sprintf
                   "%s:%d: a silenced warning is a code shape to fix" file line)
            else None))
      (sources ~dirs:(dirs @ command_dirs) ".ml" @ sources ".mli")
  in
  Alcotest.(check (list string)) "silenced warnings" [] found

(* A dune file with its comments, from a [;] to the end of its line, cut. *)
let code_of_dune s =
  String.split_on_char '\n' s
  |> List.map (fun line ->
      match String.index_opt line ';' with
      | Some i -> String.sub line 0 i
      | None -> line)
  |> String.concat "\n"

let contains s sub =
  let n = String.length sub in
  let rec at i =
    i + n <= String.length s
    && (String.equal (String.sub s i n) sub || at (i + 1))
  in
  at 0

let every_module_has_an_interface () =
  let missing =
    sources ".ml"
    |> List.filter (fun ml ->
        not (Sys.file_exists (Filename.concat ".." (ml ^ "i"))))
    |> List.map (fun ml -> ml ^ " has no .mli")
  in
  Alcotest.(check (list string)) "modules without an interface" [] missing

let words s =
  String.split_on_char ' '
    (String.map (function '\n' | '\t' -> ' ' | c -> c) s)
  |> List.filter (fun w -> not (String.equal w ""))

(* The library's stanza names Ptime and Mtime and no other library, and the
   package asks opam for nothing else but the compiler and dune, so [opam
   install wiretype] installs those two and nothing more. *)
let depends_on_ptime_and_mtime_alone () =
  let code = code_of_dune (read_source "src/dune") in
  let libraries =
    let marker = "(libraries" in
    let m = String.length marker in
    let rec at i =
      if i + m > String.length code then []
      else if String.equal (String.sub code i m) marker then
        let rest = String.sub code (i + m) (String.length code - i - m) in
        match String.index_opt rest ')' with
        | Some j -> words (String.sub rest 0 j)
        | None -> []
      else at (i + 1)
    in
    at 0
  in
  Alcotest.(check (list string))
    "the libraries src/dune names" [ "mtime"; "ptime" ]
    (List.sort String.compare libraries);
  (* Each line of the opam file's [depends] with no filter for a test, a
     document or a developer's setup. *)
  let opam = String.split_on_char '\n' (read_source "wiretype.opam") in
  let rec inside = function
    | [] -> []
    | l :: rest when String.starts_with ~prefix:"depends: [" l -> depends rest
    | _ :: rest -> inside rest
  and depends = function
    | [] -> []
    | l :: _ when String.starts_with ~prefix:"]" l -> []
    | l :: rest ->
        let filtered =
          List.exists (contains l) [ "with-test"; "with-doc"; "with-dev-setup" ]
        in
        let name =
          match String.split_on_char '"' l with _ :: n :: _ -> n | _ -> l
        in
        if filtered then depends rest else name :: depends rest
  in
  Alcotest.(check (list string))
    "what installing the package installs"
    [ "dune"; "mtime"; "ocaml"; "ptime" ]
    (List.sort String.compare (inside opam))

let () =
  Alcotest.run "style"
    [
      ( "libraries",
        [
          Alcotest.test_case "no banned identifier" `Quick no_banned_identifier;
          Alcotest.test_case "refusals read from OCAML.md" `Quick
            refusals_read_from_ocaml_md;
          Alcotest.test_case "no open but the named" `Quick
            no_open_but_the_named;
          Alcotest.test_case "no silenced warning" `Quick no_silenced_warning;
          Alcotest.test_case "every module has an interface" `Quick
            every_module_has_an_interface;
          Alcotest.test_case "no float with a unit" `Quick no_float_with_a_unit;
          Alcotest.test_case "every float with a unit" `Quick
            every_float_with_a_unit;
          Alcotest.test_case "no clock read bare" `Quick no_clock_read_bare;
          Alcotest.test_case "every clock read bare" `Quick
            every_clock_read_bare;
          Alcotest.test_case "depends on Ptime and Mtime alone" `Quick
            depends_on_ptime_and_mtime_alone;
        ] );
    ]
