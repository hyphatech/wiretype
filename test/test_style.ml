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

(* An identifier, dotted path included and [Stdlib.] taken off, and the
   reason it is refused. *)
let banned =
  [
    ("failwith", "errors are values: return a result");
    ("invalid_arg", "errors are values: only where the .mli documents a raise");
    ("Option.get", "partial: match on the option");
    ("Result.get_ok", "partial: match on the result");
    ("Result.get_error", "partial: match on the result");
    ("List.hd", "partial: match on the list");
    ("List.tl", "partial: match on the list");
    ("List.nth", "partial: use List.nth_opt");
    ("Obj.magic", "unsafe");
    ("compare", "polymorphic: use Int.compare, String.compare, ...");
    ("Sys.getenv", "a library reads no environment: take it as an argument");
    ("Sys.getenv_opt", "a library reads no environment: take it as an argument");
    ("Unix.getenv", "a library reads no environment: take it as an argument");
    ("Unix.environment", "a library reads no environment");
    ("exit", "a library never exits");
  ]
  @ List.map
      (fun name -> (name, "a library never prints: log through Logs"))
      [
        "print_string";
        "print_endline";
        "print_bytes";
        "print_char";
        "print_int";
        "print_float";
        "print_newline";
        "prerr_string";
        "prerr_endline";
        "prerr_bytes";
        "prerr_char";
        "prerr_int";
        "prerr_float";
        "prerr_newline";
        "stdout";
        "stderr";
        "Printf.printf";
        "Printf.eprintf";
        "Format.printf";
        "Format.eprintf";
        "Format.print_string";
        "Format.std_formatter";
        "Format.err_formatter";
      ]

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

(* Each banned identifier in [source], a file's text, as a line naming it. *)
let banned_in file source =
  identifiers (code_of source)
  |> List.map (fun (line, word) -> (line, unqualified word))
  |> List.filter (fun (_, word) ->
      not
        (List.exists
           (fun (f, w) -> String.equal f file && String.equal w word)
           allowed_banned))
  |> List.filter_map (fun (line, word) ->
      List.assoc_opt word banned
      |> Option.map (fun why ->
          Printf.sprintf "%s:%d: %s -- %s" file line word why))

let no_banned_identifier () =
  let found =
    List.concat_map
      (fun file -> banned_in file (read_source file))
      (sources ".ml" @ sources ".mli")
  in
  Alcotest.(check (list string)) "banned identifiers" [] found

(* What OCAML.md's "Tests are Alcotest" says every library refuses, word for
   word: each is refused, bare and behind [Stdlib.]. *)
let shared_refusals =
  [
    "failwith";
    "invalid_arg";
    "Option.get";
    "Result.get_ok";
    "Result.get_error";
    "List.hd";
    "List.tl";
    "List.nth";
    "Obj.magic";
    "compare";
    "Stdlib.compare";
    "Sys.getenv";
    "Sys.getenv_opt";
    "Unix.getenv";
    "Unix.environment";
    "exit";
    "print_string";
    "print_endline";
    "print_bytes";
    "print_char";
    "print_int";
    "print_float";
    "print_newline";
    "prerr_string";
    "prerr_endline";
    "prerr_bytes";
    "prerr_char";
    "prerr_int";
    "prerr_float";
    "prerr_newline";
    "stdout";
    "stderr";
    "Printf.printf";
    "Printf.eprintf";
    "Format.printf";
    "Format.eprintf";
    "Format.print_string";
    "Format.std_formatter";
    "Format.err_formatter";
  ]

let every_shared_refusal () =
  let missed =
    List.concat_map
      (fun name ->
        match String.starts_with ~prefix:"Stdlib." name with
        | true -> [ name ]
        | false -> [ name; "Stdlib." ^ name ])
      shared_refusals
    |> List.filter (fun name ->
        List.is_empty (banned_in "row.ml" ("let _ = " ^ name)))
  in
  Alcotest.(check (list string)) "names let through" [] missed

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

(* The library's stanza names no library, so [opam install wiretype]
   installs nothing else. *)
let depends_on_nothing () =
  let code = code_of_dune (read_source "src/dune") in
  Alcotest.(check bool)
    "src/dune names a library" false
    (contains code "(libraries")

let () =
  Alcotest.run "style"
    [
      ( "libraries",
        [
          Alcotest.test_case "no banned identifier" `Quick no_banned_identifier;
          Alcotest.test_case "every shared refusal" `Quick every_shared_refusal;
          Alcotest.test_case "no open but the named" `Quick
            no_open_but_the_named;
          Alcotest.test_case "no silenced warning" `Quick no_silenced_warning;
          Alcotest.test_case "every module has an interface" `Quick
            every_module_has_an_interface;
          Alcotest.test_case "depends on nothing" `Quick depends_on_nothing;
        ] );
    ]
