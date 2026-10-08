(* wiretype: reading, writing and the ready-made kinds.

   The parser's rows are JSONTestSuite's, kept in json-test-suite/ (nst/
   JSONTestSuite at 1ef36fa, its test_parsing directory, MIT): every [y_]
   file is read, every [n_] file refused, and each [i_] file -- what RFC 8259
   leaves to the implementation -- decided below with its reason. What the
   suite cannot say -- a description's problems, a bound, a union, a kind --
   is the rows after it. *)

module J = Wiretype
module P = J.Problem
module V = J.Value

(* What is written, or the line a log would say of what could not be. *)
let written shape v = Result.map_error J.Unwritable.to_string (J.encode shape v)
let dir = "json-test-suite"

let read file =
  In_channel.with_open_bin (Filename.concat dir file) In_channel.input_all

let files prefix =
  List.sort String.compare
    (List.filter
       (fun f ->
         String.starts_with ~prefix f && Filename.check_suffix f ".json")
       (Array.to_list (Sys.readdir dir)))

let accepts text = Result.is_ok (J.decode J.Value.json text)

(* Two [y_] files the suite accepts are refused by decision: a member named
   twice, which I-JSON (RFC 7493 §2.3) forbids, because two readers that keep
   different ones read two different documents. *)
let refused_by_decision =
  [ "y_object_duplicated_key.json"; "y_object_duplicated_key_and_value.json" ]

(* What RFC 8259 leaves to the implementation, decided. A number is a double,
   so one that overflows is refused rather than read as infinity, and one
   that underflows is zero; text is UTF-8, so a byte order mark, another
   encoding, a lone surrogate and bytes that are not UTF-8 are not JSON here;
   and nesting is bounded, so 500 levels are read and 100000 are not. *)
let implementation_defined =
  [
    ("i_number_double_huge_neg_exp.json", true);
    ("i_number_huge_exp.json", false);
    ("i_number_neg_int_huge_exp.json", false);
    ("i_number_pos_double_huge_exp.json", false);
    ("i_number_real_neg_overflow.json", false);
    ("i_number_real_pos_overflow.json", false);
    ("i_number_real_underflow.json", true);
    ("i_number_too_big_neg_int.json", true);
    ("i_number_too_big_pos_int.json", true);
    ("i_number_very_big_negative_int.json", true);
    ("i_object_key_lone_2nd_surrogate.json", false);
    ("i_string_1st_surrogate_but_2nd_missing.json", false);
    ("i_string_1st_valid_surrogate_2nd_invalid.json", false);
    ("i_string_UTF-16LE_with_BOM.json", false);
    ("i_string_UTF-8_invalid_sequence.json", false);
    ("i_string_UTF8_surrogate_U+D800.json", false);
    ("i_string_incomplete_surrogate_and_escape_valid.json", false);
    ("i_string_incomplete_surrogate_pair.json", false);
    ("i_string_incomplete_surrogates_escape_valid.json", false);
    ("i_string_invalid_lonely_surrogate.json", false);
    ("i_string_invalid_surrogate.json", false);
    ("i_string_invalid_utf-8.json", false);
    ("i_string_inverted_surrogates_U+1D11E.json", false);
    ("i_string_iso_latin_1.json", false);
    ("i_string_lone_second_surrogate.json", false);
    ("i_string_lone_utf8_continuation_byte.json", false);
    ("i_string_not_in_unicode_range.json", false);
    ("i_string_overlong_sequence_2_bytes.json", false);
    ("i_string_overlong_sequence_6_bytes.json", false);
    ("i_string_overlong_sequence_6_bytes_null.json", false);
    ("i_string_truncated-utf-8.json", false);
    ("i_string_utf16BE_no_BOM.json", false);
    ("i_string_utf16LE_no_BOM.json", false);
    ("i_structure_500_nested_arrays.json", true);
    ("i_structure_UTF-8_BOM_empty_object.json", false);
  ]

let suite_rows () =
  let check expected file =
    Alcotest.(check bool) file expected (accepts (read file))
  in
  List.iter
    (fun f -> check (not (List.mem f refused_by_decision)) f)
    (files "y_");
  List.iter (check false) (files "n_");
  let decided = files "i_" in
  List.iter
    (fun f ->
      match List.assoc_opt f implementation_defined with
      | Some expected -> check expected f
      | None -> Alcotest.failf "%s is not decided" f)
    decided;
  Alcotest.(check int)
    "every i_ file is decided, and no other" (List.length decided)
    (List.length implementation_defined)

(* A number that overflows is refused, not read as infinity. *)
let overflow () =
  let code shape text =
    match J.decode shape text with
    | Ok _ -> []
    | Error ps -> List.map (fun (p : P.t) -> P.code_to_string p.code) ps
  in
  List.iter
    (fun (name, expected, found) ->
      Alcotest.(check (list string)) name [ expected ] found)
    [
      ("any JSON past a double", "too_large", code J.Value.json "1e400");
      ("an int past a double", "too_large", code J.int "1e400");
      ("an int past a double below", "too_small", code J.int "-1e400");
      ("with a fraction", "too_large", code J.int "1.0e400");
      ("an int64 past a double", "too_large", code J.int64 "1e400");
      ("an int64 past a double below", "too_small", code J.int64 "-1e400");
    ]

(* A path is one line however its names are spelt, and two paths are never
   spelt alike: a name that is not plain is written quoted. *)
let paths () =
  List.iter
    (fun (expected, at) ->
      Alcotest.(check string) expected expected (P.path at))
    [
      ("items[2].count", [ P.Member "items"; Index 2; Member "count" ]);
      ("scores.purple[name]", [ Member "scores"; Name "purple" ]);
      ("größe", [ Member "größe" ]);
      ({|["a.b"]|}, [ Member "a.b" ]);
      ("a.b", [ Member "a"; Member "b" ]);
      ({|[""].b|}, [ Member ""; Member "b" ]);
      ({|["a\nb"]|}, [ Member "a\nb" ]);
      ({|x["[0]"]|}, [ Member "x"; Member "[0]" ]);
      ({|["a b"][name]|}, [ Name "a b" ]);
      ("", []);
    ];
  Alcotest.(check string)
    "from a root" "body[2]"
    (P.path ~root:"body" [ Index 2 ]);
  Alcotest.(check string)
    "a member from a root" "body.a"
    (P.path ~root:"body" [ Member "a" ]);
  match J.decode J.Value.json {|{"a\nb": 1, "a\nb": 2}|} with
  | Ok _ -> Alcotest.fail "a member given twice is refused"
  | Error ps ->
      Alcotest.(check string)
        "one line for a log"
        {|["a\nb"]: This member is given more than once. (repeated_member)|}
        (P.list_to_string ps)

(* ------------------------------------------------------------------ *)
(* A description *)

type move = { x : int; y : int; note : string option; tags : string list }

let move =
  J.Object.map ~kind:"move" (fun x y note tags -> { x; y; note; tags })
  |> J.Object.mem "x" (J.int_bounded ~min:0 ~max:18 ()) ~enc:(fun m -> m.x)
  |> J.Object.mem "y" (J.int_bounded ~min:0 ~max:18 ()) ~enc:(fun m -> m.y)
  |> J.Object.opt_mem "note" (J.string_bounded ~max_length:5 ()) ~enc:(fun m ->
      m.note)
  |> J.Object.mem "tags" (J.list ~max_items:2 J.string) ~absent:[]
       ~enc:(fun m -> m.tags)
  |> J.Object.finish

let problems text shape =
  match J.decode shape text with
  | Ok _ -> []
  | Error ps ->
      List.map (fun (p : P.t) -> (P.path p.at, P.code_to_string p.code)) ps

let problem = Alcotest.(pair string string)

let every_problem () =
  Alcotest.(check (list problem))
    "every problem, in document order"
    [
      ("y", "too_large");
      ("note", "unexpected_type");
      ("tags[1]", "unexpected_type");
      ("tags", "too_many");
    ]
    (problems {|{"x": 3, "y": 19, "note": 7, "tags": ["a", 2, "c"]}|} move);
  Alcotest.(check (list problem))
    "a missing member is said once the object has ended"
    [ ("y", "required") ]
    (problems {|{"x": 3}|} move);
  Alcotest.(check (list problem))
    "a string past its length, in code points"
    [ ("note", "too_long") ]
    (problems {|{"x": 0, "y": 0, "note": "éééééé"}|} move);
  Alcotest.(check (list problem))
    "five code points are five" []
    (problems {|{"x": 0, "y": 0, "note": "ééééé"}|} move);
  Alcotest.(check (list problem))
    "a member given twice"
    [ ("x", "repeated_member") ]
    (problems {|{"x": 1, "y": 2, "x": 3}|} move);
  Alcotest.(check (list problem))
    "twice, once spelt with an escape"
    [ ("x", "repeated_member") ]
    (problems {|{"x": 1, "y": 2, "\u0078": 3}|} move);
  Alcotest.(check (list problem))
    "a member it does not describe, twice"
    [ ("z", "repeated_member") ]
    (problems {|{"x": 1, "y": 2, "z": 1, "z": 2}|} move);
  Alcotest.(check (list problem))
    "a name past ASCII is read as any text"
    [ ("é", "repeated_member") ]
    (problems {|{"x": 1, "y": 2, "é": 1, "\u00e9": 2}|} move);
  Alcotest.(check (list problem))
    "a syntax error ends reading, at its place"
    [ ("tags", "syntax") ]
    (problems {|{"x": 1, "y": 2, "tags": ["a" "b"]}|} move);
  (* Text that is not JSON is that one problem alone, whatever was wrong
     before it: a value that is not JSON has no sort to be the wrong one. *)
  List.iter
    (fun (text, at) ->
      Alcotest.(check (list problem))
        (Printf.sprintf "%s is a syntax error alone" text)
        [ (at, "syntax") ]
        (problems text move))
    [
      ("nope", "");
      ("tru", "");
      ("fals", "");
      ("xyz", "");
      ({|"abc|}, "");
      ("[1,", "[1]");
      ({|{"x": nope, "y": 2}|}, "x");
      ({|{"x": "a", "y": 2, "tags": nope}|}, "tags");
      ({|{"x": 0, "y": 0, "note": {"a": 1, "a": 2}, "z": nope}|}, "z");
      ({|{"x": 0, "y": 0, "x": nope}|}, "x");
      ({|{"x": "a", "y": 2} x|}, "");
    ];
  Alcotest.(check (list problem))
    "in a list too"
    [ ("[2]", "syntax") ]
    (problems {|["a", 2, nope]|} (J.list J.string));
  Alcotest.(check (list string))
    "and nested past the limit, that one problem alone" [ "too_deep" ]
    (List.map snd
       (problems
          ({|{"x": "a", "y": 0, "note": |} ^ String.make 600 '['
         ^ String.make 600 ']' ^ "}")
          move));
  Alcotest.(check (list problem))
    "a value of the wrong sort before what is wrong inside it"
    [ ("note", "unexpected_type"); ("note.a", "repeated_member") ]
    (problems {|{"x": 0, "y": 0, "note": {"a": 1, "a": 2}}|} move);
  Alcotest.(check (list problem))
    "and not JSON inside it, a syntax error alone"
    [ ("note[1]", "syntax") ]
    (problems {|{"x": 0, "y": 0, "note": [1, nope]}|} move);
  let written_only = J.map ~enc:Fun.id J.int in
  Alcotest.(check (list problem))
    "a description made only to write"
    [ ("", "malformed") ]
    (problems "1" written_only);
  Alcotest.(check (list problem))
    "and not JSON where it is read, a syntax error alone"
    [ ("", "syntax") ]
    (problems "nope" written_only)

let absent_and_null () =
  let read text =
    match J.decode move text with Ok m -> m.note | Error _ -> Some "(refused)"
  in
  Alcotest.(check (option string)) "absent" None (read {|{"x":1,"y":2}|});
  Alcotest.(check (option string))
    "null" None
    (read {|{"x":1,"y":2,"note":null}|});
  Alcotest.(check (option string))
    "given" (Some "hi")
    (read {|{"x":1,"y":2,"note":"hi"}|});
  Alcotest.(check (result string string))
    "None is left out, a default written" (Ok {|{"x":1,"y":2,"tags":[]}|})
    (written move { x = 1; y = 2; note = None; tags = [] });
  let nullable =
    J.Object.map (fun n -> n)
    |> J.Object.mem "n" (J.nullable J.int) ~enc:Fun.id
    |> J.Object.finish
  in
  Alcotest.(check (result string string))
    "a nullable member writes null" (Ok {|{"n":null}|}) (written nullable None);
  Alcotest.(check (list problem))
    "and must be there"
    [ ("n", "required") ]
    (problems "{}" nullable)

let unknown () =
  let strict =
    J.Object.map (fun a -> a)
    |> J.Object.mem "a" J.int ~enc:Fun.id
    |> J.Object.error_unknown |> J.Object.finish
  in
  Alcotest.(check (list problem))
    "skipped by default" []
    (problems {|{"x":1,"y":2,"z":{"deep":[1,2]}}|} move);
  Alcotest.(check (list problem))
    "refused when asked"
    [ ("b", "unknown_member") ]
    (problems {|{"a":1,"b":2}|} strict);
  Alcotest.(check (list problem))
    "before what is wrong inside it"
    [ ("b", "unknown_member"); ("b.c", "repeated_member") ]
    (problems {|{"a":1,"b":{"c":1,"c":2}}|} strict);
  Alcotest.(check (list problem))
    "and not JSON, a syntax error alone"
    [ ("b", "syntax") ]
    (problems {|{"a":1,"b":nope}|} strict)

(* A value, a step, and whether the value is a multiple of it: the same rows
   are kinds.test.ts's, which runs zod's multipleOf over them. *)
let multiple_rows =
  [
    ("0", 0.01, true);
    ("-19.99", 0.01, true);
    ("19.99", 0.01, true);
    ("0.3", 0.1, true);
    ("2.03", 0.07, true);
    ("0.35", 0.1, false);
    ("1e-7", 1e-8, true);
    ("10", 2.5, true);
    ("7", 2.5, false);
    ("123456789.12", 0.01, true);
    ("0.1", 0.3, false);
  ]

(* A whole number, a step, and whether the one is a multiple of the other,
   exactly: the rows of kinds.test.ts's check the schema prints for them. *)
let int_multiple_rows =
  [
    ("0", 3, true);
    ("-9", 3, true);
    ("10", 3, false);
    ("3000000000000000", 3, true);
    ("3000000000000001", 3, false);
    ("9007199254740991", 7, false);
  ]

let numbers () =
  List.iter
    (fun (text, step, expected) ->
      Alcotest.(check bool)
        (Printf.sprintf "%s a multiple of %g" text step)
        expected
        (Result.is_ok (J.decode (J.number_bounded ~multiple_of:step ()) text)))
    multiple_rows;
  List.iter
    (fun (text, step, expected) ->
      Alcotest.(check bool)
        (Printf.sprintf "%s a multiple of %d" text step)
        expected
        (Result.is_ok (J.decode (J.int_bounded ~multiple_of:step ()) text)))
    int_multiple_rows;
  let ok shape text = J.decode shape text in
  Alcotest.(check (result int reject))
    "an integer past 2^53, exactly" (Ok 9007199254740993)
    (ok J.int "9007199254740993");
  Alcotest.(check (result int reject))
    "a whole number with a fraction" (Ok 2) (ok J.int "2.0");
  Alcotest.(check (result int reject))
    "with an exponent" (Ok 1000) (ok J.int "1e3");
  Alcotest.(check (result int64 reject))
    "int64 exactly" (Ok 9223372036854775807L)
    (ok J.int64 "9223372036854775807");
  Alcotest.(check (list problem))
    "a fraction is not an integer"
    [ ("", "unexpected_type") ]
    (problems "1.5" J.int);
  Alcotest.(check (list problem))
    "nor text"
    [ ("", "unexpected_type") ]
    (problems {|"0x1F"|} J.int);
  Alcotest.(check (list problem))
    "past an int"
    [ ("", "too_large") ]
    (problems "4611686018427387904" J.int);
  Alcotest.(check (list problem))
    "a multiple"
    [ ("", "not_a_multiple") ]
    (problems "7" (J.int_bounded ~multiple_of:5 ()));
  Alcotest.(check (list problem))
    "greater than, exclusively"
    [ ("", "too_small") ]
    (problems "0" (J.number_bounded ~above:0. ()));
  Alcotest.(check (list problem))
    "null is not a number"
    [ ("", "unexpected_type") ]
    (problems "null" J.number)

let strings () =
  Alcotest.(check (result string reject))
    "escapes and a surrogate pair" (Ok "a\"\\/\b\012\n\r\t\xf0\x9d\x84\x9e")
    (J.decode J.string {|"a\"\\\/\b\f\n\r\t𝄞"|});
  Alcotest.(check (list problem))
    "a lone surrogate"
    [ ("", "syntax") ]
    (problems {|"\ud800"|} J.string);
  Alcotest.(check (list problem))
    "bytes that are not UTF-8"
    [ ("", "syntax") ]
    (problems "\"\xff\"" J.string);
  Alcotest.(check (list problem))
    "a raw control character"
    [ ("", "syntax") ]
    (problems "\"a\nb\"" J.string);
  Alcotest.(check (result reject string))
    "a bad escape is said at its first digit"
    (Error
       "the document: This is not JSON: a hexadecimal digit was expected at \
        byte 3, and 'Z' was found. (syntax)")
    (Result.map_error P.list_to_string (J.decode J.string {|"\uZZZZ"|}))

let nesting () =
  let deep n = String.make n '[' ^ String.make n ']' in
  Alcotest.(check bool)
    "512 levels" true
    (Result.is_ok (J.decode J.Value.json (deep 512)));
  Alcotest.(check (list problem))
    "513 are too deep"
    [ (String.concat "" (List.init 512 (fun _ -> "[0]")), "too_deep") ]
    (problems (deep 513) J.Value.json);
  Alcotest.(check (list string))
    "a limit given past the ceiling is the ceiling" [ "too_deep" ]
    (match J.decode ~max_depth:max_int J.Value.json (deep 1_000_000) with
    | Ok _ -> []
    | Error ps ->
        List.map (fun (p : J.Problem.t) -> J.Problem.code_to_string p.code) ps)

type colour = Black | White

let colour =
  J.enum ~kind:"colour"
    (function Black -> "black" | White -> "white")
    [ Black; White ]

let enums () =
  Alcotest.(check (result string string))
    "written as its word" (Ok {|"white"|}) (written colour White);
  Alcotest.(check (list problem))
    "a word it does not have"
    [ ("", "unknown_word") ]
    (problems {|"red"|} colour)

(* A move is [{"pass": true}], or a point with [pass] left out. *)
type play = Pass | Play of int * int

let point =
  J.Object.map (fun row col -> (row, col))
  |> J.Object.mem "row" J.int ~enc:fst
  |> J.Object.mem "col" J.int ~enc:snd
  |> J.Object.finish

let passed =
  J.Object.Case.map true
    (J.Object.map () |> J.Object.finish)
    ~dec:(fun () -> Pass)

let played = J.Object.Case.map false point ~dec:(fun (r, c) -> Play (r, c))

type turn = { by : string; play : play }

let turn =
  J.Object.map ~kind:"turn" (fun by play -> { by; play })
  |> J.Object.mem "by" J.string ~enc:(fun t -> t.by)
  |> J.Object.case_mem "pass" J.bool ~absent:false ~omit:not
       ~enc:(fun t -> t.play)
       ~enc_case:(function
         | Pass -> J.Object.Case.value passed ()
         | Play (r, c) -> J.Object.Case.value played (r, c))
       J.Object.Case.[ make passed; make played ]
  |> J.Object.finish

let unions () =
  let read text = Result.map (fun t -> t.play) (J.decode turn text) in
  let equal a b =
    match (a, b) with
    | Pass, Pass -> true
    | Play (r, c), Play (r', c') -> r = r' && c = c'
    | (Pass | Play _), _ -> false
  in
  let pp ppf = function
    | Pass -> Format.pp_print_string ppf "pass"
    | Play (r, c) -> Format.fprintf ppf "(%d,%d)" r c
  in
  let play = Alcotest.testable pp equal in
  Alcotest.(check (result play reject))
    "the tag after what it decides"
    (Ok (Play (3, 4)))
    (read {|{"row":3,"by":"b","col":4}|});
  Alcotest.(check (result play reject))
    "a tag that is there" (Ok Pass)
    (read {|{"by":"w","pass":true}|});
  Alcotest.(check (list problem))
    "a case's missing member"
    [ ("col", "required") ]
    (problems {|{"by":"b","row":1}|} turn);
  Alcotest.(check (list problem))
    "a tag no case has"
    [ ("pass", "unknown_word") ]
    (problems {|{"by":"b","pass":"yes"}|} turn);
  Alcotest.(check (result string string))
    "the tag first, then the rest" (Ok {|{"pass":true,"by":"w"}|})
    (written turn { by = "w"; play = Pass });
  Alcotest.(check (result string string))
    "a tag left out when it may be" (Ok {|{"by":"b","row":1,"col":2}|})
    (written turn { by = "b"; play = Play (1, 2) });
  let note =
    J.Object.Case.map "note"
      (J.Object.map Fun.id
      |> J.Object.mem "data" J.Value.json ~enc:Fun.id
      |> J.Object.error_unknown |> J.Object.finish)
      ~dec:Fun.id
  in
  let event =
    J.Object.map Fun.id
    |> J.Object.case_mem "type" J.string ~enc:Fun.id
         ~enc_case:(J.Object.Case.value note)
         [ J.Object.Case.make note ]
    |> J.Object.finish
  in
  Alcotest.(check (list problem))
    "a member given twice inside a case's member, said once"
    [ ("data.x", "repeated_member") ]
    (problems {|{"type":"note","data":{"x":1,"x":2}}|} event);
  Alcotest.(check (list problem))
    "and inside a member nobody describes, said once, after it"
    [ ("z", "unknown_member"); ("z.x", "repeated_member") ]
    (problems {|{"type":"note","data":1,"z":{"x":1,"x":2}}|} event);
  Alcotest.(check (list problem))
    "a member nobody describes that is not JSON, a syntax error alone"
    [ ("z", "syntax") ]
    (problems {|{"type":"note","data":1,"z":nope}|} event);
  Alcotest.(check (list problem))
    "and whatever was wrong before it, as in a plain object"
    [ ("by", "syntax") ]
    (problems {|{"type":"note","data":{"x":1,"x":2},"by":nope}|} event);
  Alcotest.(check (list problem))
    "a case that refuses a member it does not describe"
    [ ("z", "unknown_member") ]
    (problems {|{"type":"note","data":1,"z":2}|} event);
  Alcotest.(check (list problem))
    "a case's member given twice"
    [ ("data", "repeated_member") ]
    (problems {|{"type":"note","data":1,"data":2}|} event);
  Alcotest.(check (list problem))
    "the tag given twice"
    [ ("type", "repeated_member") ]
    (problems {|{"type":"note","type":"note","data":1}|} event);
  Alcotest.(check (list problem))
    "the tag given twice, the second unlike the first"
    [ ("type", "repeated_member") ]
    (problems {|{"type":"note","data":1,"type":"x"}|} event);
  (* Each member was said again with every problem before it, which took
     40,000 members to 23 seconds. *)
  let n = 50_000 in
  let many =
    {|{"type":"note","data":1|}
    ^ String.concat "" (List.init n (fun _ -> {|,"z":1|}))
    ^ "}"
  in
  let start = Sys.time () in
  let found = List.length (problems many event) in
  Alcotest.(check int) "every member nobody describes" n found;
  Alcotest.(check bool) "read in linear time" true (Sys.time () -. start < 1.)

(* A description that can mean nothing is refused where it is built. *)
(* A value read and written through any JSON, a tuple of four, an object
   that is only written, and the small pieces a caller writing JSON by hand
   reaches for. *)
let values () =
  let m = { x = 1; y = 2; note = None; tags = [ "a" ] } in
  (match J.to_value move m with
  | Error e -> Alcotest.fail (J.Unwritable.to_string e)
  | Ok v -> (
      Alcotest.(check string)
        "as the JSON it is written as" {|{"x":1,"y":2,"tags":["a"]}|}
        (V.to_string v);
      Alcotest.(check (option string))
        "a member found" (Some {|["a"]|})
        (Option.map V.to_string (V.find "tags" v));
      Alcotest.(check (option string))
        "and one that is not there" None
        (Option.map V.to_string (V.find "note" v));
      match J.of_value move v with
      | Ok back -> Alcotest.(check int) "and read back" 2 back.y
      | Error ps -> Alcotest.fail (P.list_to_string ps)));
  Alcotest.(check (list problem))
    "read from a value, every problem"
    [ ("x", "too_large") ]
    (match
       J.of_value move (V.Object [ ("x", V.Number 19.); ("y", V.Number 0.) ])
     with
    | Ok _ -> []
    | Error ps ->
        List.map (fun (p : P.t) -> (P.path p.at, P.code_to_string p.code)) ps);
  let four = J.tuple4 J.int J.string J.bool J.number in
  Alcotest.(check (result string string))
    "a tuple of four" (Ok {|[1,"a",true,2.5]|})
    (written four (1, "a", true, 2.5));
  Alcotest.(check (list problem))
    "of another length"
    [ ("", "too_few") ]
    (problems {|[1,"a",true]|} four);
  let sent = J.Object.(enc_only () |> mem "a" J.int ~enc:Fun.id |> finish) in
  Alcotest.(check (result string string))
    "an object only written" (Ok {|{"a":1}|}) (written sent 1);
  Alcotest.(check (list problem))
    "and never read"
    [ ("", "malformed") ]
    (problems {|{"a":1}|} sent);
  Alcotest.(check string)
    "text quoted as a document has it" {|"a\u0001\"b\u007F"|}
    (J.Text.quote "a\x01\"b\x7f");
  Alcotest.(check (list problem))
    "a limit of none"
    [ ("", "too_deep") ]
    (match J.decode ~max_depth:0 J.Value.json "[]" with
    | Ok _ -> []
    | Error ps ->
        List.map (fun (p : P.t) -> (P.path p.at, P.code_to_string p.code)) ps);
  Alcotest.(check bool)
    "but a scalar" true
    (Result.is_ok (J.decode ~max_depth:0 J.Value.json "1"));
  Alcotest.(check bool)
    "a limit of two" true
    (Result.is_ok (J.decode ~max_depth:2 J.Value.json "[[]]")
    && Result.is_error (J.decode ~max_depth:2 J.Value.json "[[[]]]"))

type nest = Nest of nest list

let names () =
  let rec nest =
    lazy
      (J.map
         ~dec:(fun l -> Nest l)
         ~enc:(fun (Nest l) -> l)
         (J.list (J.rec' nest)))
  in
  List.iter
    (fun (expected, name) -> Alcotest.(check string) expected expected name)
    [
      ("integer", J.name J.int);
      ("list of string or null", J.name (J.list (J.nullable J.string)));
      ("map of uuid", J.name (J.dict J.string (J.uuid ())));
      ("colour", J.name colour);
      ("list of itself", J.name (J.rec' nest));
    ]

let malformed_descriptions () =
  let refused name message f =
    Alcotest.check_raises name (Invalid_argument message) (fun () ->
        ignore (f ()))
  in
  let obj () = J.Object.map (fun a -> a) in
  let case tag =
    J.Object.Case.map tag (J.Object.map () |> J.Object.finish) ~dec:Fun.id
  in
  refused "a multiple of zero"
    "Wiretype.int_bounded: multiple_of is 0, and must be positive" (fun () ->
      J.int_bounded ~multiple_of:0 ());
  refused "a negative multiple"
    "Wiretype.int64_bounded: multiple_of is -5, and must be positive" (fun () ->
      J.int64_bounded ~multiple_of:(-5L) ());
  refused "a multiple that is not a number"
    "Wiretype.number_bounded: multiple_of is nan, and must be positive"
    (fun () -> J.number_bounded ~multiple_of:Float.nan ());
  refused "a bound no number meets"
    "Wiretype.number_bounded: max is -inf, which no number meets" (fun () ->
      J.number_bounded ~max:Float.neg_infinity ());
  refused "nor a number above"
    "Wiretype.number_bounded: above is inf, which no number meets" (fun () ->
      J.number_bounded ~above:Float.infinity ());
  refused "a bound that is no number"
    "Wiretype.number_bounded: min is nan, which no number meets" (fun () ->
      J.number_bounded ~min:Float.nan ());
  Alcotest.(check bool)
    "but one every number meets is none" true
    (Result.is_ok
       (J.decode
          (J.number_bounded ~min:Float.neg_infinity ~max:Float.infinity
             ~above:Float.neg_infinity ~below:Float.infinity ())
          "-1e308"));
  refused "two values, one word" "Wiretype.enum: two values are written \"a\""
    (fun () -> J.enum (fun _ -> "a") [ 1; 2 ]);
  refused "a member described twice"
    "Wiretype.Object.finish: the member \"a\" is described twice" (fun () ->
      J.Object.map (fun a _ -> a)
      |> J.Object.mem "a" J.int ~enc:Fun.id
      |> J.Object.opt_mem "a" J.int ~enc:Option.some
      |> J.Object.finish);
  refused "a member that is the union's tag"
    "Wiretype.Object.finish: the member \"t\" is described twice" (fun () ->
      J.Object.map (fun _ c -> c)
      |> J.Object.mem "t" J.int ~enc:(fun _ -> 0)
      |> J.Object.case_mem "t" J.string ~enc:Fun.id
           ~enc_case:(fun () -> J.Object.Case.value (case "a") ())
           [ J.Object.Case.make (case "a") ]
      |> J.Object.finish);
  refused "a member the object and its case both describe"
    "Wiretype.Object.finish: the member \"a\" is described twice: by the \
     object and by its case \"c\"" (fun () ->
      let c =
        J.Object.Case.map "c"
          (obj () |> J.Object.mem "a" J.int ~enc:Fun.id |> J.Object.finish)
          ~dec:Fun.id
      in
      J.Object.map (fun _ c -> c)
      |> J.Object.mem "a" J.int ~enc:Fun.id
      |> J.Object.case_mem "t" J.string ~enc:Fun.id
           ~enc_case:(J.Object.Case.value c)
           [ J.Object.Case.make c ]
      |> J.Object.finish);
  refused "two cases tagged alike"
    "Wiretype.Object.finish: two cases of the union \"t\" are tagged \"a\""
    (fun () ->
      obj ()
      |> J.Object.case_mem "t" J.string ~enc:Fun.id
           ~enc_case:(fun () -> J.Object.Case.value (case "a") ())
           [ J.Object.Case.make (case "a"); J.Object.Case.make (case "a") ]
      |> J.Object.finish);
  refused "two unions"
    "Wiretype.Object.finish: \"t1\" and \"t2\" are each a union's tag, and an \
     object has one union" (fun () ->
      J.Object.map (fun a _ -> a)
      |> J.Object.case_mem "t1" J.string ~enc:Fun.id
           ~enc_case:(fun () -> J.Object.Case.value (case "a") ())
           [ J.Object.Case.make (case "a") ]
      |> J.Object.case_mem "t2" J.string ~enc:Fun.id
           ~enc_case:(fun () -> J.Object.Case.value (case "b") ())
           [ J.Object.Case.make (case "b") ]
      |> J.Object.finish);
  refused "a case with a union of its own"
    "Wiretype.Object.Case.map: a case is an object with no union of its own, \
     since an object has one" (fun () ->
      J.Object.Case.map "outer"
        (obj ()
        |> J.Object.case_mem "inner" J.string ~enc:Fun.id
             ~enc_case:(fun () -> J.Object.Case.value (case "a") ())
             [ J.Object.Case.make (case "a") ]
        |> J.Object.finish)
        ~dec:Fun.id);
  refused "a case that is no object"
    "Wiretype.Object.Case.map: a case is an object" (fun () ->
      J.Object.Case.map "a" J.int ~dec:Fun.id);
  refused "a case that is the union it is a case of"
    "Wiretype.Object.Case.map: a case is an object" (fun () ->
      let rec u =
        lazy
          (obj ()
          |> J.Object.case_mem "t" J.string ~enc:Fun.id
               ~enc_case:(fun () -> J.Object.Case.value (case "a") ())
               [
                 J.Object.Case.make
                   (J.Object.Case.map "b" (J.rec' u) ~dec:Fun.id);
               ]
          |> J.Object.finish)
      in
      Lazy.force u);
  refused "a tag that cannot be written"
    "Wiretype.Object.finish: a tag of the union \"t\": This value is read and \
     never written." (fun () ->
      obj ()
      |> J.Object.case_mem "t"
           (J.map ~dec:Fun.id J.string)
           ~enc:Fun.id
           ~enc_case:(fun () -> J.Object.Case.value (case "a") ())
           [ J.Object.Case.make (case "a") ]
      |> J.Object.finish);
  refused "a member left out that is read as required"
    "Wiretype.Object.finish: the member \"a\" is left out by omit and has no \
     absent to be read as" (fun () ->
      obj () |> J.Object.mem "a" J.int ~omit:(Int.equal 0) |> J.Object.finish);
  refused "a tag left out that is read as required"
    "Wiretype.Object.finish: the member \"t\" is left out by omit and has no \
     absent to be read as" (fun () ->
      obj ()
      |> J.Object.case_mem "t" J.string ~omit:(String.equal "a") ~enc:Fun.id
           ~enc_case:(fun () -> J.Object.Case.value (case "a") ())
           [ J.Object.Case.make (case "a") ]
      |> J.Object.finish);
  Alcotest.(check (result string string))
    "but one that is never read may leave out what it likes" (Ok "{}")
    (written
       (J.Object.enc_only ()
       |> J.Object.mem "a" J.int ~enc:Fun.id ~omit:(Int.equal 0)
       |> J.Object.finish)
       0)

(* ------------------------------------------------------------------ *)
(* The ready-made kinds *)

(* Each row is a string, whether the server takes it, and whether the
   browser's zod check does; the same rows are written out in the web suite's
   kinds.test.ts, which runs zod over them. The two answers are equal but
   where the browser is the looser -- a URI, an instant past the years 0000
   to 9999 once in UTC, a duration past a hundred thousand years -- so the
   server never takes what the browser refuses. *)
let instant_rows =
  [
    ("2026-09-30T12:00:00Z", true, true);
    ("2026-09-30T12:00:00.5Z", true, true);
    ("2026-09-30T12:00:00.123456789+05:30", true, true);
    ("0000-01-01T00:00:00Z", true, true);
    ("2024-02-29T23:59:59-23:59", true, true);
    ("9999-12-31T23:59:59.999Z", true, true);
    ("9999-12-31T23:59:59-00:01", false, true);
    ("0000-01-01T00:00:00+00:01", false, true);
    ("2026-09-30t12:00:00Z", false, false);
    ("2026-09-30T12:00:00z", false, false);
    ("2026-09-30T12:00Z", false, false);
    ("2026-09-30T12:00:60Z", false, false);
    ("2026-02-29T00:00:00Z", false, false);
    ("2026-09-30T24:00:00Z", false, false);
    ("2026-09-30T12:00:00", false, false);
    ("2026-09-30T12:00:00+24:00", false, false);
    ("2026-09-30 12:00:00Z", false, false);
    ("2026-09-30T12:00:00.Z", false, false);
  ]

let date_rows =
  [
    ("2026-09-30", true, true);
    ("2024-02-29", true, true);
    ("2000-02-29", true, true);
    ("0000-02-29", true, true);
    ("1900-02-29", false, false);
    ("2026-02-29", false, false);
    ("2026-13-01", false, false);
    ("2026-9-30", false, false);
    ("20260930", false, false);
  ]

let duration_rows =
  [
    ("PT1H30M", true, true);
    ("P1W", true, true);
    ("P1D", true, true);
    ("P1DT2H", true, true);
    ("PT0.5S", true, true);
    ("PT1,5S", true, true);
    ("PT0S", true, true);
    ("P1DT1H1M1.001S", true, true);
    ("P1Y", false, false);
    ("P1M", false, false);
    ("P1Y2M", false, false);
    ("PT", false, false);
    ("P", false, false);
    ("P1W1D", false, false);
    ("1D", false, false);
    ("PT1.5M", false, false);
    ("P1DT", false, false);
    ("P36525000D", true, true);
    ("P36525001D", false, true);
    ("P99999999999D", false, true);
    ("P1000000000000W", false, true);
    ("PT3000000000000000H", false, true);
    ("PT9999999999999999S", false, true);
  ]

let uuid_rows =
  [
    ("01890a5d-ac96-7a3b-9e5a-5f1c2a7b8c9d", true, true);
    ("01890A5D-AC96-7A3B-9E5A-5F1C2A7B8C9D", true, true);
    ("f47ac10b-58cc-4372-a567-0e02b2c3d479", true, true);
    ("00000000-0000-0000-0000-000000000000", true, true);
    ("ffffffff-ffff-ffff-ffff-ffffffffffff", true, true);
    ("FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF", false, false);
    ("01890a5d-ac96-0a3b-9e5a-5f1c2a7b8c9d", false, false);
    ("01890a5d-ac96-7a3b-ce5a-5f1c2a7b8c9d", false, false);
    ("01890a5dac96-7a3b-9e5a-5f1c2a7b8c9d", false, false);
    ("01890a5d-ac96-7a3b-9e5a-5f1c2a7b8c9", false, false);
  ]

let uuid7_rows =
  [
    ("01890a5d-ac96-7a3b-9e5a-5f1c2a7b8c9d", true, true);
    ("f47ac10b-58cc-4372-a567-0e02b2c3d479", false, false);
    ("00000000-0000-0000-0000-000000000000", false, false);
  ]

let base64_rows =
  [
    ("", true, true);
    ("QQ==", true, true);
    ("QUI=", true, true);
    ("QUJD", true, true);
    ("QR==", true, true);
    ("QQ", false, false);
    ("QQ=", false, false);
    ("Q===", false, false);
    ("QU JD", false, false);
    ("QUJD=", false, false);
    ("-_8=", false, false);
  ]

let base64url_rows =
  [
    ("", true, true);
    ("QQ", true, true);
    ("QUI", true, true);
    ("QUJD", true, true);
    ("-_8", true, true);
    ("QQ==", false, false);
    ("Q", false, false);
    ("QU+D", false, false);
  ]

let uri_rows =
  [
    ("https://example.com/path?q=1#frag", true, true);
    ("mailto:a@b.c", true, true);
    ("urn:isbn:0451450523", true, true);
    ("http://[::1]:8080/", true, true);
    ("http://[::ffff:192.0.2.1]/", true, true);
    ("ftp://user:pw@host/x", true, true);
    ("https://example.com:65535", true, true);
    ("https://192.0.2.1/", true, true);
    ("foo:bar", true, true);
    ("example.com", false, false);
    ("https://exa mple.com", false, false);
    ("http://", false, false);
    ("https:", false, false);
    ("https://example.com:65536", false, false);
    ("http://[v1.fe]/", false, false);
    ("https://999.1.1.1/", false, false);
    (* The browser is the looser: the URL standard takes these, RFC 3986 or
       our reading of a special scheme's host does not. *)
    ("https://example.com/a b", false, true);
    ("http:example.com", false, true);
    ("https://xn--nxasmq6b.com/", true, true);
    ("https://xn--mnchen-3ya.de/", true, true);
    ("https://xn--ls8h.la/", true, true);
    (* A browser's answer to these three is its runtime's IDNA to decide,
       null in kinds.test.ts; the rule holds whichever it is. *)
    ("https://xn--zz.com/", false, false);
    ("https://xn--abc-.com/", false, false);
    ("https://xn--xn--a--gua.pt/", false, false);
    ("file:///etc/hosts", true, true);
    ("file://host/x", true, true);
    ("file://host:80/x", false, false);
    ("file://u@host/x", false, false);
    ("file://1.2.3.999/", false, false);
    ("foo://", true, true);
    ("foo://:80", false, false);
    ("foo://u@", false, false);
    ("foo://u@:1", false, false);
    ("http://[1.2.3.4::]/", false, false);
  ]

let ipv4_rows =
  [
    ("192.0.2.1", true, true);
    ("0.0.0.0", true, true);
    ("255.255.255.255", true, true);
    ("256.0.0.1", false, false);
    ("01.2.3.4", false, false);
    ("1.2.3", false, false);
    ("1.2.3.4.5", false, false);
    (" 1.2.3.4", false, false);
  ]

let ipv6_rows =
  [
    ("::", true, true);
    ("::1", true, true);
    ("2001:db8::1", true, true);
    ("1:2:3:4:5:6:7:8", true, true);
    ("1:2:3:4:5:6:7::", true, true);
    ("::2:3:4:5:6:7:8", true, true);
    ("FE80::1", true, true);
    ("1:2:3:4:5:6:7:8:9", false, false);
    (":::", false, false);
    ("1::2::3", false, false);
    ("::ffff:192.0.2.1", true, true);
    ("::ffff:192.0.2.256", false, false);
    ("fe80::1%eth0", false, false);
    ("12345::", false, false);
    ("1:2:3:4:5:6:7", false, false);
    ("1.2.3.4::", false, false);
    ("1.2.3.4::1", false, false);
    ("1:1.2.3.4::", false, false);
    ("1:2:3:4:5:6:1.2.3.4", true, true);
    ("1:2:3:4:5:6:7:1.2.3.4", false, false);
  ]

(* A label is decoded in time that grows with the square of its length, so
   one longer than DNS allows, 63 octets, is refused before it is decoded. *)
let long_label () =
  let start = Sys.time () in
  let uri = "https://xn--" ^ String.make 20_000 'a' ^ ".com/" in
  Alcotest.(check bool)
    "refused" false
    (Result.is_ok (J.decode J.uri (V.to_string (V.String uri))));
  Alcotest.(check bool) "at once" true (Sys.time () -. start < 0.05)

let kind_rows name shape rows () =
  List.iter
    (fun (text, server, _browser) ->
      Alcotest.(check bool)
        (Printf.sprintf "%s %S" name text)
        server
        (Result.is_ok (J.decode shape (V.to_string (V.String text)))))
    rows

let malformed () =
  Alcotest.(check (list problem))
    "a kind's refusal is at its place"
    [ ("", "malformed") ]
    (problems {|"2026-02-30"|} J.date)

(* Whatever value a kind can write, it reads back as itself. *)
let round_trip name shape gen ~equal ~print =
  QCheck.Test.make ~count:2000 ~name:(name ^ " reads back what it writes")
    (QCheck.make ~print gen) (fun v ->
      match written shape v with
      | Error m ->
          QCheck.Test.fail_reportf "%s cannot be written: %s" (print v) m
      | Ok s -> (
          match J.decode shape s with
          | Ok w -> equal v w
          | Error ps -> QCheck.Test.fail_report (P.list_to_string ps)))

let instant_of text =
  match J.decode J.instant (V.to_string (V.String text)) with
  | Ok t -> t
  | Error ps -> Alcotest.fail (P.list_to_string ps)

let kinds_round_trip =
  let open QCheck.Gen in
  let bytes = string_size ~gen:char (int_bound 40) in
  let hex =
    string_size
      ~gen:(oneof_list (List.init 16 (fun i -> "0123456789abcdef".[i])))
      (return 32)
  in
  let uuid (h, version, variant) =
    let part i n = String.sub h i n in
    Printf.sprintf "%s-%s-%c%s-%c%s-%s" (part 0 8) (part 8 4)
      "12345678".[version] (part 13 3) "89ab".[variant] (part 17 3) (part 20 12)
  in
  let date (y, m, d) = Printf.sprintf "%04d-%02d-%02d" y m d in
  let same_date (y, m, d) (y', m', d') = y = y' && m = m' && d = d' in
  [
    round_trip "an instant" J.instant
      (int_range
         (instant_of "0000-01-01T00:00:00Z")
         (instant_of "9999-12-31T23:59:59.999Z"))
      ~equal:Int.equal ~print:string_of_int;
    round_trip "a date" J.date
      (triple (int_range 0 9999) (int_range 1 12) (int_range 1 28))
      ~equal:same_date ~print:date;
    round_trip "a duration" J.duration
      (oneof [ int_range 0 3_155_760_000_000_000; int_range 0 100_000_000 ])
      ~equal:Int.equal ~print:string_of_int;
    round_trip "a uuid" (J.uuid ())
      (map uuid (triple hex (int_bound 7) (int_bound 3)))
      ~equal:String.equal ~print:Fun.id;
    round_trip "base64" J.base64 bytes ~equal:String.equal
      ~print:(Printf.sprintf "%S");
    round_trip "base64url" J.base64url bytes ~equal:String.equal
      ~print:(Printf.sprintf "%S");
  ]

(* One value has one spelling: what a kind reads it writes back as itself. *)
let spellings () =
  let round shape text expected =
    Alcotest.(check (result string string))
      text (Ok expected)
      (match J.decode shape (V.to_string (V.String text)) with
      | Error _ -> Error "refused"
      | Ok v -> written shape v)
  in
  round J.instant "2026-09-30T12:00:00Z" {|"2026-09-30T12:00:00.000Z"|};
  round J.instant "2026-09-30T17:30:00.123999+05:30"
    {|"2026-09-30T12:00:00.123Z"|};
  round J.instant "1969-12-31T23:59:59.999Z" {|"1969-12-31T23:59:59.999Z"|};
  round J.duration "P1DT1H1M1.001S" {|"P1DT1H1M1.001S"|};
  round J.duration "PT90M" {|"PT1H30M"|};
  round J.duration "P2W" {|"P14D"|};
  round J.duration "PT0S" {|"PT0S"|};
  round J.duration "PT1,5S" {|"PT1.5S"|};
  round (J.uuid ()) "01890A5D-AC96-7A3B-9E5A-5F1C2A7B8C9D"
    {|"01890a5d-ac96-7a3b-9e5a-5f1c2a7b8c9d"|};
  round J.base64 "QR==" {|"QQ=="|};
  Alcotest.(check (result string string))
    "an instant is epoch milliseconds" (Ok "0")
    (Result.map string_of_int
       (Result.map_error
          (fun _ -> "refused")
          (J.decode J.instant {|"1970-01-01T00:00:00Z"|})));
  Alcotest.(check (result string string))
    "a date past the years is not written"
    (Error
       "the value: 10000-1-1 is not a date of the years 0000 to 9999. \
        (unspellable)")
    (written J.date (10000, 1, 1));
  Alcotest.(check (result string string))
    "nor a negative duration"
    (Error "the value: A duration is never negative. (unspellable)")
    (written J.duration (-1))

(* Whether [s] holds [sub]: what the schema rows look for in a document. *)
let contains s sub =
  let n = String.length s and m = String.length sub in
  let rec at i =
    i + m <= n && (String.equal (String.sub s i m) sub || at (i + 1))
  in
  at 0

(* ------------------------------------------------------------------ *)
(* Derived *)

type placed = {
  row : int; [@min 0] [@max 18]  (** from the top *)
  col : int; [@min 0] [@max 18]
  note : string option; [@max_length 5]
  as_ : string; [@default "black"]
  board_size : int; [@key "size"] [@default 19]
}
[@@deriving wiretype]
(** A stone placed. *)

type side = Black_side | White_side | Not_a_side [@@deriving wiretype]

type order =
  | Find of { rank : int; minutes : int option }
  | Cancel of { id : string }
  | Wait
[@@deriving wiretype]

type shape = [ `Dot | `Point of placed ] [@@tag "kind"] [@@deriving wiretype]

type tree = { label : string; children : tree list [@default []] }
[@@deriving wiretype]

type 'a boxed = { inside : 'a; count : int }
[@@rename_all camel] [@@deriving wiretype]

(* A union of expressions, each through the other: a group with no
   parameters whose types refer to each other. *)
type expr = Num of { n : int } | Add of operands
and operands = { l : expr; r : expr } [@@deriving wiretype]

(* What the attributes a derived record may carry write. *)
type hue = Red | Dark_blue
[@@rename_all kebab] [@@kind "hue"] [@@deriving wiretype]

type login = {
  user : string;
  password : string; [@write_only]
  id : int64; [@min 1L] [@max 10L]
  shout : string; [@with J.map ~dec:String.uppercase_ascii ~enc:Fun.id J.string]
  tags : (string[@with J.string_bounded ~max_length:3 ()]) list;
}
[@@deriving wiretype]

let derived_attributes () =
  Alcotest.(check (result string string))
    "a group of two, each through the other"
    (Ok {|{"type":"add","l":{"type":"num","n":1},"r":{"type":"num","n":2}}|})
    (written expr_json (Add { l = Num { n = 1 }; r = Num { n = 2 } }));
  Alcotest.(check (result string string))
    "kebab words" (Ok {|["red","dark-blue"]|})
    (written (J.list hue_json) [ Red; Dark_blue ]);
  Alcotest.(check string) "named by [@@kind]" "hue" (J.name hue_json);
  Alcotest.(check (list problem))
    "an int64's bounds, and an item's [@with]"
    [ ("id", "too_large"); ("tags[0]", "too_long") ]
    (problems
       {|{"user":"a","password":"p","id":11,"shout":"x","tags":["long"]}|}
       login_json);
  match
    J.decode login_json
      {|{"user":"a","password":"p","id":2,"shout":"hey","tags":[]}|}
  with
  | Error ps -> Alcotest.failf "a login: %s" (P.list_to_string ps)
  | Ok l ->
      Alcotest.(check string) "a field's [@with]" "HEY" l.shout;
      let ctx = J.Schema.create () in
      let root = J.Schema.walk ctx J.Schema.Encode ~at:"login" login_json in
      Alcotest.(check bool)
        "a write-only member is in no answer" false
        (contains
           (J.Value.to_string
              (J.Schema.Json_schema.document (J.Schema.components ctx) root))
           "password")

(* What a person might have in scope, and types the deriver once wrote code
   for that did not compile: a pipe of the caller's own, a group mixing a
   type with a parameter and one without, a parameter beside a type whose
   name its argument took, and exclusive bounds. *)
module Awkward = struct
  let ( |> ) x _ = x

  type 'a pair = { left : 'a; right : 'a }
  and grid = { cells : int pair list } [@@deriving wiretype]

  type a = { s : string } [@@deriving wiretype]
  type 'a holder = { fixed : a; free : 'a } [@@deriving wiretype]
  type ratio = { r : float [@above 0.] [@below 1.] } [@@deriving wiretype]
end

let derived_awkward () =
  Alcotest.(check int)
    "the pipe in scope is the caller's" 1 (Awkward.( |> ) 1 2);
  Alcotest.(check (result string string))
    "a group mixing parameters" (Ok {|{"cells":[{"left":1,"right":2}]}|})
    (written Awkward.grid_json { cells = [ { left = 1; right = 2 } ] });
  Alcotest.(check (result string string))
    "a parameter beside a type of its name"
    (Ok {|{"fixed":{"s":"x"},"free":3}|})
    (written (Awkward.holder_json J.int) { fixed = { s = "x" }; free = 3 });
  Alcotest.(check (list problem))
    "exclusive bounds"
    [ ("r", "too_large") ]
    (problems {|{"r":1}|} Awkward.ratio_json)

let derived () =
  let ok shape text =
    Result.map_error (List.map P.to_string) (J.decode shape text)
  in
  let enc shape v = written shape v in
  Alcotest.(check (result string (list string)))
    "a record, its defaults, key and keyword"
    (Ok {|{"row":3,"col":4,"as":"black","size":19}|})
    (Result.bind (ok placed_json {|{"row":3,"col":4}|}) (fun v ->
         Result.map_error (fun e -> [ e ]) (enc placed_json v)));
  Alcotest.(check (list problem))
    "its bounds"
    [ ("row", "too_large"); ("note", "too_long") ]
    (problems {|{"row":19,"col":0,"note":"longer"}|} placed_json);
  Alcotest.(check (result string string))
    "an enum's words" (Ok {|["black_side","not_a_side"]|})
    (enc (J.list side_json) [ Black_side; Not_a_side ]);
  Alcotest.(check (result string string))
    "a union by its tag" (Ok {|[{"type":"find","rank":3},{"type":"wait"}]|})
    (enc (J.list order_json) [ Find { rank = 3; minutes = None }; Wait ]);
  (match ok order_json {|{"id":"x","type":"cancel"}|} with
  | Ok (Cancel { id }) -> Alcotest.(check string) "a case read, tag last" "x" id
  | Ok _ | Error _ -> Alcotest.fail "a cancel");
  Alcotest.(check (result string string))
    "a polymorphic variant, its tag named" (Ok {|{"kind":"dot"}|})
    (enc shape_json `Dot);
  (match
     ok tree_json
       {|{"label":"a","children":[{"label":"b","children":[{"label":"c"}]}]}|}
   with
  | Ok t ->
      Alcotest.(check int) "a type that holds itself" 1 (List.length t.children)
  | Error ps -> Alcotest.failf "a tree: %s" (String.concat "; " ps));
  Alcotest.(check (result string string))
    "a parameter, and camelCase" (Ok {|{"inside":true,"count":2}|})
    (enc (boxed_json J.bool) { inside = true; count = 2 })

(* What a derived record says of itself reaches a schema: its doc comments
   and its bounds, and an enum's exact words. *)
let derived_schema () =
  let ctx = J.Schema.create () in
  let root = J.Schema.walk ctx J.Schema.Encode ~at:"placed" placed_json in
  let doc =
    J.Value.to_string
      (J.Schema.Json_schema.document (J.Schema.components ctx) root)
  in
  let has what = Alcotest.(check bool) what true (contains doc what) in
  has {|"description":"A stone placed."|};
  has {|"description":"from the top"|};
  has {|"minimum":0,"maximum":18|};
  has {|"maxLength":5|};
  let zod = J.Schema.Zod.components (J.Schema.components ctx) in
  Alcotest.(check bool)
    "zod bounds" true
    (contains zod "z.int().check(z.gte(0), z.lte(18))");
  let ctx = J.Schema.create () in
  let side = J.Schema.walk ctx J.Schema.Encode ~at:"side" side_json in
  Alcotest.(check string)
    "an enum's words" {|z.enum(["black_side", "white_side", "not_a_side"])|}
    (J.Schema.Zod.of_t side)

(* An option is described as any member is: read-only, with examples. Any
   JSON is described by [Value.json], through whatever alias names it, and an
   array whatever [Array] is in scope. *)
module Profile = struct
  module Array = struct
    let of_list (_ : int list) = "not Stdlib's"
  end

  type t = {
    nick : string option; [@read_only] [@examples [ "ann" ]]
    extra : J.Value.t;
    scores : int array;
  }
  [@@deriving wiretype]
end

let derived_options () =
  Alcotest.(check string)
    "the array description is derived where Array is not Stdlib's"
    "not Stdlib's" (Profile.Array.of_list []);
  let walk dir =
    let ctx = J.Schema.create () in
    let root = J.Schema.walk ctx dir ~at:"profile" Profile.json in
    J.Value.to_string
      (J.Schema.Json_schema.document (J.Schema.components ctx) root)
  in
  Alcotest.(check bool)
    "an answer has it, with its example" true
    (contains (walk J.Schema.Encode) {|"examples":["ann"]|});
  Alcotest.(check bool)
    "a request has it not" false
    (contains (walk J.Schema.Decode) {|"nick"|});
  Alcotest.(check (result string reject))
    "any JSON and an array, read and written"
    (Ok {|{"nick":"ann","extra":{"a":[1]},"scores":[1,2]}|})
    (Result.bind
       (Result.map_error P.list_to_string
          (J.decode Profile.json
             {|{"nick":"ann","extra":{"a":[1]},"scores":[1,2]}|}))
       (written Profile.json))

(* Any text a document may hold: code points from all of Unicode but the
   surrogates, as UTF-8. *)
let unicode n =
  QCheck.Gen.(
    map
      (fun cps ->
        let b = Buffer.create 16 in
        List.iter (fun c -> Buffer.add_utf_8_uchar b (Uchar.of_int c)) cps;
        Buffer.contents b)
      (list_size (int_bound n)
         (oneof
            [
              int_range 0 0x7F; int_range 0x80 0xD7FF; int_range 0xE000 0x10FFFF;
            ])))

(* What cannot be written is said at its place, with its code: what [encode]
   writes, [decode] reads. *)
let unwritable () =
  let refused name shape v expected =
    Alcotest.(check (result string string))
      name (Error expected)
      (Result.map_error
         (fun (e : J.Unwritable.t) ->
           P.path e.at ^ " " ^ J.Unwritable.code_to_string e.code)
         (J.encode shape v))
  in
  refused "text that is not UTF-8" J.string "\xff" " not_utf8";
  refused "at its member" move
    { x = 0; y = 0; note = Some "a\xc3"; tags = [] }
    "note not_utf8";
  refused "a member given twice in any JSON" J.Value.json
    (V.Object [ ("a", V.Array [ V.Object [ ("b", V.Null); ("b", V.Null) ] ]) ])
    "a[0].b repeated_member";
  refused "a map's name written twice" (J.dict J.string J.int)
    [ ("a", 1); ("a", 2) ]
    "[1] repeated_member";
  refused "a map's name that is no text" (J.dict J.int J.int)
    [ (1, 1) ]
    "[0] name_not_text";
  refused "a description made only to read"
    (J.list (J.map ~dec:Fun.id J.int))
    [ 1 ] "[0] read_only";
  refused "a number that is no JSON number" J.number Float.nan " unspellable";
  refused "nor an infinite one" (J.list J.number) [ Float.infinity ]
    "[0] unspellable";
  refused "a value an enum has no word for"
    (J.enum string_of_int [ 1; 2 ])
    3 " unspellable";
  let case tag =
    J.Object.Case.map tag (J.Object.map () |> J.Object.finish) ~dec:Fun.id
  in
  refused "a case the union does not have"
    (J.Object.map Fun.id
    |> J.Object.case_mem "t" J.string ~enc:Fun.id
         ~enc_case:(fun () -> J.Object.Case.value (case "b") ())
         [ J.Object.Case.make (case "a") ]
    |> J.Object.finish)
    () "t unspellable";
  let rec deep n = if n = 0 then V.Null else V.Array [ deep (n - 1) ] in
  Alcotest.(check (result unit string))
    "nested as deep as a reader reads" (Ok ())
    (Result.map (fun _ -> ()) (written J.Value.json (deep 512)));
  refused "and no deeper" J.Value.json (deep 513)
    (String.concat "" (List.init 512 (fun _ -> "[0]")) ^ " too_deep")

(* An object, a union, a map and a tuple, written and read back: what is
   read is written as it was. *)
type mark = Dot | Note of { text : string } [@@deriving wiretype]

type sample = {
  id : int;
  tags : (string * int) list; [@dict]
  at : int * float;
  mark : mark;
  label : string option;
}
[@@deriving wiretype]

let descriptions_round_trip =
  let open QCheck.Gen in
  let mark =
    oneof [ return Dot; map (fun text -> Note { text }) (unicode 6) ]
  in
  let tags =
    map
      (List.sort_uniq (fun (a, _) (b, _) -> String.compare a b))
      (list_size (int_bound 4) (pair (unicode 4) nat_small))
  in
  let sample =
    map
      (fun (id, tags, at, (mark, label)) -> { id; tags; at; mark; label })
      (quad int tags
         (pair int (float_bound_inclusive 1e9))
         (pair mark (option (unicode 6))))
  in
  QCheck.Test.make ~count:2000 ~name:"a description reads back what it writes"
    (QCheck.make sample) (fun v ->
      match written sample_json v with
      | Error m -> QCheck.Test.fail_report m
      | Ok s -> (
          match J.decode sample_json s with
          | Error ps -> QCheck.Test.fail_report (P.list_to_string ps)
          | Ok w ->
              Result.equal ~ok:String.equal ~error:String.equal (Ok s)
                (written sample_json w)))

(* A tuple is a JSON array of exactly its items, each by its own description:
   another length is the list's problem, a wrong item its own at its index. *)
type waypoint = { at : int * int; label : string * float } [@@deriving wiretype]

let tuples () =
  let pair = J.tuple2 J.int J.string in
  Alcotest.(check (result (pair int string) reject))
    "read"
    (Ok (1, "a"))
    (J.decode pair {|[1, "a"]|});
  Alcotest.(check (result string string))
    "written" (Ok {|[1,"a"]|})
    (written pair (1, "a"));
  Alcotest.(check (list problem))
    "too few"
    [ ("", "too_few") ]
    (problems "[1]" pair);
  Alcotest.(check (list problem))
    "too many"
    [ ("", "too_many") ]
    (problems {|[1, "a", 3]|} pair);
  Alcotest.(check (list problem))
    "a wrong item, at its index"
    [ ("[0]", "unexpected_type") ]
    (problems {|["x", "a"]|} pair);
  Alcotest.(check (list problem))
    "not a list"
    [ ("", "unexpected_type") ]
    (problems "{}" pair);
  Alcotest.(check (result string string))
    "three, and any length built as an object is" (Ok {|[1,2.5,true]|})
    (written (J.tuple3 J.int J.number J.bool) (1, 2.5, true));
  Alcotest.(check (result string string))
    "derived" (Ok {|{"at":[3,4],"label":["tengen",0.5]}|})
    (Result.bind
       (Result.map_error
          (fun _ -> "refused")
          (J.decode waypoint_json {|{"at":[3,4],"label":["tengen",0.5]}|}))
       (written waypoint_json));
  let ctx = J.Schema.create () in
  let t = J.Schema.walk ctx J.Schema.Encode ~at:"pair" pair in
  Alcotest.(check string)
    "its JSON Schema"
    {|{"type":"array","prefixItems":[{"type":"integer"},{"type":"string"}],"minItems":2,"maxItems":2}|}
    (V.to_string (J.Schema.Json_schema.of_t t));
  Alcotest.(check string)
    "its zod" "z.tuple([z.int(), z.string()])" (J.Schema.Zod.of_t t)

(* A map is a JSON object read as its members, in order: a wrong value is a
   problem at its member, a wrong name one at the name, and a name given twice
   is refused both ways. *)
type ledger = {
  scores : (string * int) list; [@dict] [@min_properties 1]
  by_side : ((side * float) list[@dict]) option;
}
[@@deriving wiretype]

let dicts () =
  let scores = J.dict J.string J.int in
  Alcotest.(check (result (list (pair string int)) reject))
    "read, in the document's order"
    (Ok [ ("b", 2); ("a", 1) ])
    (J.decode scores {|{"b":2,"a":1}|});
  Alcotest.(check (result string string))
    "written, in the list's order" (Ok {|{"b":2,"a":1}|})
    (written scores [ ("b", 2); ("a", 1) ]);
  Alcotest.(check (result string string)) "empty" (Ok "{}") (written scores []);
  Alcotest.(check (list problem))
    "a wrong value, at its member"
    [ ("a", "unexpected_type"); ("c", "unexpected_type") ]
    (problems {|{"a":"x","b":1,"c":null}|} scores);
  Alcotest.(check (list problem))
    "a name given twice"
    [ ("a", "repeated_member") ]
    (problems {|{"a":1,"a":2}|} scores);
  Alcotest.(check bool)
    "a name written twice" true
    (Result.is_error (written scores [ ("a", 1); ("a", 2) ]));
  Alcotest.(check (list problem))
    "not an object"
    [ ("", "unexpected_type") ]
    (problems "[]" scores);
  Alcotest.(check (list problem))
    "a name its kind refuses, at the name"
    [ ("nope[name]", "malformed") ]
    (problems {|{"nope":true}|} (J.dict (J.uuid ()) J.bool));
  Alcotest.(check (list problem))
    "a name that is not a word, and a value wrong beside it"
    [ ("purple[name]", "unknown_word"); ("white_side", "unexpected_type") ]
    (problems {|{"purple":1,"white_side":"x"}|} (J.dict side_json J.int));
  Alcotest.(check (list problem))
    "a name too long"
    [ ("abc[name]", "too_long") ]
    (problems {|{"abc":1}|} (J.dict (J.string_bounded ~max_length:2 ()) J.int));
  let bounded = J.dict ~min_properties:1 ~max_properties:2 J.string J.int in
  Alcotest.(check (list problem))
    "too few"
    [ ("", "too_few") ]
    (problems "{}" bounded);
  Alcotest.(check (list problem))
    "too many"
    [ ("", "too_many") ]
    (problems {|{"a":1,"b":2,"c":3}|} bounded);
  Alcotest.(check bool)
    "a key not written as text" true
    (Result.is_error (written (J.dict J.int J.int) [ (1, 1) ]));
  Alcotest.(check (result string string))
    "derived" (Ok {|{"scores":{"x":1},"by_side":{"black_side":0.5}}|})
    (Result.bind
       (Result.map_error
          (fun _ -> "refused")
          (J.decode ledger_json
             {|{"scores":{"x":1},"by_side":{"black_side":0.5}}|}))
       (written ledger_json));
  Alcotest.(check (list problem))
    "derived, bounded"
    [ ("scores", "too_few") ]
    (problems {|{"scores":{}}|} ledger_json);
  let schema t =
    let ctx = J.Schema.create () in
    let s = J.Schema.walk ctx J.Schema.Encode ~at:"map" t in
    ( V.to_string (J.Schema.Json_schema.of_t s),
      J.Schema.Zod.of_t s,
      List.map J.Schema.error_to_string (J.Schema.errors ctx) )
  in
  let json, zod, errors = schema scores in
  Alcotest.(check string)
    "its JSON Schema"
    {|{"type":"object","additionalProperties":{"type":"integer"}}|} json;
  Alcotest.(check string) "its zod" "z.record(z.string(), z.int())" zod;
  Alcotest.(check (list string)) "no error" [] errors;
  let json, _, _ = schema (J.dict (J.uuid ()) J.bool) in
  Alcotest.(check string)
    "its names' kind"
    {|{"type":"object","additionalProperties":{"type":"boolean"},"propertyNames":{"type":"string","format":"uuid"}}|}
    json;
  let json, zod, _ = schema bounded in
  Alcotest.(check string)
    "its bounds"
    {|{"type":"object","additionalProperties":{"type":"integer"},"minProperties":1,"maxProperties":2}|}
    json;
  Alcotest.(check string)
    "its bounds in zod"
    "z.record(z.string(), z.int()).check(z.refine((o) => Object.keys(o).length \
     >= 1), z.refine((o) => Object.keys(o).length <= 2))"
    zod;
  let _, zod, _ = schema (J.dict side_json J.int) in
  Alcotest.(check string)
    "keyed by an enum, only the words it has"
    {|z.partialRecord(z.enum(["black_side", "white_side", "not_a_side"]), z.int())|}
    zod;
  let _, _, errors = schema (J.dict J.int J.int) in
  Alcotest.(check int) "a key that is not text" 1 (List.length errors)

(* ------------------------------------------------------------------ *)
(* Internationalised hosts, by Unicode's own conformance file *)

(* IdnaTestV2 (Unicode 15.0.0, idna-test/): each line a source, what UTS #46
   makes of it and the errors it finds. A browser's URL parser runs it with no
   hyphen rules, no STD3 rules and no DNS lengths, so those errors are
   dropped, and the rows are then read both ways: an ASCII host the standard
   refuses, [J.uri] refuses; one it takes, [J.uri] takes -- unless it is
   one of the two places [J.uri] is stricter, a joiner or a code point that
   may not be in NFC, which the row checks in Unicode's own file, or a label
   that decodes to one beginning [xn--], which UTS #46 has refused since
   15.1 and browsers with it, though this file of 15.0 takes it. *)
let ignored = [ "V2"; "V3"; "U1"; "A4_1"; "A4_2"; "X3"; "X4_2" ]

let unescape s =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let rec go i =
    if i >= n then ()
    else if s.[i] = '\\' && i + 1 < n && s.[i + 1] = 'u' && i + 5 < n then (
      Buffer.add_utf_8_uchar b
        (Uchar.of_int (int_of_string ("0x" ^ String.sub s (i + 2) 4)));
      go (i + 6))
    else if s.[i] = '\\' && i + 2 < n && s.[i + 1] = 'x' && s.[i + 2] = '{' then (
      let j = String.index_from s i '}' in
      Buffer.add_utf_8_uchar b
        (Uchar.of_int (int_of_string ("0x" ^ String.sub s (i + 3) (j - i - 3))));
      go (j + 1))
    else (
      Buffer.add_char b s.[i];
      go (i + 1))
  in
  go 0;
  Buffer.contents b

let codes s =
  let s = String.trim s in
  if String.length s < 2 then []
  else
    List.filter
      (fun c -> not (String.equal c ""))
      (String.split_on_char ' '
         (String.map
            (function ',' -> ' ' | c -> c)
            (String.sub s 1 (String.length s - 2))))

let code_points s =
  let rec go i acc =
    if i >= String.length s then List.rev acc
    else
      let d = String.get_utf_8_uchar s i in
      go
        (i + Uchar.utf_decode_length d)
        (Uchar.to_int (Uchar.utf_decode_uchar d) :: acc)
  in
  go 0 []

let not_nfc =
  lazy
    (List.filter_map
       (fun line ->
         match
           String.split_on_char ';' (List.hd (String.split_on_char '#' line))
         with
         | [ range; prop; value ]
           when String.equal (String.trim prop) "NFC_QC"
                && List.mem (String.trim value) [ "N"; "M" ] ->
             let range = String.trim range in
             Some
               (match String.split_on_char '.' range with
               | [ a; ""; b ] ->
                   (int_of_string ("0x" ^ a), int_of_string ("0x" ^ b))
               | _ ->
                   (int_of_string ("0x" ^ range), int_of_string ("0x" ^ range)))
         | _ -> None)
       (String.split_on_char '\n'
          (In_channel.with_open_bin
             "../src/unicode/DerivedNormalizationProps.txt" In_channel.input_all)))

(* Code points by their status in IdnaMappingTable, as the test reads it. *)
let statuses =
  lazy
    (List.filter_map
       (fun line ->
         match
           String.split_on_char ';' (List.hd (String.split_on_char '#' line))
         with
         | range :: status :: _ when not (String.equal (String.trim range) "")
           ->
             let range = String.trim range in
             let lo, hi =
               match String.split_on_char '.' range with
               | [ a; ""; b ] ->
                   (int_of_string ("0x" ^ a), int_of_string ("0x" ^ b))
               | _ ->
                   (int_of_string ("0x" ^ range), int_of_string ("0x" ^ range))
             in
             Some (lo, hi, String.trim status)
         | _ -> None)
       (String.split_on_char '\n'
          (In_channel.with_open_bin "../src/unicode/IdnaMappingTable.txt"
             In_channel.input_all)))

let status c =
  match
    List.find_opt (fun (lo, hi, _) -> c >= lo && c <= hi) (Lazy.force statuses)
  with
  | Some (_, _, s) -> s
  | None -> "disallowed"

(* A V6 the file finds only because it applies the STD3 rules, which a
   browser does not: every code point valid but for them. *)
let std3_only unicode =
  let cps = List.filter (fun c -> c <> 0x2E) (code_points unicode) in
  List.for_all
    (fun c ->
      List.mem (status c) [ "valid"; "deviation"; "disallowed_STD3_valid" ])
    cps
  && List.exists (fun c -> String.equal (status c) "disallowed_STD3_valid") cps

(* Refused where a browser takes it: a joiner, a code point that may not be
   in NFC, a label already in [xn--], and an A-label past DNS's 63 octets. *)
let stricter unicode ascii_n =
  List.exists (fun l -> String.length l > 63) (String.split_on_char '.' ascii_n)
  || List.exists
       (fun c ->
         c = 0x200C || c = 0x200D
         || List.exists
              (fun (lo, hi) -> c >= lo && c <= hi)
              (Lazy.force not_nfc))
       (code_points unicode)
  || List.exists
       (fun l -> String.starts_with ~prefix:"xn--" l)
       (String.split_on_char '.' unicode)

let plain_host h =
  let h =
    if String.ends_with ~suffix:"." h then String.sub h 0 (String.length h - 1)
    else h
  in
  let labels = String.split_on_char '.' h in
  List.for_all
    (fun l ->
      String.length l > 0
      && String.for_all
           (function 'a' .. 'z' | '0' .. '9' | '-' | '_' -> true | _ -> false)
           l)
    labels
  &&
  match List.rev labels with
  | last :: _ -> not (match last.[0] with '0' .. '9' -> true | _ -> false)
  | [] -> false

let ascii s = String.for_all (fun c -> Char.code c < 0x80) s

let takes host =
  Result.is_ok
    (J.decode J.uri (V.to_string (V.String ("http://" ^ host ^ "/"))))

let idna_rows () =
  let lines =
    String.split_on_char '\n'
      (In_channel.with_open_bin "idna-test/IdnaTestV2.txt" In_channel.input_all)
  in
  let refused = ref 0 and taken = ref 0 in
  List.iter
    (fun line ->
      let line = List.hd (String.split_on_char '#' line) in
      match List.map String.trim (String.split_on_char ';' line) with
      | source :: unicode :: ustatus :: ascii_n :: astatus :: _ ->
          let source = unescape source in
          let unicode =
            if String.equal unicode "" then source else unescape unicode
          in
          let ustatus = codes ustatus in
          let ascii_n =
            if String.equal ascii_n "" then unicode else unescape ascii_n
          in
          let astatus =
            if String.equal astatus "" then ustatus else codes astatus
          in
          let errors =
            List.filter (fun c -> not (List.mem c ignored)) astatus
          in
          let errors =
            match errors with
            | [ "V6" ] when std3_only unicode -> []
            | errors -> errors
          in
          if (match errors with [] -> false | _ :: _ -> true) && ascii source
          then begin
            incr refused;
            if takes source then
              Alcotest.failf "%S is refused by UTS #46 (%s) and taken" source
                (String.concat " " errors)
          end
          else if
            (match errors with [] -> true | _ :: _ -> false)
            && ascii ascii_n && plain_host ascii_n
            && not (stricter unicode ascii_n)
          then begin
            incr taken;
            if not (takes ascii_n) then
              Alcotest.failf "%S (%s) is taken by UTS #46 and refused" ascii_n
                unicode
          end
      | _ -> ())
    lines;
  Alcotest.(check bool)
    (Printf.sprintf "rows read both ways: %d refused, %d taken" !refused !taken)
    true
    (!refused > 1000 && !taken > 400)

(* The bytes a document is written as: minified, a double in the fewest
   digits that read back as itself, and text escaped as RFC 8259 asks. *)
let bytes () =
  let enc v = written J.Value.json v in
  Alcotest.(check (result string string))
    "numbers" (Ok "[0.1,0.30000000000000004,3,-0,1e+21,null]")
    (enc
       (V.Array
          (List.map
             (fun f -> V.Number f)
             [ 0.1; 0.1 +. 0.2; 3.; -0.; 1e21; Float.nan ])));
  Alcotest.(check (result string string))
    "escapes" (Ok {|"q\"b\\n\nc\u0001d\u007Fé"|})
    (enc (V.String "q\"b\\n\nc\001d\127é"));
  Alcotest.(check (result string string))
    "an int past 2^53, as its digits" (Ok "9007199254740993")
    (written J.int 9007199254740993)

let test_numbers_are_written_as_typed () =
  match
    written
      Wiretype.(list number)
      [ 0.1; 7.5; -0.76; 1.0; -0.; 0.1 +. 0.2; 1. /. 3.; 1e21; 5e-324 ]
  with
  | Error m -> Alcotest.fail m
  | Ok s ->
      Alcotest.(check string)
        "each read back as it was"
        {|[0.1,7.5,-0.76,1,-0,0.30000000000000004,0.3333333333333333,1e+21,5e-324]|}
        s

(* Every finite double reads back as itself. *)
let test_a_number_reads_back_as_itself =
  QCheck.Test.make ~count:5000 ~name:"a number reads back as itself"
    QCheck.(map Int64.float_of_bits int64)
    (fun f ->
      QCheck.assume (Float.is_finite f);
      match written Wiretype.number f with
      | Ok s -> Float.equal (float_of_string s) f
      | Error m -> QCheck.Test.fail_report m)

(* The encoder's buffer is smaller than an answer can be, so a value
   several times its size, with an escape at every few bytes, comes out
   whole wherever a boundary falls. *)
let test_a_value_longer_than_the_buffer () =
  let times n s = String.concat "" (List.init n (fun _ -> s)) in
  match written Wiretype.string (times 600 {|ab"cd|}) with
  | Error m -> Alcotest.fail m
  | Ok s ->
      Alcotest.(check string)
        "every byte"
        ({|"|} ^ times 600 {|ab\"cd|} ^ {|"|})
        s

(* ------------------------------------------------------------------ *)
(* A double's spelling *)

let spell f =
  match written J.number f with Ok s -> s | Error e -> Alcotest.fail e

(* A number's significant digits, whatever its layout: [1.50e+3] is [15]. *)
let significant s =
  let mantissa =
    match String.index_opt s 'e' with Some i -> String.sub s 0 i | None -> s
  in
  let d = String.concat "" (String.split_on_char '.' mantissa) in
  let d =
    if String.length d > 0 && d.[0] = '-' then
      String.sub d 1 (String.length d - 1)
    else d
  in
  let rec strip_lead i =
    if i < String.length d && d.[i] = '0' then strip_lead (i + 1) else i
  in
  let d = String.sub d (strip_lead 0) (String.length d - strip_lead 0) in
  let rec strip_trail n =
    if n > 0 && d.[n - 1] = '0' then strip_trail (n - 1) else n
  in
  String.sub d 0 (strip_trail (String.length d))

(* The fewest digits that read back, found by asking every precision. *)
let brute f =
  let rec go p =
    let s = Printf.sprintf "%.*g" p f in
    if p >= 17 || Float.equal (float_of_string s) f then s else go (p + 1)
  in
  go 1

(* The spelling before the printer was ours: sixteen digits, or seventeen. *)
let formatter f =
  let s = Printf.sprintf "%.16g" f in
  if Float.equal (float_of_string s) f then s else Printf.sprintf "%.17g" f

let judged f =
  let ours = spell f in
  Float.equal (float_of_string ours) f
  && String.length (significant ours) <= String.length (significant (brute f))
  && (String.length (significant ours) < String.length (significant (brute f))
     || String.equal (significant ours) (significant (brute f)))
  && (String.length (significant ours)
      <> String.length (significant (formatter f))
     || String.equal ours (formatter f))

let finite =
  QCheck.(
    make ~print:(Printf.sprintf "%h") (Gen.map Int64.float_of_bits Gen.int64))

let test_a_double_is_spelt_shortest =
  QCheck.Test.make ~count:200_000
    ~name:"a double's digits are its fewest, as a search for them finds" finite
    (fun f ->
      QCheck.assume
        (Float.is_finite f && not (Float.is_integer f && Float.abs f < 0x1p53));
      judged f)

(* Every binary exponent a double has, at its smallest, its largest and a
   middle mantissa, and each of those a bit either side. *)
let every_exponent () =
  for e = 0 to 2046 do
    List.iter
      (fun m ->
        let bits = Int64.logor (Int64.shift_left (Int64.of_int e) 52) m in
        List.iter
          (fun b ->
            let f = Int64.float_of_bits b in
            if
              Float.is_finite f
              && not (Float.is_integer f && Float.abs f < 0x1p53)
            then
              if not (judged f) then
                Alcotest.failf "%h is spelt %s; the fewest digits are %s" f
                  (spell f) (brute f))
          [ bits; Int64.succ bits; Int64.pred bits ])
      [ 1L; 0x8_0000_0000_0000L; 0xF_FFFF_FFFF_FFFFL ]
  done

let spellings_known () =
  List.iter
    (fun (f, s) -> Alcotest.(check string) s s (spell f))
    [
      (5e-324, "5e-324");
      (Float.max_float, "1.7976931348623157e+308");
      (Float.min_float, "2.2250738585072014e-308");
      (0.1, "0.1");
      (0.1 +. 0.2, "0.30000000000000004");
      (1. /. 3., "0.3333333333333333");
      (1e21, "1e+21");
      (123456789012345680., "1.2345678901234568e+17");
      (1e-5, "1e-05");
      (0.0001, "0.0001");
      (-2.5, "-2.5");
      (-0., "-0");
      (9007199254740993., "9007199254740992");
    ]

(* What we write, a reader that is not ours reads back as what we wrote. *)
let referee =
  let open QCheck in
  let rec gen n =
    Gen.(
      if n <= 0 then
        oneof
          [
            return V.Null;
            map (fun b -> V.Bool b) bool;
            map (fun i -> V.Number (float_of_int i)) int_small;
            map (fun f -> V.Number f) (float_bound_inclusive 1e6);
            map (fun s -> V.String s) (unicode 8);
          ]
      else
        oneof_weighted
          [
            (3, gen 0);
            (1, map (fun l -> V.Array l) (list_size (int_bound 4) (gen (n / 2))));
            ( 1,
              map
                (fun l ->
                  V.Object
                    (List.sort_uniq (fun (a, _) (b, _) -> String.compare a b) l))
                (list_size (int_bound 4) (pair (unicode 4) (gen (n / 2)))) );
          ])
  in
  let rec yo = function
    | V.Null -> `Null
    | V.Bool b -> `Bool b
    | V.Number f -> `Float f
    | V.String s -> `String s
    | V.Array l -> `List (List.map yo l)
    | V.Object m -> `Assoc (List.map (fun (k, v) -> (k, yo v)) m)
  in
  let rec same (a : Yojson.Safe.t) (b : Yojson.Safe.t) =
    match (a, b) with
    | `Int x, `Float y | `Float y, `Int x -> Float.equal (float_of_int x) y
    | `Float x, `Float y -> Float.equal x y
    | `Int x, `Int y -> x = y
    | `List xs, `List ys -> List.equal same xs ys
    | `Assoc xs, `Assoc ys ->
        List.equal (fun (k, x) (l, y) -> String.equal k l && same x y) xs ys
    | a, b -> Yojson.Safe.equal a b
  in
  Test.make ~count:500 ~name:"yojson reads back what we write, and so do we"
    (make ~print:V.to_string (Gen.sized gen))
    (fun v ->
      match written J.Value.json v with
      | Error _ -> false
      | Ok s -> (
          same (Yojson.Safe.from_string s) (yo v)
          &&
          match J.decode J.Value.json s with
          | Ok w -> V.equal v w
          | Error _ -> false))

let () =
  Alcotest.run "wiretype"
    [
      ( "parser",
        [
          Alcotest.test_case "JSONTestSuite" `Quick suite_rows;
          Alcotest.test_case "overflow" `Quick overflow;
          Alcotest.test_case "paths" `Quick paths;
          Alcotest.test_case "strings" `Quick strings;
          Alcotest.test_case "nesting" `Quick nesting;
        ] );
      ( "descriptions",
        [
          Alcotest.test_case "every problem" `Quick every_problem;
          Alcotest.test_case "absent and null" `Quick absent_and_null;
          Alcotest.test_case "unknown members" `Quick unknown;
          Alcotest.test_case "numbers" `Quick numbers;
          Alcotest.test_case "enums" `Quick enums;
          Alcotest.test_case "unions" `Quick unions;
          Alcotest.test_case "tuples" `Quick tuples;
          Alcotest.test_case "maps" `Quick dicts;
          Alcotest.test_case "names" `Quick names;
          Alcotest.test_case "values, tuples and the pieces" `Quick values;
          Alcotest.test_case "malformed descriptions" `Quick
            malformed_descriptions;
        ] );
      ( "writing",
        [
          Alcotest.test_case "what cannot be written" `Quick unwritable;
          QCheck_alcotest.to_alcotest descriptions_round_trip;
          Alcotest.test_case "bytes" `Quick bytes;
          Alcotest.test_case "numbers as typed" `Quick
            test_numbers_are_written_as_typed;
          QCheck_alcotest.to_alcotest test_a_number_reads_back_as_itself;
          QCheck_alcotest.to_alcotest test_a_double_is_spelt_shortest;
          Alcotest.test_case "every exponent" `Quick every_exponent;
          Alcotest.test_case "known spellings" `Quick spellings_known;
          Alcotest.test_case "longer than the buffer" `Quick
            test_a_value_longer_than_the_buffer;
          QCheck_alcotest.to_alcotest referee;
        ] );
      ( "derived",
        [
          Alcotest.test_case "values" `Quick derived;
          Alcotest.test_case "schema" `Quick derived_schema;
          Alcotest.test_case "options, any JSON and arrays" `Quick
            derived_options;
          Alcotest.test_case "what once did not compile" `Quick derived_awkward;
          Alcotest.test_case "attributes" `Quick derived_attributes;
        ] );
      ( "kinds",
        [
          Alcotest.test_case "instant" `Quick
            (kind_rows "instant" J.instant instant_rows);
          Alcotest.test_case "date" `Quick (kind_rows "date" J.date date_rows);
          Alcotest.test_case "duration" `Quick
            (kind_rows "duration" J.duration duration_rows);
          Alcotest.test_case "uuid" `Quick
            (kind_rows "uuid" (J.uuid ()) uuid_rows);
          Alcotest.test_case "uuid v7" `Quick
            (kind_rows "uuid v7" (J.uuid ~version:`V7 ()) uuid7_rows);
          Alcotest.test_case "base64" `Quick
            (kind_rows "base64" J.base64 base64_rows);
          Alcotest.test_case "base64url" `Quick
            (kind_rows "base64url" J.base64url base64url_rows);
          Alcotest.test_case "uri" `Quick (kind_rows "uri" J.uri uri_rows);
          Alcotest.test_case "ipv4" `Quick (kind_rows "ipv4" J.ipv4 ipv4_rows);
          Alcotest.test_case "ipv6" `Quick (kind_rows "ipv6" J.ipv6 ipv6_rows);
          Alcotest.test_case "an internationalised host, by IdnaTestV2" `Quick
            idna_rows;
          Alcotest.test_case "a label longer than DNS allows" `Quick long_label;
          Alcotest.test_case "malformed" `Quick malformed;
          Alcotest.test_case "spellings" `Quick spellings;
        ]
        @ List.map QCheck_alcotest.to_alcotest kinds_round_trip );
    ]
