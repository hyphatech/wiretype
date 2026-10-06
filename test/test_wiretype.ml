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

let dir = "json-test-suite"

let read file =
  In_channel.with_open_bin (Filename.concat dir file) In_channel.input_all

let files prefix =
  List.sort String.compare
    (List.filter
       (fun f ->
         String.starts_with ~prefix f && Filename.check_suffix f ".json")
       (Array.to_list (Sys.readdir dir)))

let accepts text = Result.is_ok (J.decode J.value text)

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
  match J.decode J.value "1e400" with
  | Error [ { code = P.Too_large; _ } ] -> ()
  | Ok _ | Error _ -> Alcotest.fail "1e400 should be too large"

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
    (problems {|{"x": 1, "y": 2, "tags": ["a" "b"]}|} move)

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
    (J.encode move { x = 1; y = 2; note = None; tags = [] });
  let nullable =
    J.Object.map (fun n -> n)
    |> J.Object.mem "n" (J.nullable J.int) ~enc:Fun.id
    |> J.Object.finish
  in
  Alcotest.(check (result string string))
    "a nullable member writes null" (Ok {|{"n":null}|}) (J.encode nullable None);
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
    (problems {|{"a":1,"b":2}|} strict)

let numbers () =
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
    (problems "\"a\nb\"" J.string)

let nesting () =
  let deep n = String.make n '[' ^ String.make n ']' in
  Alcotest.(check bool)
    "512 levels" true
    (Result.is_ok (J.decode J.value (deep 512)));
  Alcotest.(check (list problem))
    "513 are too deep"
    [ (String.concat "" (List.init 512 (fun _ -> "[0]")), "too_deep") ]
    (problems (deep 513) J.value)

type colour = Black | White

let colour =
  J.enum ~kind:"colour"
    (function Black -> "black" | White -> "white")
    [ Black; White ]

let enums () =
  Alcotest.(check (result string string))
    "written as its word" (Ok {|"white"|}) (J.encode colour White);
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
    (J.encode turn { by = "w"; play = Pass });
  Alcotest.(check (result string string))
    "a tag left out when it may be" (Ok {|{"by":"b","row":1,"col":2}|})
    (J.encode turn { by = "b"; play = Play (1, 2) })

(* ------------------------------------------------------------------ *)
(* The ready-made kinds *)

(* Each row is a string, whether the server takes it, and whether the
   browser's zod check does; the same rows are written out in the web suite's
   kinds.test.ts, which runs zod over them. The two answers are equal but
   where the browser is the looser -- a URI -- so the server never takes what
   the browser refuses. *)
let instant_rows =
  [
    ("2026-09-30T12:00:00Z", true, true);
    ("2026-09-30T12:00:00.5Z", true, true);
    ("2026-09-30T12:00:00.123456789+05:30", true, true);
    ("0000-01-01T00:00:00Z", true, true);
    ("2024-02-29T23:59:59-23:59", true, true);
    ("9999-12-31T23:59:59.999Z", true, true);
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
    ("https://xn--zz.com/", false, false);
    ("https://xn--abc-.com/", false, false);
    ("https://xn--xn--a--gua.pt/", false, false);
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
  ]

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

(* One value has one spelling: what a kind reads it writes back as itself. *)
let spellings () =
  let round shape text expected =
    Alcotest.(check (result string string))
      text (Ok expected)
      (match J.decode shape (V.to_string (V.String text)) with
      | Error _ -> Error "refused"
      | Ok v -> J.encode shape v)
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
    (Error "10000-1-1 is not a date of the years 0000 to 9999.")
    (J.encode J.date (10000, 1, 1));
  Alcotest.(check (result string string))
    "nor a negative duration" (Error "A duration is never negative.")
    (J.encode J.duration (-1))

(* Whether [s] holds [sub]: what the schema rows look for in a document. *)
module Astring_contains = struct
  let contains s sub =
    let n = String.length s and m = String.length sub in
    let rec at i =
      i + m <= n && (String.equal (String.sub s i m) sub || at (i + 1))
    in
    at 0
end

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

let derived () =
  let ok shape text =
    Result.map_error (List.map P.to_string) (J.decode shape text)
  in
  let enc shape v = J.encode shape v in
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
  let has what =
    Alcotest.(check bool) what true (Astring_contains.contains doc what)
  in
  has {|"description":"A stone placed."|};
  has {|"description":"from the top"|};
  has {|"minimum":0,"maximum":18|};
  has {|"maxLength":5|};
  let zod = J.Schema.Zod.components (J.Schema.components ctx) in
  Alcotest.(check bool)
    "zod bounds" true
    (Astring_contains.contains zod "z.int().check(z.gte(0), z.lte(18))");
  let ctx = J.Schema.create () in
  let side = J.Schema.walk ctx J.Schema.Encode ~at:"side" side_json in
  Alcotest.(check string)
    "an enum's words" {|z.enum(["black_side", "white_side", "not_a_side"])|}
    (J.Schema.Zod.of_t side)

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
    (J.encode pair (1, "a"));
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
    (J.encode (J.tuple3 J.int J.number J.bool) (1, 2.5, true));
  Alcotest.(check (result string string))
    "derived" (Ok {|{"at":[3,4],"label":["tengen",0.5]}|})
    (Result.bind
       (Result.map_error
          (fun _ -> "refused")
          (J.decode waypoint_json {|{"at":[3,4],"label":["tengen",0.5]}|}))
       (J.encode waypoint_json));
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
    (J.encode scores [ ("b", 2); ("a", 1) ]);
  Alcotest.(check (result string string)) "empty" (Ok "{}") (J.encode scores []);
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
    (Result.is_error (J.encode scores [ ("a", 1); ("a", 2) ]));
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
    (Result.is_error (J.encode (J.dict J.int J.int) [ (1, 1) ]));
  Alcotest.(check (result string string))
    "derived" (Ok {|{"scores":{"x":1},"by_side":{"black_side":0.5}}|})
    (Result.bind
       (Result.map_error
          (fun _ -> "refused")
          (J.decode ledger_json
             {|{"scores":{"x":1},"by_side":{"black_side":0.5}}|}))
       (J.encode ledger_json));
  Alcotest.(check (list problem))
    "derived, bounded"
    [ ("scores", "too_few") ]
    (problems {|{"scores":{}}|} ledger_json);
  let schema t =
    let ctx = J.Schema.create () in
    let s = J.Schema.walk ctx J.Schema.Encode ~at:"map" t in
    ( V.to_string (J.Schema.Json_schema.of_t s),
      J.Schema.Zod.of_t s,
      J.Schema.errors ctx )
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

let stricter unicode =
  List.exists
    (fun c ->
      c = 0x200C || c = 0x200D
      || List.exists (fun (lo, hi) -> c >= lo && c <= hi) (Lazy.force not_nfc))
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
            && not (stricter unicode)
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
  let enc v = J.encode J.value v in
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
    (J.encode J.int 9007199254740993)

let test_numbers_are_written_as_typed () =
  match
    Wiretype.encode
      Wiretype.(list number)
      [
        0.1; 7.5; -0.76; 1.0; -0.; 0.1 +. 0.2; 1. /. 3.; 1e21; 5e-324; Float.nan;
      ]
  with
  | Error m -> Alcotest.fail m
  | Ok s ->
      Alcotest.(check string)
        "each read back as it was"
        {|[0.1,7.5,-0.76,1,-0,0.30000000000000004,0.3333333333333333,1e+21,5e-324,null]|}
        s

(* Every finite double reads back as itself. *)
let test_a_number_reads_back_as_itself =
  QCheck.Test.make ~count:5000 ~name:"a number reads back as itself"
    QCheck.(map Int64.float_of_bits int64)
    (fun f ->
      QCheck.assume (Float.is_finite f);
      match Wiretype.encode Wiretype.number f with
      | Ok s -> Float.equal (float_of_string s) f
      | Error m -> QCheck.Test.fail_report m)

(* The encoder's buffer is smaller than an answer can be, so a value
   several times its size, with an escape at every few bytes, comes out
   whole wherever a boundary falls. *)
let test_a_value_longer_than_the_buffer () =
  let times n s = String.concat "" (List.init n (fun _ -> s)) in
  match Wiretype.encode Wiretype.string (times 600 {|ab"cd|}) with
  | Error m -> Alcotest.fail m
  | Ok s ->
      Alcotest.(check string)
        "every byte"
        ({|"|} ^ times 600 {|ab\"cd|} ^ {|"|})
        s

(* ------------------------------------------------------------------ *)
(* A double's spelling *)

let spell f =
  match J.encode J.number f with Ok s -> s | Error e -> Alcotest.fail e

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
    ~name:"a double's digits are its fewest, as before" finite (fun f ->
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
            map (fun s -> V.String s) (string_size ~gen:printable (int_bound 8));
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
                (list_size (int_bound 4)
                   (pair
                      (string_size ~gen:printable (int_bound 4))
                      (gen (n / 2)))) );
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
      match J.encode J.value v with
      | Error _ -> false
      | Ok s -> (
          same (Yojson.Safe.from_string s) (yo v)
          &&
          match J.decode J.value s with
          | Ok w -> V.equal v w
          | Error _ -> false))

let () =
  Alcotest.run "wiretype"
    [
      ( "parser",
        [
          Alcotest.test_case "JSONTestSuite" `Quick suite_rows;
          Alcotest.test_case "overflow" `Quick overflow;
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
        ] );
      ( "writing",
        [
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
            (kind_rows "uuid v7" (J.uuid ~version:7 ()) uuid7_rows);
          Alcotest.test_case "base64" `Quick
            (kind_rows "base64" J.base64 base64_rows);
          Alcotest.test_case "base64url" `Quick
            (kind_rows "base64url" J.base64url base64url_rows);
          Alcotest.test_case "uri" `Quick (kind_rows "uri" J.uri uri_rows);
          Alcotest.test_case "ipv4" `Quick (kind_rows "ipv4" J.ipv4 ipv4_rows);
          Alcotest.test_case "ipv6" `Quick (kind_rows "ipv6" J.ipv6 ipv6_rows);
          Alcotest.test_case "an internationalised host, by IdnaTestV2" `Quick
            idna_rows;
          Alcotest.test_case "malformed" `Quick malformed;
          Alcotest.test_case "spellings" `Quick spellings;
        ] );
    ]
