(* The deriver's refusals: a type with no JSON shape is refused where it is
   written, saying what to write instead. Each snippet is parsed and expanded
   in-process by the deriver this executable links, which reports a refusal
   as an error in its output. *)

open Ppxlib

let contains s sub =
  let n = String.length s and m = String.length sub in
  let rec at i =
    i + m <= n && (String.equal (String.sub s i m) sub || at (i + 1))
  in
  at 0

(* Every error the expansion holds, as its message: each is printed as
   [[%ocaml.error "..."]], its message an OCaml string literal after any
   blanks. *)
let errors expanded =
  let printed = Format.asprintf "%a" Pprintast.structure expanded in
  let marker = "[%ocaml.error" in
  let n = String.length printed and m = String.length marker in
  let rec from i acc =
    if i + m > n then List.rev acc
    else if String.equal (String.sub printed i m) marker then
      let message =
        Scanf.sscanf (String.sub printed (i + m) (n - i - m)) " %S" Fun.id
      in
      from (i + m) (message :: acc)
    else from (i + 1) acc
  in
  from 0 []

let expand source =
  let structure = Parse.implementation (Lexing.from_string source) in
  match Driver.map_structure structure with
  | expanded -> (
      match errors expanded with
      | [] -> Ok (Format.asprintf "%a" Pprintast.structure expanded)
      | ms -> Error (String.concat "\n" ms))
  | exception Location.Error e -> Error (Location.Error.message e)

(* A refusal is an error in the output, at its place. *)
let refused source message () =
  let said = match expand source with Ok out -> out | Error m -> m in
  if not (contains said message) then
    Alcotest.failf "expected %S in:\n%s" message said

let accepted source () =
  match expand source with
  | Ok _ -> ()
  | Error m -> Alcotest.failf "refused: %s" m

let () =
  Alcotest.run "ppx_wiretype"
    [
      ( "refusals",
        [
          Alcotest.test_case "a function" `Quick
            (refused "type t = { f : int -> int } [@@deriving wiretype]"
               "this type has no JSON shape");
          Alcotest.test_case "several arguments" `Quick
            (refused "type t = A of int * string [@@deriving wiretype]"
               "give them names in an inline record");
          Alcotest.test_case "a bound on text" `Quick
            (refused "type t = { n : string [@min 1] } [@@deriving wiretype]"
               "bound an int, an int64 or a float");
          Alcotest.test_case "a length on a number" `Quick
            (refused
               "type t = { n : int [@max_length 1] } [@@deriving wiretype]"
               "[@max_length] bound a string");
          Alcotest.test_case "an unknown spelling" `Quick
            (refused
               "type t = { a : int } [@@rename_all pascal] [@@deriving \
                wiretype]"
               "[@@rename_all pascal]: write camel, kebab or snake");
          Alcotest.test_case "an abstract type" `Quick
            (refused "type t [@@deriving wiretype]" "is abstract");
          Alcotest.test_case "a map that is not a list of pairs" `Quick
            (refused "type t = { m : int list [@dict] } [@@deriving wiretype]"
               "[@dict] describes a (key * value) list");
          Alcotest.test_case "a map's bound on a list" `Quick
            (refused
               "type t = { m : int list [@min_properties 1] } [@@deriving \
                wiretype]"
               "[@min_properties] and [@max_properties] bound a map");
          Alcotest.test_case "an item bound on a map" `Quick
            (refused
               "type t = { m : (string * int) list [@dict] [@min_items 1] } \
                [@@deriving wiretype]"
               "a map is bounded by [@min_properties]");
          Alcotest.test_case "a member named twice" `Quick
            (refused
               "type t = { a : int; b : int [@key \"a\"] } [@@deriving \
                wiretype]"
               "a and b are both the member \"a\"");
          Alcotest.test_case "a member spelt twice" `Quick
            (refused
               "type t = { a_b : int; aB : int } [@@rename_all camel] \
                [@@deriving wiretype]"
               "a_b and aB are both the member \"aB\"");
          Alcotest.test_case "a member that is the tag" `Quick
            (refused
               "type t = A of { type_ : string } | B [@@deriving wiretype]"
               "type_ is the member \"type\", which is the union's tag");
          Alcotest.test_case "every refusal in a file, and the rest derived"
            `Quick (fun () ->
              let source =
                "type a = { f : int -> int } [@@deriving wiretype]\n\
                 type b = { g : string [@min 1]; h : int [@max_length 2] } \
                 [@@deriving wiretype]\n\
                 type c = { i : int } [@@deriving wiretype]"
              in
              let structure =
                Driver.map_structure
                  (Parse.implementation (Lexing.from_string source))
              in
              Alcotest.(check int)
                "three refusals" 3
                (List.length (errors structure));
              Alcotest.(check bool)
                "c is still derived" true
                (contains
                   (Format.asprintf "%a" Pprintast.structure structure)
                   "let c_json"));
          Alcotest.test_case "read-only and write-only at once" `Quick
            (refused
               "type t = { a : int; [@read_only] [@write_only] } [@@deriving \
                wiretype]"
               "write [@read_only] or [@write_only], not both");
          Alcotest.test_case "two constructors written alike" `Quick
            (refused "type t = A | B [@name \"a\"] [@@deriving wiretype]"
               "two constructors are written \"a\"");
          Alcotest.test_case "an exclusive bound on an int" `Quick
            (refused "type t = { a : int [@above 0] } [@@deriving wiretype]"
               "[@above] and [@below] bound a float; on an int, write [@min] \
                or [@max]");
          Alcotest.test_case "an abstract type, and what to write" `Quick
            (refused "type t [@@deriving wiretype]"
               "write its description by hand, as json");
          Alcotest.test_case "an extensible type, and what to write" `Quick
            (refused "type t = .. [@@deriving wiretype]"
               "write its description by hand, as json");
          Alcotest.test_case "a map that is none, and what to write" `Quick
            (refused "type t = { m : int list [@dict] } [@@deriving wiretype]"
               "drop [@dict], or describe it with [@with d]");
          Alcotest.test_case
            "a bound on what it cannot bound, and what to write" `Quick
            (refused "type t = { n : string [@min 1] } [@@deriving wiretype]"
               "drop them, or describe it with [@with d]");
          Alcotest.test_case "an attribute wiretype has not" `Quick
            (refused
               "type t = { a : int [@wiretype.mni 1] } [@@deriving wiretype]"
               "[@wiretype.mni] is no attribute of wiretype's");
          Alcotest.test_case "an attribute where wiretype does not read it"
            `Quick
            (refused "type t = A [@key \"x\"] | B [@@deriving wiretype]"
               "[@key] is read on a record's field, not here");
          Alcotest.test_case "a tag on a type that is no union" `Quick
            (refused "type t = { a : int } [@@tag \"k\"] [@@deriving wiretype]"
               "[@@tag] names a union's tag, and t is no union");
          Alcotest.test_case "an inherited polymorphic variant" `Quick
            (refused "type a = [ `A ] type t = [ a | `B ] [@@deriving wiretype]"
               "list its tags");
          Alcotest.test_case "a polymorphic tag with several types" `Quick
            (refused "type t = [ `A of int & string ] [@@deriving wiretype]"
               "has several types");
          Alcotest.test_case "an unnamed parameter" `Quick
            (refused "type _ t = { a : int } [@@deriving wiretype]"
               "a type parameter is named");
          Alcotest.test_case "a bound beside [@with d]" `Quick
            (refused
               "type t = { a : int [@with d] [@min 1] } [@@deriving wiretype]"
               "is bounded by d");
          Alcotest.test_case "an item bound on what is no list" `Quick
            (refused "type t = { a : int [@min_items 1] } [@@deriving wiretype]"
               "[@min_items] and [@max_items] bound a list");
          Alcotest.test_case "two polymorphic tags written alike" `Quick
            (refused "type t = [ `A | `B [@name \"a\"] ] [@@deriving wiretype]"
               "two constructors are written \"a\"");
        ] );
      ( "accepted",
        [
          Alcotest.test_case "a signature" `Quick (fun () ->
              let signature =
                Driver.map_signature
                  (Parse.interface
                     (Lexing.from_string
                        "type 'a t = { a : 'a } [@@deriving wiretype]\n\
                         type u = A | B [@@deriving wiretype]"))
              in
              let printed =
                Format.asprintf "%a" Pprintast.signature signature
              in
              List.iter
                (fun sub ->
                  Alcotest.(check bool) sub true (contains printed sub))
                [
                  "val json : 'a Wiretype.t -> 'a t Wiretype.t";
                  "val u_json : u Wiretype.t";
                ]);
          Alcotest.test_case "a tuple" `Quick
            (accepted
               "type t = { p : int * string * float } [@@deriving wiretype]");
          Alcotest.test_case "a map, on its field and on its type" `Quick
            (accepted
               "type t = { a : (string * int) list [@dict] [@max_properties \
                3]; b : ((string * int) list[@dict]) option } [@@deriving \
                wiretype]");
          Alcotest.test_case "an option that is read-only, with examples" `Quick
            (accepted
               "type t = { a : string option; [@read_only] [@examples [ \"x\" \
                ]] } [@@deriving wiretype]");
          Alcotest.test_case "a deprecated member" `Quick
            (accepted
               "type t = { a : int; [@wiretype.deprecated] b : int } \
                [@@deriving wiretype]");
          Alcotest.test_case "another deriver's attributes" `Quick
            (accepted
               "type t = { a : int [@yojson.key \"b\"] [@ocaml.warning \
                \"-32\"] } [@@deriving wiretype]");
        ] );
    ]
