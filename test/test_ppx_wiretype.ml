(* The deriver's refusals: a type with no JSON shape is refused where it is
   written, saying what to write instead. Each snippet is parsed and expanded
   in-process by the deriver this executable links, which reports a refusal
   as an error in its output. *)

open Ppxlib

let expand source =
  let structure = Parse.implementation (Lexing.from_string source) in
  match Driver.map_structure structure with
  | expanded -> Ok (Format.asprintf "%a" Pprintast.structure expanded)
  | exception Location.Error e -> Error (Location.Error.message e)

let contains s sub =
  let n = String.length s and m = String.length sub in
  let rec at i =
    i + m <= n && (String.equal (String.sub s i m) sub || at (i + 1))
  in
  at 0

(* An error is either raised or embedded in the output as [%ocaml.error]. *)
let refused source message () =
  let said = match expand source with Ok out -> out | Error m -> m in
  if not (contains said message) then
    Alcotest.failf "expected %S in:\n%s" message said

let accepted source () =
  match expand source with
  | Ok out when not (contains out "ocaml.error") -> ()
  | Ok out -> Alcotest.failf "refused:\n%s" out
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
               "write camel, kebab or snake");
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
        ] );
      ( "accepted",
        [
          Alcotest.test_case "a tuple" `Quick
            (accepted
               "type t = { p : int * string * float } [@@deriving wiretype]");
          Alcotest.test_case "a map, on its field and on its type" `Quick
            (accepted
               "type t = { a : (string * int) list [@dict] [@max_properties \
                3]; b : ((string * int) list[@dict]) option } [@@deriving \
                wiretype]");
          Alcotest.test_case "a deprecated member" `Quick
            (accepted
               "type t = { a : int; [@wiretype.deprecated] b : int } \
                [@@deriving wiretype]");
        ] );
    ]
