(* The deriver's refusals: a type with no JSON shape is refused where it is
   written, saying what to write instead. Each snippet is parsed and expanded
   in-process by the deriver this executable links, which reports a refusal
   as an error in its output. *)

open Ppxlib

(* Every error the expansion holds, as its message. *)
let errors =
  object
    inherit [string list] Ast_traverse.fold as super

    method! extension ext acc =
      match ext with
      | ( { txt = "ocaml.error"; _ },
          PStr
            [
              {
                pstr_desc =
                  Pstr_eval
                    ( { pexp_desc = Pexp_constant (Pconst_string (m, _, _)); _ },
                      _ );
                _;
              };
            ] ) ->
          m :: acc
      | _ -> super#extension ext acc
  end

let expand source =
  let structure = Parse.implementation (Lexing.from_string source) in
  match Driver.map_structure structure with
  | expanded -> (
      match errors#structure expanded [] with
      | [] -> Ok (Format.asprintf "%a" Pprintast.structure expanded)
      | ms -> Error (String.concat "\n" (List.rev ms)))
  | exception Location.Error e -> Error (Location.Error.message e)

let contains s sub =
  let n = String.length s and m = String.length sub in
  let rec at i =
    i + m <= n && (String.equal (String.sub s i m) sub || at (i + 1))
  in
  at 0

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
                (List.length (errors#structure structure []));
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
          Alcotest.test_case "an option that is read-only, with examples" `Quick
            (accepted
               "type t = { a : string option; [@read_only] [@examples [ \"x\" \
                ]] } [@@deriving wiretype]");
          Alcotest.test_case "a deprecated member" `Quick
            (accepted
               "type t = { a : int; [@wiretype.deprecated] b : int } \
                [@@deriving wiretype]");
        ] );
    ]
