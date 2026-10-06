(* The Unicode properties an internationalised host is checked by, written
   as OCaml at build time from the files in unicode/, Unicode 15.0.0 as
   published: which code points a label may hold (IdnaMappingTable), which
   are combining marks (DerivedGeneralCategory), which may not be in NFC
   (DerivedNormalizationProps) and each one's bidirectional class
   (DerivedBidiClass). Each is sorted ranges, adjacent ones joined. *)

let dir = Sys.argv.(1)

let lines file =
  String.split_on_char '\n'
    (In_channel.with_open_bin (Filename.concat dir file) In_channel.input_all)

(* A line's fields, its comment dropped. *)
let fields line =
  let line =
    match String.index_opt line '#' with
    | Some i -> String.sub line 0 i
    | None -> line
  in
  List.map String.trim (String.split_on_char ';' line)

let range s =
  let hex h = int_of_string ("0x" ^ h) in
  match String.split_on_char '.' s with
  | [ a ] -> (hex a, hex a)
  | [ a; ""; b ] -> (hex a, hex b)
  | _ -> failwith ("not a range: " ^ s)

(* Every range whose fields [keep] answers [Some v] for, as (lo, hi, v). *)
let ranges file keep =
  List.filter_map
    (fun line ->
      match fields line with
      | code :: rest when not (String.equal code "") -> (
          match keep rest with
          | Some v ->
              let lo, hi = range code in
              Some (lo, hi, v)
          | None -> None)
      | _ -> None)
    (lines file)

let joined rs =
  let rs = List.sort (fun (a, _, _) (b, _, _) -> Int.compare a b) rs in
  List.rev
    (List.fold_left
       (fun acc (lo, hi, v) ->
         match acc with
         | (plo, phi, pv) :: rest when phi + 1 = lo && pv = v ->
             (plo, hi, pv) :: rest
         | _ -> (lo, hi, v) :: acc)
       [] rs)

let print_pairs name rs =
  Printf.printf "let %s = [|\n" name;
  List.iter
    (fun (lo, hi, _) -> Printf.printf "  0x%X; 0x%X;\n" lo hi)
    (joined rs);
  print_string "|]\n\n"

let () =
  print_string
    "(* Written by gen/idna.exe from Unicode 15.0.0's data: sorted ranges, lo \
     then hi. *)\n\n";
  (* A label's code points once decoded: valid, a deviation, or valid but for
     the ASCII rules a browser does not apply. *)
  print_pairs "valid"
    (ranges "IdnaMappingTable.txt" (function
      | ("valid" | "deviation" | "disallowed_STD3_valid") :: _ -> Some 0
      | _ -> None));
  print_pairs "mark"
    (ranges "DerivedGeneralCategory.txt" (function
      | [ ("Mn" | "Mc" | "Me") ] -> Some 0
      | _ -> None));
  print_pairs "not_nfc"
    (ranges "DerivedNormalizationProps.txt" (function
      | [ "NFC_QC"; ("N" | "M") ] -> Some 0
      | _ -> None));
  (* Every class but L, which is what a code point listed nowhere is. *)
  let code = function
    | "R" -> Some 1
    | "AL" -> Some 2
    | "AN" -> Some 3
    | "EN" -> Some 4
    | "ES" -> Some 5
    | "CS" -> Some 6
    | "ET" -> Some 7
    | "ON" -> Some 8
    | "BN" -> Some 9
    | "NSM" -> Some 10
    | "L" -> None
    | _ -> Some 11
  in
  print_string "let bidi = [|\n";
  List.iter
    (fun (lo, hi, c) -> Printf.printf "  0x%X; 0x%X; %d;\n" lo hi c)
    (joined
       (ranges "DerivedBidiClass.txt" (function [ c ] -> code c | _ -> None)));
  print_string "|]\n"
