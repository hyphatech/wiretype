type segment = Member of string | Index of int | Name of string

type code =
  | Syntax
  | Too_deep
  | Required
  | Unexpected_type
  | Too_small
  | Too_large
  | Not_a_multiple
  | Too_short
  | Too_long
  | Too_few
  | Too_many
  | Unknown_word
  | Unknown_member
  | Repeated_member
  | Malformed

let code_to_string = function
  | Syntax -> "syntax"
  | Too_deep -> "too_deep"
  | Required -> "required"
  | Unexpected_type -> "unexpected_type"
  | Too_small -> "too_small"
  | Too_large -> "too_large"
  | Not_a_multiple -> "not_a_multiple"
  | Too_short -> "too_short"
  | Too_long -> "too_long"
  | Too_few -> "too_few"
  | Too_many -> "too_many"
  | Unknown_word -> "unknown_word"
  | Unknown_member -> "unknown_member"
  | Repeated_member -> "repeated_member"
  | Malformed -> "malformed"

type t = { at : segment list; code : code; message : string }

(* What ends a line, or changes the order a reader sees text in, though it is
   not ASCII: the C1 controls, NEL among them, the line and paragraph
   separators, and Unicode's bidirectional controls (UAX #9). A log shows
   each as nothing, or acts on it. *)
let invisible u =
  (u >= 0x80 && u <= 0x9F)
  || u = 0x061C || u = 0x200E || u = 0x200F || u = 0x2028 || u = 0x2029
  || (u >= 0x202A && u <= 0x202E)
  || (u >= 0x2066 && u <= 0x2069)

let has_invisible name =
  let rec from i =
    i < String.length name
    &&
    let d = String.get_utf_8_uchar name i in
    Uchar.utf_decode_is_valid d
    && invisible (Uchar.to_int (Uchar.utf_decode_uchar d))
    || from (i + Uchar.utf_decode_length d)
  in
  from 0

(* A name is written bare where nothing in it could be read as a path's own
   punctuation or end a log line, and quoted as JSON otherwise, so a path is
   one line and two paths are never spelt alike. *)
let plain name =
  (not (String.equal name ""))
  && String.for_all
       (function
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' | '$' -> true
         | c -> Char.code c >= 0x80)
       name
  && not (has_invisible name)

(* Quoted as JSON, and what is invisible written as JSON's escape for it,
   which reads back as the same name. *)
let quoted b name =
  let q = Text.quote name in
  let rec from i =
    if i < String.length q then begin
      let d = String.get_utf_8_uchar q i in
      let n = Uchar.utf_decode_length d in
      let u = Uchar.to_int (Uchar.utf_decode_uchar d) in
      if Uchar.utf_decode_is_valid d && invisible u then
        Buffer.add_string b (Printf.sprintf "\\u%04X" u)
      else Buffer.add_substring b q i n;
      from (i + n)
    end
  in
  from 0

let path ?(root = "") at =
  let b = Buffer.create 32 in
  Buffer.add_string b root;
  let name n =
    if not (plain n) then (
      Buffer.add_char b '[';
      quoted b n;
      Buffer.add_char b ']')
    else (
      if Buffer.length b > 0 then Buffer.add_char b '.';
      Buffer.add_string b n)
  in
  List.iter
    (function
      | Member n -> name n
      | Name n ->
          name n;
          Buffer.add_string b "[name]"
      | Index i -> Buffer.add_string b (Printf.sprintf "[%d]" i))
    at;
  Buffer.contents b

let to_string p =
  Printf.sprintf "%s: %s (%s)"
    (match path p.at with "" -> "the document" | s -> s)
    p.message (code_to_string p.code)

let list_to_string ps = String.concat "; " (List.map to_string ps)
