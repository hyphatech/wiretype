(* Reading a document by its description, in one pass and with no tree: the
   text is read with the description in hand and the value built as it goes.
   A value of the wrong sort is skipped -- the lexer reads past any
   well-formed value -- and its problem recorded where it is, so a document
   answers every problem it has; a document that is not JSON, or nested past
   the limit, is that one problem alone, since what was found before it
   depends on how far one pass had read: a union reads none of its members
   until its object has ended.

   A union's members are read in two passes, because its tag may come after
   the members it decides: the first finds where each member's value starts,
   and the second reads each from there once the tag has chosen a case. *)

module S = Shape
module P = Problem
module Names = Set.Make (String)

type st = {
  s : string;
  len : int;
  mutable i : int;
  mutable problems : P.t list;  (** newest first *)
  max_depth : int;
}

(* Raised with the problem that ends reading, and caught in [run]: it never
   leaves this module. *)
exception Stop of P.t

type path = P.segment list (* innermost first *)

let report st (path : path) code message =
  st.problems <- { P.at = List.rev path; code; message } :: st.problems

let fatal path code message =
  raise (Stop { P.at = List.rev path; code; message })

let at_end st = st.i >= st.len

(* A NUL byte where a structural character is expected is refused whatever
   it is taken for, so it stands for the end of the text. *)
let peek st = if st.i < st.len then st.s.[st.i] else '\000'

let found st =
  if at_end st then "the end of the text"
  else
    match peek st with
    | c when Char.code c < 0x20 || Char.code c >= 0x7F ->
        Printf.sprintf "the byte 0x%02X" (Char.code c)
    | c -> Printf.sprintf "'%c'" c

let syntax st path what =
  fatal path P.Syntax
    (Printf.sprintf
       "This is not JSON: %s was expected at byte %d, and %s was found." what
       st.i (found st))

let rec ws st =
  if st.i < st.len then
    match st.s.[st.i] with
    | ' ' | '\t' | '\n' | '\r' ->
        st.i <- st.i + 1;
        ws st
    | _ -> ()

let literal st path word =
  let n = String.length word in
  let rec matches k =
    k = n || (Char.equal st.s.[st.i + k] word.[k] && matches (k + 1))
  in
  if st.i + n <= st.len && matches 0 then st.i <- st.i + n
  else syntax st path ("'" ^ word ^ "'")

let enter st path depth =
  if depth >= st.max_depth then
    fatal path P.Too_deep
      (Printf.sprintf "This is nested more than %d deep." st.max_depth)

(* ------------------------------------------------------------------ *)
(* Strings *)

(* One character of UTF-8 at [st.i], which is at least 0x80, as RFC 3629
   has it: no overlong form, no surrogate, nothing past U+10FFFF. *)
let utf8 st path =
  let i = st.i in
  let byte k = if i + k < st.len then Char.code st.s.[i + k] else -1 in
  let cont k =
    let c = byte k in
    c >= 0x80 && c <= 0xBF
  in
  let between k lo hi =
    let c = byte k in
    c >= lo && c <= hi
  in
  let b0 = byte 0 in
  let n =
    if b0 >= 0xC2 && b0 <= 0xDF && cont 1 then 2
    else if b0 = 0xE0 && between 1 0xA0 0xBF && cont 2 then 3
    else if
      ((b0 >= 0xE1 && b0 <= 0xEC) || b0 = 0xEE || b0 = 0xEF) && cont 1 && cont 2
    then 3
    else if b0 = 0xED && between 1 0x80 0x9F && cont 2 then 3
    else if b0 = 0xF0 && between 1 0x90 0xBF && cont 2 && cont 3 then 4
    else if b0 >= 0xF1 && b0 <= 0xF3 && cont 1 && cont 2 && cont 3 then 4
    else if b0 = 0xF4 && between 1 0x80 0x8F && cont 2 && cont 3 then 4
    else 0
  in
  if n = 0 then syntax st path "text in UTF-8" else st.i <- i + n

let hex4 st path =
  let digit k =
    let c = st.s.[st.i + k] in
    match c with
    | '0' .. '9' -> Char.code c - 48
    | 'a' .. 'f' -> Char.code c - 87
    | 'A' .. 'F' -> Char.code c - 55
    | _ ->
        st.i <- st.i + k;
        syntax st path "a hexadecimal digit"
  in
  if st.i + 4 > st.len then syntax st path "four hexadecimal digits"
  else
    (* In order, so a bad digit is said at the first of them. *)
    let d0 = digit 0 in
    let d1 = digit 1 in
    let d2 = digit 2 in
    let d3 = digit 3 in
    st.i <- st.i + 4;
    (d0 lsl 12) lor (d1 lsl 8) lor (d2 lsl 4) lor d3

(* A string, at its opening quote. With [keep] it is built; without, it is
   only checked -- a value being skipped. Text with no escape is one copy. *)
let string_ st path ~keep =
  st.i <- st.i + 1;
  let start = st.i in
  let add_uchar b u = if keep then Buffer.add_utf_8_uchar b (Uchar.of_int u) in
  let escape b =
    if at_end st then syntax st path "an escape"
    else
      let c = st.s.[st.i] in
      st.i <- st.i + 1;
      match c with
      | '"' -> add_uchar b 0x22
      | '\\' -> add_uchar b 0x5C
      | '/' -> add_uchar b 0x2F
      | 'b' -> add_uchar b 0x08
      | 'f' -> add_uchar b 0x0C
      | 'n' -> add_uchar b 0x0A
      | 'r' -> add_uchar b 0x0D
      | 't' -> add_uchar b 0x09
      | 'u' ->
          let u = hex4 st path in
          if u >= 0xD800 && u <= 0xDBFF then
            if
              st.i + 1 < st.len
              && Char.equal st.s.[st.i] '\\'
              && Char.equal st.s.[st.i + 1] 'u'
            then (
              st.i <- st.i + 2;
              let lo = hex4 st path in
              if lo >= 0xDC00 && lo <= 0xDFFF then
                add_uchar b (0x10000 + ((u - 0xD800) lsl 10) + (lo - 0xDC00))
              else syntax st path "a low surrogate after a high one")
            else syntax st path "a low surrogate after a high one"
          else if u >= 0xDC00 && u <= 0xDFFF then
            syntax st path "a high surrogate before a low one"
          else add_uchar b u
      | _ ->
          st.i <- st.i - 1;
          syntax st path {|an escape: \" \\ \/ \b \f \n \r \t or \u|}
  in
  let rec escaped b =
    if at_end st then syntax st path "the '\"' that ends a string"
    else
      match st.s.[st.i] with
      | '"' ->
          st.i <- st.i + 1;
          if keep then Buffer.contents b else ""
      | '\\' ->
          st.i <- st.i + 1;
          escape b;
          escaped b
      | c when Char.code c < 0x20 ->
          syntax st path "an escape for a control character"
      | c when Char.code c < 0x80 ->
          if keep then Buffer.add_char b c;
          st.i <- st.i + 1;
          escaped b
      | _ ->
          let from = st.i in
          utf8 st path;
          if keep then Buffer.add_substring b st.s from (st.i - from);
          escaped b
  in
  let rec plain () =
    if at_end st then syntax st path "the '\"' that ends a string"
    else
      match st.s.[st.i] with
      | '"' ->
          let r = if keep then String.sub st.s start (st.i - start) else "" in
          st.i <- st.i + 1;
          r
      | '\\' ->
          let b = Buffer.create 64 in
          if keep then Buffer.add_substring b st.s start (st.i - start);
          escaped b
      | c when Char.code c < 0x20 ->
          syntax st path "an escape for a control character"
      | c when Char.code c < 0x80 ->
          st.i <- st.i + 1;
          plain ()
      | _ ->
          utf8 st path;
          plain ()
  in
  plain ()

(* ------------------------------------------------------------------ *)
(* Numbers *)

(* A number's text, RFC 8259 §6, and whether it is written as an integer --
   with no fraction and no exponent. The text is read by the description,
   so an integer is read from its digits and never through a double. *)
let number st path =
  let start = st.i in
  let digit () =
    st.i < st.len && match st.s.[st.i] with '0' .. '9' -> true | _ -> false
  in
  let digits what =
    if not (digit ()) then syntax st path what;
    while digit () do
      st.i <- st.i + 1
    done
  in
  if Char.equal (peek st) '-' then st.i <- st.i + 1;
  (match peek st with
  | '0' -> st.i <- st.i + 1
  | '1' .. '9' -> digits "a digit"
  | _ -> syntax st path "a digit");
  let integral = ref true in
  if Char.equal (peek st) '.' then (
    st.i <- st.i + 1;
    integral := false;
    digits "a digit after the decimal point");
  (match peek st with
  | 'e' | 'E' ->
      st.i <- st.i + 1;
      integral := false;
      (match peek st with '+' | '-' -> st.i <- st.i + 1 | _ -> ());
      digits "a digit of the exponent"
  | _ -> ());
  (String.sub st.s start (st.i - start), !integral)

let spelled f =
  let b = Buffer.create 24 in
  Text.number b f;
  Buffer.contents b

(* ------------------------------------------------------------------ *)
(* Skipping *)

let rec skip st path depth =
  ws st;
  match peek st with
  | '{' ->
      enter st path depth;
      st.i <- st.i + 1;
      ws st;
      if Char.equal (peek st) '}' then st.i <- st.i + 1
      else
        let rec members names =
          ws st;
          if not (Char.equal (peek st) '"') then
            syntax st path "a member's name";
          let name = string_ st path ~keep:true in
          let at = P.Member name :: path in
          let names =
            if Names.mem name names then (
              report st at P.Repeated_member
                "This member is given more than once.";
              names)
            else Names.add name names
          in
          ws st;
          if not (Char.equal (peek st) ':') then syntax st path "a ':'";
          st.i <- st.i + 1;
          skip st at (depth + 1);
          ws st;
          match peek st with
          | ',' ->
              st.i <- st.i + 1;
              members names
          | '}' -> st.i <- st.i + 1
          | _ -> syntax st path "a ',' or a '}'"
        in
        members Names.empty
  | '[' ->
      enter st path depth;
      st.i <- st.i + 1;
      ws st;
      if Char.equal (peek st) ']' then st.i <- st.i + 1
      else
        let rec items n =
          skip st (P.Index n :: path) (depth + 1);
          ws st;
          match peek st with
          | ',' ->
              st.i <- st.i + 1;
              items (n + 1)
          | ']' -> st.i <- st.i + 1
          | _ -> syntax st path "a ',' or a ']'"
        in
        items 0
  | '"' -> ignore (string_ st path ~keep:false : string)
  | 't' -> literal st path "true"
  | 'f' -> literal st path "false"
  | 'n' -> literal st path "null"
  | '-' | '0' .. '9' -> ignore (number st path : string * bool)
  | _ -> syntax st path "a value"

(* ------------------------------------------------------------------ *)
(* Any JSON *)

let rec json st path depth : Value.t =
  ws st;
  match peek st with
  | '{' ->
      enter st path depth;
      st.i <- st.i + 1;
      ws st;
      if Char.equal (peek st) '}' then (
        st.i <- st.i + 1;
        Value.Object [])
      else
        let rec members names acc =
          ws st;
          if not (Char.equal (peek st) '"') then
            syntax st path "a member's name";
          let name = string_ st path ~keep:true in
          let at = P.Member name :: path in
          ws st;
          if not (Char.equal (peek st) ':') then syntax st path "a ':'";
          st.i <- st.i + 1;
          let names, acc =
            if Names.mem name names then (
              report st at P.Repeated_member
                "This member is given more than once.";
              skip st at (depth + 1);
              (names, acc))
            else
              let v = json st at (depth + 1) in
              (Names.add name names, (name, v) :: acc)
          in
          ws st;
          match peek st with
          | ',' ->
              st.i <- st.i + 1;
              members names acc
          | '}' ->
              st.i <- st.i + 1;
              Value.Object (List.rev acc)
          | _ -> syntax st path "a ',' or a '}'"
        in
        members Names.empty []
  | '[' ->
      enter st path depth;
      st.i <- st.i + 1;
      ws st;
      if Char.equal (peek st) ']' then (
        st.i <- st.i + 1;
        Value.Array [])
      else
        let rec items n acc =
          let v = json st (P.Index n :: path) (depth + 1) in
          ws st;
          match peek st with
          | ',' ->
              st.i <- st.i + 1;
              items (n + 1) (v :: acc)
          | ']' ->
              st.i <- st.i + 1;
              Value.Array (List.rev (v :: acc))
          | _ -> syntax st path "a ',' or a ']'"
        in
        items 0 []
  | '"' -> Value.String (string_ st path ~keep:true)
  | 't' ->
      literal st path "true";
      Value.Bool true
  | 'f' ->
      literal st path "false";
      Value.Bool false
  | 'n' ->
      literal st path "null";
      Value.Null
  | '-' | '0' .. '9' ->
      let text, _ = number st path in
      let f = float_of_string text in
      if not (Float.is_finite f) then
        report st path
          (if f < 0. then P.Too_small else P.Too_large)
          "This number is too large to be read.";
      Value.Number f
  | _ -> syntax st path "a value"

let state ?(max_depth = Encode.max_depth) s =
  { s; len = String.length s; i = 0; problems = []; max_depth }

(* A value as the JSON it is written as: a union's tag, compared with the
   one a document gives; an example; [Wiretype.to_value]. What [Encode]
   writes is UTF-8, has no member twice and nests no deeper than this
   reads, so its text is always read back, and a refusal here would be the
   two disagreeing: it is said as the value's, with what was refused. *)
let written shape v =
  match Encode.run shape v with
  | Error e -> Error e
  | Ok s -> (
      let st = state s in
      let unspellable problems =
        Error
          {
            Unwritable.at = [];
            code = Unwritable.Unspellable;
            message = P.list_to_string problems;
          }
      in
      match json st [] 0 with
      | v when List.is_empty st.problems -> Ok v
      | _ -> unspellable (List.rev st.problems)
      | exception Stop p -> unspellable [ p ])

(* ------------------------------------------------------------------ *)
(* Reading by a description *)

let sort_found st =
  match peek st with
  | '{' -> "an object"
  | '[' -> "a list"
  | '"' -> "text"
  | 't' | 'f' -> "true or false"
  | 'n' -> "null"
  | '-' | '0' .. '9' -> "a number"
  | _ -> "something else"

let mismatch st path depth expected =
  report st path P.Unexpected_type
    (Printf.sprintf "This must be %s, not %s." expected (sort_found st));
  skip st path depth;
  None

let one_of words =
  match List.rev words with
  | [] -> "This must be one of no words at all."
  | [ w ] -> Printf.sprintf "This must be %s." w
  | last :: rest ->
      Printf.sprintf "This must be one of %s or %s."
        (String.concat ", " (List.rev rest))
        last

(* Every bound a value breaks, each its own problem. *)
let checked st path v checks =
  let broken =
    List.fold_left
      (fun broken (holds, code, message) ->
        if holds then broken
        else (
          report st path code (Lazy.force message);
          true))
      false checks
  in
  if broken then None else Some v

let bound opt f = match opt with None -> true | Some m -> f m

let past_type st path negative what =
  report st path
    (if negative then P.Too_small else P.Too_large)
    (Printf.sprintf "This is %s than %s can hold."
       (if negative then "smaller" else "larger")
       what);
  None

let int_ st path (b : S.int_bounds) =
  let text, integral = number st path in
  let negative = Char.equal text.[0] '-' in
  let n =
    if integral then
      match int_of_string_opt text with
      | Some n -> Some n
      | None -> past_type st path negative "a whole number here"
    else
      let f = float_of_string text in
      if not (Float.is_integer f) then (
        report st path P.Unexpected_type
          "This must be a whole number, not a fraction.";
        None)
      else if Float.abs f > 0x1p53 then
        past_type st path negative "a whole number written with a fraction"
      else Some (Float.to_int f)
  in
  Option.bind n (fun n ->
      checked st path n
        [
          ( bound b.min (fun m -> n >= m),
            P.Too_small,
            lazy
              (Printf.sprintf "This must be at least %d."
                 (Option.value b.min ~default:0)) );
          ( bound b.max (fun m -> n <= m),
            P.Too_large,
            lazy
              (Printf.sprintf "This must be at most %d."
                 (Option.value b.max ~default:0)) );
          ( bound b.multiple_of (fun m -> m = 0 || n mod m = 0),
            P.Not_a_multiple,
            lazy
              (Printf.sprintf "This must be a multiple of %d."
                 (Option.value b.multiple_of ~default:1)) );
        ])

let int64_ st path (b : S.int64_bounds) =
  let text, integral = number st path in
  let negative = Char.equal text.[0] '-' in
  let n =
    if integral then
      match Int64.of_string_opt text with
      | Some n -> Some n
      | None -> past_type st path negative "a 64-bit whole number"
    else
      let f = float_of_string text in
      if not (Float.is_integer f) then (
        report st path P.Unexpected_type
          "This must be a whole number, not a fraction.";
        None)
      else if Float.abs f > 0x1p53 then
        past_type st path negative "a whole number written with a fraction"
      else Some (Int64.of_float f)
  in
  let show o = Int64.to_string (Option.value o ~default:0L) in
  Option.bind n (fun n ->
      checked st path n
        [
          ( bound b.min (fun m -> Int64.compare n m >= 0),
            P.Too_small,
            lazy (Printf.sprintf "This must be at least %s." (show b.min)) );
          ( bound b.max (fun m -> Int64.compare n m <= 0),
            P.Too_large,
            lazy (Printf.sprintf "This must be at most %s." (show b.max)) );
          ( bound b.multiple_of (fun m ->
                Int64.equal m 0L || Int64.equal (Int64.rem n m) 0L),
            P.Not_a_multiple,
            lazy
              (Printf.sprintf "This must be a multiple of %s."
                 (show b.multiple_of)) );
        ])

(* zod's own check, so a browser and the server decide alike: the quotient
   within four epsilons of a whole number, scaled by its size, since the
   value and the step were each rounded to a double before the division
   rounded again -- 19.99 is a multiple of 0.01, though 19.99 /. 0.01 is
   1998.9999999999998. *)
let multiple f m =
  let ratio = f /. m in
  Float.abs (ratio -. Float.round ratio)
  < 4. *. epsilon_float *. Float.max (Float.abs ratio) 1.

let number_ st path (b : S.number_bounds) =
  let text, _ = number st path in
  let f = float_of_string text in
  if not (Float.is_finite f) then
    past_type st path (Char.equal text.[0] '-') "a number"
  else
    let show o = spelled (Option.value o ~default:0.) in
    checked st path f
      [
        ( bound b.min (fun m -> f >= m),
          P.Too_small,
          lazy (Printf.sprintf "This must be at least %s." (show b.min)) );
        ( bound b.max (fun m -> f <= m),
          P.Too_large,
          lazy (Printf.sprintf "This must be at most %s." (show b.max)) );
        ( bound b.above (fun m -> f > m),
          P.Too_small,
          lazy (Printf.sprintf "This must be greater than %s." (show b.above))
        );
        ( bound b.below (fun m -> f < m),
          P.Too_large,
          lazy (Printf.sprintf "This must be less than %s." (show b.below)) );
        ( bound b.multiple_of (multiple f),
          P.Not_a_multiple,
          lazy
            (Printf.sprintf "This must be a multiple of %s."
               (show b.multiple_of)) );
      ]

(* A string's length is its code points, as JSON Schema counts it. *)
let code_points s =
  String.fold_left
    (fun n c -> if Char.code c land 0xC0 = 0x80 then n else n + 1)
    0 s

let string_bounded st path (l : S.length) s =
  let n = lazy (code_points s) in
  let chars k =
    if k = 1 then "1 character" else Printf.sprintf "%d characters" k
  in
  checked st path s
    [
      ( bound l.min_length (fun m -> Lazy.force n >= m),
        P.Too_short,
        lazy
          (Printf.sprintf "This must be at least %s long."
             (chars (Option.value l.min_length ~default:0))) );
      ( bound l.max_length (fun m -> Lazy.force n <= m),
        P.Too_long,
        lazy
          (Printf.sprintf "This must be at most %s long."
             (chars (Option.value l.max_length ~default:0))) );
    ]

let sorts (a : _ S.any) =
  List.filter_map Fun.id
    [
      Option.map (fun _ -> "null") a.null;
      Option.map (fun _ -> "true or false") a.bool;
      Option.map (fun _ -> "a number") a.number;
      Option.map (fun _ -> "text") a.string;
      Option.map (fun _ -> "a list") a.array;
      Option.map (fun _ -> "an object") a.object_;
    ]

let either = function
  | [] -> "nothing at all"
  | [ w ] -> w
  | ws -> (
      match List.rev ws with
      | last :: rest -> String.concat ", " (List.rev rest) ^ " or " ^ last
      | [] -> "")

type 'a slot = Unset | Failed | Got of 'a
type setter = unit -> unit

(* A member an object describes, for one reading of it: what reads its
   value, and whether the document has named it yet -- which is how a
   member given twice is found without keeping every name. *)
type member = { name : string; set : setter; mutable seen : bool }

let member name set = { name; set; seen = false }

(* What a union's tag decides, once the members are known: the case's
   setters, what its object does with a member neither describes, and what
   fills the union's slot when they have run. *)
type hook = (string * int) list -> member list * S.unknown * (unit -> unit)

let rec value : type a. st -> path -> int -> a S.t -> a option =
 fun st path depth shape ->
  ws st;
  match shape with
  | S.Null v ->
      if Char.equal (peek st) 'n' then (
        literal st path "null";
        Some v)
      else mismatch st path depth "null"
  | S.Bool -> (
      match peek st with
      | 't' ->
          literal st path "true";
          Some true
      | 'f' ->
          literal st path "false";
          Some false
      | _ -> mismatch st path depth "true or false")
  | S.Int b -> (
      match peek st with
      | '-' | '0' .. '9' -> int_ st path b
      | _ -> mismatch st path depth "a whole number")
  | S.Int64 b -> (
      match peek st with
      | '-' | '0' .. '9' -> int64_ st path b
      | _ -> mismatch st path depth "a whole number")
  | S.Number b -> (
      match peek st with
      | '-' | '0' .. '9' -> number_ st path b
      | _ -> mismatch st path depth "a number")
  | S.String l ->
      if Char.equal (peek st) '"' then
        string_bounded st path l (string_ st path ~keep:true)
      else mismatch st path depth "text"
  | S.Enum (_, e) ->
      let words () = List.map (fun (w, _) -> Text.quote w) e.words in
      if Char.equal (peek st) '"' then (
        let w = string_ st path ~keep:true in
        match List.assoc_opt w e.words with
        | Some v -> Some v
        | None ->
            report st path P.Unknown_word (one_of (words ()));
            None)
      else mismatch st path depth ("one of " ^ either (words ()))
  | S.List l ->
      if Char.equal (peek st) '[' then list st path depth l
      else mismatch st path depth "a list"
  | S.Tuple items ->
      if Char.equal (peek st) '[' then tuple st path depth items
      else mismatch st path depth "a list"
  | S.Dict d ->
      if Char.equal (peek st) '{' then dict st path depth d
      else mismatch st path depth "an object"
  | S.Nullable t ->
      if Char.equal (peek st) 'n' then (
        literal st path "null";
        Some None)
      else Option.map Option.some (value st path depth t)
  | S.Object (_, o) ->
      if Char.equal (peek st) '{' then object_ st path depth o
      else mismatch st path depth "an object"
  | S.Any (_, a) -> (
      let pick =
        match peek st with
        | 'n' -> a.null
        | 't' | 'f' -> a.bool
        | '-' | '0' .. '9' -> a.number
        | '"' -> a.string
        | '[' -> a.array
        | '{' -> a.object_
        | _ -> None
      in
      match pick with
      | Some t -> value st path depth t
      | None -> mismatch st path depth (either (sorts a)))
  | S.Map (_, m) -> (
      match m.dec with
      | None ->
          report st path P.Malformed "This cannot be read here.";
          skip st path depth;
          None
      | Some dec -> (
          match value st path depth m.inner with
          | None -> None
          | Some x -> (
              match dec x with
              | Ok y -> Some y
              | Error message ->
                  report st path P.Malformed message;
                  None)))
  | S.Value -> Some (json st path depth)
  | S.Rec l -> value st path depth (Lazy.force l)

and list : type a. st -> path -> int -> a S.elements -> a list option =
 fun st path depth l ->
  enter st path depth;
  st.i <- st.i + 1;
  ws st;
  let items = ref [] and failed = ref false and n = ref 0 in
  (if Char.equal (peek st) ']' then st.i <- st.i + 1
   else
     let rec loop () =
       (match value st (P.Index !n :: path) (depth + 1) l.elt with
       | Some x -> items := x :: !items
       | None -> failed := true);
       incr n;
       ws st;
       match peek st with
       | ',' ->
           st.i <- st.i + 1;
           loop ()
       | ']' -> st.i <- st.i + 1
       | _ -> syntax st path "a ',' or a ']'"
     in
     loop ());
  let count k = if k = 1 then "1 item" else Printf.sprintf "%d items" k in
  let within =
    checked st path ()
      [
        ( bound l.min_items (fun m -> !n >= m),
          P.Too_few,
          lazy
            (Printf.sprintf "This must have at least %s."
               (count (Option.value l.min_items ~default:0))) );
        ( bound l.max_items (fun m -> !n <= m),
          P.Too_many,
          lazy
            (Printf.sprintf "This must have at most %s."
               (count (Option.value l.max_items ~default:0))) );
      ]
  in
  match within with
  | Some () when not !failed -> Some (List.rev !items)
  | Some () | None -> None

(* A map's members in the document's order, each value at its member and
   each name read by the key's description, what is wrong with a name said
   at the name. A name given twice is refused, as any object's is. *)
and dict : type k v. st -> path -> int -> (k, v) S.dict -> (k * v) list option =
 fun st path depth d ->
  enter st path depth;
  st.i <- st.i + 1;
  ws st;
  let items = ref [] and failed = ref false and n = ref 0 in
  (if Char.equal (peek st) '}' then st.i <- st.i + 1
   else
     let rec loop names =
       ws st;
       if not (Char.equal (peek st) '"') then syntax st path "a member's name";
       let name = string_ st path ~keep:true in
       ws st;
       if not (Char.equal (peek st) ':') then syntax st path "a ':'";
       st.i <- st.i + 1;
       let at = P.Member name :: path in
       let names =
         if Names.mem name names then (
           report st at P.Repeated_member "This member is given more than once.";
           skip st at (depth + 1);
           names)
         else
           let k = key st (P.Name name :: path) d.key name in
           (match (k, value st at (depth + 1) d.value) with
           | Some k, Some v -> items := (k, v) :: !items
           | _ -> failed := true);
           incr n;
           Names.add name names
       in
       ws st;
       match peek st with
       | ',' ->
           st.i <- st.i + 1;
           loop names
       | '}' -> st.i <- st.i + 1
       | _ -> syntax st path "a ',' or a '}'"
     in
     loop Names.empty);
  let count k = if k = 1 then "1 member" else Printf.sprintf "%d members" k in
  let within =
    checked st path ()
      [
        ( bound d.min_properties (fun m -> !n >= m),
          P.Too_few,
          lazy
            (Printf.sprintf "This must have at least %s."
               (count (Option.value d.min_properties ~default:0))) );
        ( bound d.max_properties (fun m -> !n <= m),
          P.Too_many,
          lazy
            (Printf.sprintf "This must have at most %s."
               (count (Option.value d.max_properties ~default:0))) );
      ]
  in
  match within with
  | Some () when not !failed -> Some (List.rev !items)
  | Some () | None -> None

(* A name, read by a description as the JSON string it was written as: text
   as it is, and anything else from the name quoted again, its problems
   joining the document's where the name is. *)
and key : type k. st -> path -> k S.t -> string -> k option =
 fun st at shape name ->
  match shape with
  | S.String l -> string_bounded st at l name
  | _ -> (
      let quoted = state ~max_depth:st.max_depth (Text.quote name) in
      match value quoted at 0 shape with
      | v ->
          st.problems <- quoted.problems @ st.problems;
          v
      | exception Stop p ->
          st.problems <- (p :: quoted.problems) @ st.problems;
          None)

(* A tuple's items, each read by its own description at its place; a list
   of another length is a problem of the list's, said once it has ended. *)
and tuple : type a. st -> path -> int -> (a, a) S.items -> a option =
 fun st path depth items ->
  enter st path depth;
  st.i <- st.i + 1;
  ws st;
  let setters, k = prepare_items st path depth items in
  let setters = Array.of_list setters in
  let arity = Array.length setters in
  let n = ref 0 in
  (if Char.equal (peek st) ']' then st.i <- st.i + 1
   else
     let rec loop () =
       if !n < arity then setters.(!n) ()
       else skip st (P.Index !n :: path) (depth + 1);
       incr n;
       ws st;
       match peek st with
       | ',' ->
           st.i <- st.i + 1;
           loop ()
       | ']' -> st.i <- st.i + 1
       | _ -> syntax st path "a ',' or a ']'"
     in
     loop ());
  if !n = arity then k ()
  else (
    report st path
      (if !n < arity then P.Too_few else P.Too_many)
      (Printf.sprintf "This must have exactly %d items." arity);
    None)

and prepare_items : type t f.
    st -> path -> int -> (t, f) S.items -> setter list * (unit -> f option) =
 fun st path depth items ->
  match items with
  | S.Items f -> ([], fun () -> Some f)
  | S.Item (prev, it) -> (
      let setters, k = prepare_items st path depth prev in
      let slot = ref Unset in
      let at = P.Index (List.length setters) :: path in
      let set () =
        slot :=
          match value st at (depth + 1) it.described with
          | Some v -> Got v
          | None -> Failed
      in
      ( setters @ [ set ],
        fun () ->
          let f = k () in
          match (f, !slot) with Some f, Got v -> Some (f v) | _ -> None ))

(* Each member's setter reads its value from where the text is, and a
   function of them builds the object once every member has been seen: what
   is missing is said then, in the order the members were declared. *)
and prepare : type o f.
    st ->
    path ->
    int ->
    (o, f) S.fields ->
    member list ->
    hook option ref ->
    member list * (unit -> f option) =
 fun st path depth fields acc hook ->
  match fields with
  | S.Build f -> (acc, fun () -> Some f)
  | S.Unread ->
      ( acc,
        fun () ->
          report st path P.Malformed "This is written and never read.";
          None )
  | S.Mem (prev, m) -> (
      let slot = ref Unset in
      let at = P.Member m.name :: path in
      let set () =
        slot :=
          match value st at (depth + 1) m.shape with
          | Some v -> Got v
          | None -> Failed
      in
      let acc, k = prepare st path depth prev (member m.name set :: acc) hook in
      ( acc,
        fun () ->
          let f = k () in
          let a =
            match !slot with
            | Got v -> Some v
            | Failed -> None
            | Unset -> (
                match m.absent with
                | Some v -> Some v
                | None ->
                    report st at P.Required "This is required.";
                    None)
          in
          match (f, a) with Some f, Some a -> Some (f a) | _ -> None ))
  | S.Cases (prev, cases) -> (
      let slot = ref Unset in
      hook := Some (fun members -> choose st path depth cases slot members);
      let acc, k = prepare st path depth prev acc hook in
      ( acc,
        fun () ->
          let f = k () in
          let c = match !slot with Got v -> Some v | Failed | Unset -> None in
          match (f, c) with Some f, Some c -> Some (f c) | _ -> None ))

and choose : type o c.
    st ->
    path ->
    int ->
    (o, c) S.cases ->
    c slot ref ->
    (string * int) list ->
    member list * S.unknown * (unit -> unit) =
 fun st path depth (S.Tagged t) slot members ->
  let at = P.Member t.name :: path in
  (* A tag that cannot be written is refused where the union is built. *)
  let tag_value tag =
    match written t.shape tag with Ok v -> v | Error _ -> Value.Null
  in
  let cases =
    List.map (fun (S.Case cm as c) -> (tag_value cm.tag, c)) t.cases
  in
  let given =
    match List.assoc_opt t.name members with
    | Some pos ->
        st.i <- pos;
        Some (json st at (depth + 1))
    | None -> Option.map tag_value t.absent
  in
  let the_tag = member t.name (fun () -> ()) in
  let refused () =
    slot := Failed;
    ([ the_tag ], S.Skip, fun () -> ())
  in
  match given with
  | None ->
      report st at P.Required "This is required.";
      refused ()
  | Some v -> (
      match List.find_opt (fun (tv, _) -> Value.equal tv v) cases with
      | None ->
          report st at P.Unknown_word
            (one_of (List.map (fun (tv, _) -> Value.to_string tv) cases));
          refused ()
      | Some (_, S.Case cm) -> (
          (* A case's own union is not read: an object has one union. *)
          let setters, k =
            prepare st path depth cm.obj.fields [ the_tag ] (ref None)
          in
          ( setters,
            cm.obj.unknown,
            fun () ->
              slot :=
                match (k (), cm.build) with
                | Some x, Some build -> Got (build x)
                | Some _, None ->
                    report st at P.Malformed
                      "This case is written and never read.";
                    Failed
                | None, (Some _ | None) -> Failed )))

and object_ : type o. st -> path -> int -> o S.obj -> o option =
 fun st path depth o ->
  enter st path depth;
  match Encode.find_cases o.fields with
  | None -> plain st path depth o
  | Some (Encode.Packed _) -> cased st path depth o

and unknown st at (policy : S.unknown) =
  match policy with
  | S.Skip -> ()
  | S.Refuse ->
      report st at P.Unknown_member "This is not a member this object has."

and plain : type o. st -> path -> int -> o S.obj -> o option =
 fun st path depth o ->
  st.i <- st.i + 1;
  ws st;
  let members, k = prepare st path depth o.fields [] (ref None) in
  (if Char.equal (peek st) '}' then st.i <- st.i + 1
   else
     (* Only a name the object does not describe is kept, to find it given
        twice; one it describes says so in its own [seen]. *)
     let rec loop unknowns =
       ws st;
       if not (Char.equal (peek st) '"') then syntax st path "a member's name";
       let found, name = name_of st path members in
       ws st;
       if not (Char.equal (peek st) ':') then syntax st path "a ':'";
       st.i <- st.i + 1;
       let unknowns =
         match found with
         | Some m when m.seen ->
             report st (P.Member m.name :: path) P.Repeated_member
               "This member is given more than once.";
             skip st (P.Member m.name :: path) (depth + 1);
             unknowns
         | Some m ->
             m.seen <- true;
             m.set ();
             unknowns
         | None ->
             let name = name () in
             let at = P.Member name :: path in
             if Names.mem name unknowns then (
               report st at P.Repeated_member
                 "This member is given more than once.";
               skip st at (depth + 1);
               unknowns)
             else (
               unknown st at o.unknown;
               skip st at (depth + 1);
               Names.add name unknowns)
       in
       ws st;
       match peek st with
       | ',' ->
           st.i <- st.i + 1;
           loop unknowns
       | '}' -> st.i <- st.i + 1
       | _ -> syntax st path "a ',' or a '}'"
     in
     loop Names.empty);
  k ()

(* A member's name, at its quote, matched against the members an object
   describes where it stands in the text: a name with no escape is never
   copied unless it is one the object does not describe, which is then
   made by the function answered beside it. *)
and name_of st path members =
  let start = st.i + 1 in
  let rec plain_to i =
    if i >= st.len then None
    else
      match String.unsafe_get st.s i with
      | '"' -> Some i
      | '\\' -> None
      | c when Char.code c < 0x20 || Char.code c >= 0x80 -> None
      | _ -> plain_to (i + 1)
  in
  match plain_to start with
  | Some stop ->
      let len = stop - start in
      let same m =
        String.length m.name = len
        &&
        let rec go k =
          k = len
          || Char.equal
               (String.unsafe_get st.s (start + k))
               (String.unsafe_get m.name k)
             && go (k + 1)
        in
        go 0
      in
      st.i <- stop + 1;
      (List.find_opt same members, fun () -> String.sub st.s start len)
  | None ->
      (* Escapes or text past ASCII: read as any string is, checked. *)
      let name = string_ st path ~keep:true in
      (List.find_opt (fun m -> String.equal m.name name) members, fun () -> name)

and cased : type o. st -> path -> int -> o S.obj -> o option =
 fun st path depth o ->
  st.i <- st.i + 1;
  ws st;
  let members =
    if Char.equal (peek st) '}' then (
      st.i <- st.i + 1;
      [])
    else
      let rec loop names acc =
        ws st;
        if not (Char.equal (peek st) '"') then syntax st path "a member's name";
        let name = string_ st path ~keep:true in
        let at = P.Member name :: path in
        ws st;
        if not (Char.equal (peek st) ':') then syntax st path "a ':'";
        st.i <- st.i + 1;
        ws st;
        let pos = st.i in
        (* What skipping finds inside a member is said once: by its reading
           when the object or its case describes it, and here when neither
           does. *)
        let before = st.problems in
        skip st at (depth + 1);
        let found = List.length st.problems - List.length before in
        let inside = List.filteri (fun i _ -> i < found) st.problems in
        st.problems <- before;
        let names, acc =
          if Names.mem name names then (
            report st at P.Repeated_member
              "This member is given more than once.";
            st.problems <- inside @ st.problems;
            (names, acc))
          else (Names.add name names, (name, pos, inside) :: acc)
        in
        ws st;
        match peek st with
        | ',' ->
            st.i <- st.i + 1;
            loop names acc
        | '}' ->
            st.i <- st.i + 1;
            List.rev acc
        | _ -> syntax st path "a ',' or a '}'"
      in
      loop Names.empty []
  in
  let stop = st.i in
  let hook = ref None in
  let base, k = prepare st path depth o.fields [] hook in
  let own, case_unknown, fill =
    match !hook with
    | Some h -> h (List.map (fun (name, pos, _) -> (name, pos)) members)
    | None -> ([], S.Skip, fun () -> ())
  in
  let setters = base @ own in
  (* A member neither describes is refused where either refuses one. *)
  let policy =
    match (o.unknown, case_unknown) with
    | S.Skip, S.Skip -> S.Skip
    | S.Refuse, _ | _, S.Refuse -> S.Refuse
  in
  List.iter
    (fun (name, pos, inside) ->
      match List.find_opt (fun m -> String.equal m.name name) setters with
      | Some m ->
          st.i <- pos;
          m.set ()
      | None ->
          unknown st (P.Member name :: path) policy;
          st.problems <- inside @ st.problems)
    members;
  fill ();
  st.i <- stop;
  k ()

let run ?max_depth shape s =
  let st = state ?max_depth s in
  let read () =
    let v = value st [] 0 shape in
    ws st;
    if not (at_end st) then syntax st [] "the end of the text";
    v
  in
  match read () with
  | exception Stop p -> Error [ p ]
  | v -> (
      match (v, st.problems) with
      | Some v, [] -> Ok v
      | None, [] ->
          Error
            [
              {
                P.at = [];
                code = P.Malformed;
                message = "This could not be read.";
              };
            ]
      | _, problems -> Error (List.rev problems))
