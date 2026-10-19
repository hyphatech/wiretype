(* Printed by wiretype from test/fixtures/shapes.ml. Do not edit: run dune promote. *)

(* What every printed module opens with, before its types: a problem as the
   server says one, the kinds' types, and the readers and writers the
   printed code is made of. It is Melange over Js.Json, Js.Date and Js.Dict
   alone, so a printed module needs no library. *)

(** What is wrong with a document: where, a code to branch on, and a sentence
    about that one place, as the server reports them. *)
module Problem = struct
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
    | Too_many_problems

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
    | Too_many_problems -> "too_many_problems"

  type t = { at : string; code : code; message : string }
  (** [at] is the path from the root, as the server writes one:
      [items[2].count], or the empty string for the document itself. *)

  let to_string p =
    (match p.at with "" -> "the document" | s -> s)
    ^ ": " ^ p.message ^ " (" ^ code_to_string p.code ^ ")"
end

(* JavaScript's own, where OCaml's standard library has no word for it. *)
external char_code_at : string -> int -> float = "charCodeAt" [@@mel.send]
external code_point_at : string -> int -> float = "codePointAt" [@@mel.send]
external is_integer : float -> bool = "isInteger" [@@mel.scope "Number"]
external is_finite : float -> bool = "isFinite" [@@mel.scope "Number"]
external js_string : float -> string = "String"

external set_utc_full_year : Js.Date.t -> float -> float -> float -> float
  = "setUTCFullYear"
[@@mel.send]

external set_utc_hours : Js.Date.t -> float -> float -> float -> float -> float
  = "setUTCHours"
[@@mel.send]

external utc_full_year : Js.Date.t -> float = "getUTCFullYear" [@@mel.send]
external utc_month : Js.Date.t -> float = "getUTCMonth" [@@mel.send]
external utc_date : Js.Date.t -> float = "getUTCDate" [@@mel.send]
external utc_hours : Js.Date.t -> float = "getUTCHours" [@@mel.send]
external utc_minutes : Js.Date.t -> float = "getUTCMinutes" [@@mel.send]
external utc_seconds : Js.Date.t -> float = "getUTCSeconds" [@@mel.send]

external utc_milliseconds : Js.Date.t -> float = "getUTCMilliseconds"
[@@mel.send]

external url_can_parse : string -> bool = "canParse" [@@mel.scope "URL"]
external to_lower_case : string -> string = "toLowerCase" [@@mel.send]
external slice : string -> int -> int -> string = "slice" [@@mel.send]
external string_includes : string -> string -> bool = "includes" [@@mel.send]
external split : string -> string -> string array = "split" [@@mel.send]
external js_array_from : string -> string array = "from" [@@mel.scope "Array"]
external pad_start : string -> int -> string -> string = "padStart" [@@mel.send]
external to_radix : int -> int -> string = "toString" [@@mel.send]

let digit c = c >= 48. && c <= 57.
let hex c = digit c || (c >= 97. && c <= 102.) || (c >= 65. && c <= 70.)
let alpha c = (c >= 97. && c <= 122.) || (c >= 65. && c <= 90.)

(* [n] digits of [s] at [i], as a number, or -1. *)
let fixed s i n =
  if i + n > String.length s then -1.
  else
    let rec go k acc =
      if k = n then acc
      else
        let c = char_code_at s (i + k) in
        if digit c then go (k + 1) ((acc *. 10.) +. c -. 48.) else -1.
    in
    go 0 0.

let leap y =
  (Float.rem y 4. = 0. && Float.rem y 100. <> 0.) || Float.rem y 400. = 0.

let days_in_month y m =
  if m = 2. then if leap y then 29. else 28.
  else if m = 4. || m = 6. || m = 9. || m = 11. then 30.
  else 31.

let date_at s i =
  if
    i + 10 > String.length s
    || char_code_at s (i + 4) <> 45.
    || char_code_at s (i + 7) <> 45.
  then None
  else
    let y = fixed s i 4 and m = fixed s (i + 5) 2 and d = fixed s (i + 8) 2 in
    if y < 0. || m < 1. || m > 12. || d < 1. || d > days_in_month y m then None
    else Some (y, m, d)

let two n = pad_start (js_string n) 2 "0"
let four n = pad_start (js_string n) 4 "0"

(** An instant, as RFC 3339 writes one: read with any fraction and offset, held
    to the millisecond, written in UTC with milliseconds. *)
module Instant : sig
  type t

  val to_date : t -> Js.Date.t

  val of_date : Js.Date.t -> t option
  (** [None] for an invalid date, and for one outside the years 0000 to 9999,
      which the server refuses. *)

  val to_string : t -> string
  val of_string : string -> t option
end = struct
  type t = Js.Date.t

  let to_date t = t

  let of_date d =
    let ms = Js.Date.getTime d in
    if is_finite ms && utc_full_year d >= 0. && utc_full_year d <= 9999. then
      Some d
    else None

  let to_string t =
    four (utc_full_year t)
    ^ "-"
    ^ two (utc_month t +. 1.)
    ^ "-"
    ^ two (utc_date t)
    ^ "T"
    ^ two (utc_hours t)
    ^ ":"
    ^ two (utc_minutes t)
    ^ ":"
    ^ two (utc_seconds t)
    ^ "."
    ^ pad_start (js_string (utc_milliseconds t)) 3 "0"
    ^ "Z"

  let of_string s =
    let n = String.length s in
    match date_at s 0 with
    | None -> None
    | Some (y, mo, d) -> (
        if
          n < 20
          || char_code_at s 10 <> 84.
          || char_code_at s 13 <> 58.
          || char_code_at s 16 <> 58.
        then None
        else
          let h = fixed s 11 2 and mi = fixed s 14 2 and sec = fixed s 17 2 in
          if h < 0. || h > 23. || mi < 0. || mi > 59. || sec < 0. || sec > 59.
          then None
          else
            (* A fraction is read to the millisecond and the rest dropped. *)
            let i, ms =
              if n > 19 && char_code_at s 19 = 46. then (
                let j = ref 20 in
                while !j < n && digit (char_code_at s !j) do
                  incr j
                done;
                let digits = slice s 20 !j in
                if String.length digits = 0 then (!j, -1.)
                else (!j, fixed (digits ^ "00") 0 3))
              else (19, 0.)
            in
            let offset =
              if ms < 0. then None
              else if i = n - 1 && char_code_at s i = 90. then Some 0.
              else if
                i = n - 6
                && (char_code_at s i = 43. || char_code_at s i = 45.)
                && char_code_at s (i + 3) = 58.
              then
                let oh = fixed s (i + 1) 2 and om = fixed s (i + 4) 2 in
                if oh < 0. || oh > 23. || om < 0. || om > 59. then None
                else
                  let minutes = (oh *. 60.) +. om in
                  Some (if char_code_at s i = 43. then minutes else -.minutes)
              else None
            in
            match offset with
            | None -> None
            | Some minutes ->
                (* Set field by field: Date.UTC reads a year below 100 as
                   one of the 1900s. *)
                let date = Js.Date.fromFloat 0. in
                let (_ : float) = set_utc_full_year date y (mo -. 1.) d in
                let (_ : float) = set_utc_hours date h mi sec ms in
                of_date
                  (Js.Date.fromFloat
                     (Js.Date.getTime date -. (minutes *. 60000.))))
end

(** A duration, as ISO 8601 writes one without years or months, held in
    milliseconds. *)
module Duration : sig
  type t

  val to_ms : t -> float

  val of_ms : float -> t option
  (** [None] for a fraction, a negative number, and one longer than the server
      holds, about 584 years. *)

  val to_string : t -> string
  val of_string : string -> (t, string) result
end = struct
  type t = float

  let day_ms = 86_400_000.

  (* 2^64 - 1 nanoseconds in whole milliseconds, the longest the server
     reads. *)
  let longest_ms = 18_446_744_073_709.
  let to_ms t = t

  let of_ms f =
    if is_integer f && f >= 0. && f <= longest_ms then Some f else None

  let not_a_duration =
    "This must be a duration written as ISO 8601 has it, like PT1H30M: weeks, \
     or days, hours, minutes and seconds, and never years or months."

  let of_string s =
    let n = String.length s in
    let i = ref 1 in
    let number () =
      let start = !i in
      while !i < n && digit (char_code_at s !i) do
        incr i
      done;
      if !i = start || !i - start > 16 then None
      else Some (Js.Float.fromString (slice s start !i))
    in
    let fraction () =
      let c = char_code_at s !i in
      if !i < n && (c = 46. || c = 44.) then (
        incr i;
        let start = !i in
        while !i < n && digit (char_code_at s !i) do
          incr i
        done;
        if !i = start then None else Some (fixed (slice s start !i ^ "00") 0 3))
      else Some 0.
    in
    let designator c =
      if !i < n && char_code_at s !i = c then (
        incr i;
        true)
      else false
    in
    let fail = Error not_a_duration in
    let scaled v unit =
      if v > longest_ms /. unit then longest_ms +. 1. else v *. unit
    in
    let total ms =
      if ms > longest_ms then Error "This duration is too long." else Ok ms
    in
    let time () =
      let components = ref 0 and ms = ref 0. in
      let step unit c =
        if !i < n && digit (char_code_at s !i) then
          let save = !i in
          match number () with
          | None -> false
          | Some v ->
              if designator c then (
                ms := !ms +. scaled v unit;
                incr components;
                true)
              else (
                i := save;
                true)
        else true
      in
      let seconds () =
        if !i < n && digit (char_code_at s !i) then
          match number () with
          | None -> false
          | Some v -> (
              match fraction () with
              | None -> false
              | Some frac ->
                  if designator 83. then (
                    ms := !ms +. scaled v 1000. +. frac;
                    incr components;
                    true)
                  else false)
        else true
      in
      if
        step 3_600_000. 72. && step 60_000. 77. && seconds () && !components > 0
      then Some !ms
      else None
    in
    if n < 2 || char_code_at s 0 <> 80. then fail
    else if char_code_at s 1 = 84. then (
      incr i;
      match time () with
      | Some ms when !i = n -> total ms
      | Some _ | None -> fail)
    else
      match number () with
      | None -> fail
      | Some v ->
          if designator 87. then
            if !i = n then total (scaled v (7. *. day_ms)) else fail
          else if designator 68. then
            let days = scaled v day_ms in
            if !i = n then total days
            else if designator 84. then
              match time () with
              | Some ms when !i = n -> total (days +. ms)
              | Some _ | None -> fail
            else fail
          else fail

  let to_string ms =
    let days = Float.trunc (ms /. day_ms) and rest = Float.rem ms day_ms in
    let h = Float.trunc (rest /. 3_600_000.)
    and m = Float.rem (Float.trunc (rest /. 60_000.)) 60.
    and s = Float.rem (Float.trunc (rest /. 1000.)) 60.
    and frac = Float.rem rest 1000. in
    let b = ref "P" in
    if days > 0. then b := !b ^ js_string days ^ "D";
    if rest > 0. || days = 0. then begin
      b := !b ^ "T";
      if h > 0. then b := !b ^ js_string h ^ "H";
      if m > 0. then b := !b ^ js_string m ^ "M";
      if s > 0. || frac > 0. || rest = 0. then begin
        b := !b ^ js_string s;
        if frac > 0. then begin
          let f = pad_start (js_string frac) 3 "0" in
          let len = ref 3 in
          while char_code_at f (!len - 1) = 48. do
            decr len
          done;
          b := !b ^ "." ^ slice f 0 !len
        end;
        b := !b ^ "S"
      end
    end;
    !b
end

(** A UUID, as RFC 9562 writes one, held in lower case. *)
module Uuid : sig
  type t

  val to_string : t -> string

  val of_string : string -> t option
  (** Read in either case: versions 1 to 8, the nil and the max UUID. *)

  val of_string_version : int -> string -> t option
  (** One version alone, as a description that names one reads it. *)
end = struct
  type t = string

  let to_string t = t

  let check version s =
    let shaped =
      String.length s = 36
      &&
      let ok = ref true in
      for i = 0 to 35 do
        let c = char_code_at s i in
        ok :=
          !ok && if i = 8 || i = 13 || i = 18 || i = 23 then c = 45. else hex c
      done;
      !ok
    in
    let variant =
      shaped
      &&
      let c = char_code_at s 19 in
      c = 56. || c = 57. || c = 97. || c = 98. || c = 65. || c = 66.
    in
    let special =
      s = "00000000-0000-0000-0000-000000000000"
      || s = "ffffffff-ffff-ffff-ffff-ffffffffffff"
    in
    shaped
    &&
    match version with
    | Some v -> char_code_at s 14 = 48. +. float_of_int v && variant
    | None ->
        special
        ||
        let c = char_code_at s 14 in
        c >= 49. && c <= 56. && variant

  let of_string s = if check None s then Some (to_lower_case s) else None

  let of_string_version v s =
    if check (Some v) s then Some (to_lower_case s) else None
end

(* ------------------------------------------------------------------ *)
(* Reading and writing, as the printed code calls them *)

(** The readers and writers a printed module is made of. Nothing here is an
    island's to call: the functions printed for each component are. *)
module Wire = struct
  type segment = Member of string | Index of int | Name of string

  type path = segment list
  (** The innermost segment first. *)

  exception Enough

  type ctx = {
    mutable problems : Problem.t list;
    mutable listed : int;
    limit : int;
  }

  type 'a reader = ctx -> path -> Js.Json.t -> 'a option
  type 'a writer = ctx -> path -> 'a -> Js.Json.t option

  let create () = { problems = []; listed = 0; limit = 100 }

  (* A code point a path quotes, as the server does: one that would end a log
     line or reorder the text around it. *)
  let invisible u =
    (u >= 0x80 && u <= 0x9F)
    || u = 0x2028 || u = 0x2029 || u = 0x61C || u = 0x200E || u = 0x200F
    || (u >= 0x202A && u <= 0x202E)
    || (u >= 0x2066 && u <= 0x2069)

  let plain name =
    name <> ""
    &&
    let ok = ref true in
    Array.iter
      (fun ch ->
        let c = code_point_at ch 0 in
        ok :=
          !ok
          && (alpha c || digit c || c = 95. || c = 45. || c = 36.
             || (c >= 128. && not (invisible (int_of_float c)))))
      (js_array_from name);
    !ok

  (* Quoted as JSON, with what is invisible written as JSON's escape. *)
  let quoted name =
    let b = ref "\"" in
    Array.iter
      (fun ch ->
        let c = int_of_float (code_point_at ch 0) in
        b :=
          !b
          ^
          if c = 34 then "\\\""
          else if c = 92 then "\\\\"
          else if c = 10 then "\\n"
          else if c = 13 then "\\r"
          else if c = 9 then "\\t"
          else if c < 0x20 || c = 0x7F || invisible c then
            "\\u" ^ pad_start (to_lower_case (to_radix c 16)) 4 "0"
          else ch)
      (js_array_from name);
    !b ^ "\""

  let path_to_string (path : path) =
    let b = ref "" in
    let name n =
      if plain n then (
        if !b <> "" then b := !b ^ ".";
        b := !b ^ n)
      else b := !b ^ "[" ^ quoted n ^ "]"
    in
    List.iter
      (function
        | Member n -> name n
        | Name n ->
            name n;
            b := !b ^ "[name]"
        | Index i -> b := !b ^ "[" ^ string_of_int i ^ "]")
      (List.rev path);
    !b

  let report ctx path code message =
    if ctx.listed >= ctx.limit then raise Enough;
    ctx.problems <-
      { Problem.at = path_to_string path; code; message } :: ctx.problems;
    ctx.listed <- ctx.listed + 1

  let sort_found j =
    match Js.Json.classify j with
    | JSONObject _ -> "an object"
    | JSONArray _ -> "a list"
    | JSONString _ -> "text"
    | JSONTrue | JSONFalse -> "true or false"
    | JSONNull -> "null"
    | JSONNumber _ -> "a number"

  let mismatch ctx path expected j =
    report ctx path Unexpected_type
      ("This must be " ^ expected ^ ", not " ^ sort_found j ^ ".");
    None

  let either = function
    | [] -> "nothing at all"
    | [ w ] -> w
    | ws ->
        let rec last = function
          | [ w ] -> ([], w)
          | w :: rest ->
              let init, l = last rest in
              (w :: init, l)
          | [] -> ([], "")
        in
        let init, l = last ws in
        String.concat ", " init ^ " or " ^ l

  let quote_word w = Js.Json.stringify (Js.Json.string w)

  let one_of words =
    match words with
    | [] -> "This must be one of no words at all."
    | [ w ] -> "This must be " ^ w ^ "."
    | ws -> "This must be one of " ^ either ws ^ "."

  (* Every bound a value breaks, each its own problem. *)
  let checked ctx path v broken =
    match broken with
    | [] -> Some v
    | broken ->
        List.iter (fun (code, message) -> report ctx path code message) broken;
        None

  let some f = function None -> [] | Some x -> f x

  (* A number as the server spells one in a sentence: a whole one with no
     point. *)
  let spelled f = js_string f

  (* Zod's own check, as the server has it: the quotient within four
     epsilons of a whole number, scaled by its size. *)
  let multiple f m =
    let ratio = f /. m in
    Float.abs (ratio -. Float.round ratio)
    < 4. *. epsilon_float *. Float.max (Float.abs ratio) 1.

  let number_bounds ~min ~max ~above ~below ~multiple_of f =
    some
      (fun m ->
        if not (f >= m) then
          [ (Problem.Too_small, "This must be at least " ^ spelled m ^ ".") ]
        else [])
      min
    @ some
        (fun m ->
          if not (f <= m) then
            [ (Problem.Too_large, "This must be at most " ^ spelled m ^ ".") ]
          else [])
        max
    @ some
        (fun m ->
          if not (f > m) then
            [
              (Problem.Too_small, "This must be greater than " ^ spelled m ^ ".");
            ]
          else [])
        above
    @ some
        (fun m ->
          if not (f < m) then
            [ (Problem.Too_large, "This must be less than " ^ spelled m ^ ".") ]
          else [])
        below
    @ some
        (fun m ->
          if not (multiple f m) then
            [
              ( Problem.Not_a_multiple,
                "This must be a multiple of " ^ spelled m ^ "." );
            ]
          else [])
        multiple_of

  let whole_bounds ~min ~max ~multiple_of f =
    number_bounds ~min ~max ~above:None ~below:None ~multiple_of:None f
    @ some
        (fun m ->
          if Float.rem f m <> 0. then
            [
              ( Problem.Not_a_multiple,
                "This must be a multiple of " ^ spelled m ^ "." );
            ]
          else [])
        multiple_of

  let code_points s = Array.length (js_array_from s)

  let counted noun k =
    if k = 1 then "1 " ^ noun else string_of_int k ^ " " ^ noun ^ "s"

  let length_bounds ~min_length ~max_length s =
    match (min_length, max_length) with
    | None, None -> []
    | min, max ->
        let n = code_points s in
        some
          (fun m ->
            if n < m then
              [
                ( Problem.Too_short,
                  "This must be at least " ^ counted "character" m ^ " long." );
              ]
            else [])
          min
        @ some
            (fun m ->
              if n > m then
                [
                  ( Problem.Too_long,
                    "This must be at most " ^ counted "character" m ^ " long."
                  );
                ]
              else [])
            max

  let count_bounds noun ~min ~max n =
    some
      (fun m ->
        if n < m then
          [
            (Problem.Too_few, "This must have at least " ^ counted noun m ^ ".");
          ]
        else [])
      min
    @ some
        (fun m ->
          if n > m then
            [
              ( Problem.Too_many,
                "This must have at most " ^ counted noun m ^ "." );
            ]
          else [])
        max

  (* ---------------------------------------------------------------- *)
  (* Readers *)

  let any : Js.Json.t reader = fun _ _ j -> Some j

  let null : unit reader =
   fun ctx path j ->
    match Js.Json.classify j with
    | JSONNull -> Some ()
    | _ -> mismatch ctx path "null" j

  let bool : bool reader =
   fun ctx path j ->
    match Js.Json.classify j with
    | JSONTrue -> Some true
    | JSONFalse -> Some false
    | _ -> mismatch ctx path "true or false" j

  let number ?min ?max ?above ?below ?multiple_of () : float reader =
   fun ctx path j ->
    match Js.Json.classify j with
    | JSONNumber f ->
        checked ctx path f
          (number_bounds ~min ~max ~above ~below ~multiple_of f)
    | _ -> mismatch ctx path "a number" j

  let past_type ctx path negative what =
    report ctx path
      (if negative then Problem.Too_small else Problem.Too_large)
      ("This is "
      ^ (if negative then "smaller" else "larger")
      ^ " than " ^ what ^ " can hold.");
    None

  (* A whole number as a float, which holds one exactly to 2^53: past it the
     browser cannot tell which whole number it is, as Zod cannot. *)
  let whole ?min ?max ?multiple_of () : float reader =
   fun ctx path j ->
    match Js.Json.classify j with
    | JSONNumber f ->
        if not (is_integer f) then (
          report ctx path Unexpected_type
            "This must be a whole number, not a fraction.";
          None)
        else if Float.abs f >= 9007199254740992. then
          past_type ctx path (f < 0.) "a whole number here"
        else checked ctx path f (whole_bounds ~min ~max ~multiple_of f)
    | _ -> mismatch ctx path "a whole number" j

  (* An integer whose bounds lie within 32 bits, which Melange's int holds. *)
  let int ?min ?max ?multiple_of () : int reader =
   fun ctx path j ->
    match
      whole
        ?min:(Option.map float_of_int min)
        ?max:(Option.map float_of_int max)
        ?multiple_of:(Option.map float_of_int multiple_of)
        () ctx path j
    with
    | Some f -> Some (int_of_float f)
    | None -> None

  let text ctx path j =
    match Js.Json.classify j with
    | JSONString s -> Some s
    | _ -> mismatch ctx path "text" j

  let string ?min_length ?max_length () : string reader =
   fun ctx path j ->
    match text ctx path j with
    | Some s -> checked ctx path s (length_bounds ~min_length ~max_length s)
    | None -> None

  (* A kind over text: its length first, then its grammar, a refusal of
     which is [malformed] with the kind's sentence. *)
  let kind ?min_length ?max_length parse : 'a reader =
   fun ctx path j ->
    match string ?min_length ?max_length () ctx path j with
    | None -> None
    | Some s -> (
        match parse s with
        | Ok v -> Some v
        | Error message ->
            report ctx path Malformed message;
            None)

  let refusing message f s =
    match f s with Some v -> Ok v | None -> Error message

  let instant ?min_length ?max_length () =
    kind ?min_length ?max_length
      (refusing
         "This must be an instant written as RFC 3339 has it, like \
          2026-09-30T12:00:00Z."
         Instant.of_string)

  let date ?min_length ?max_length () =
    kind ?min_length ?max_length (fun s ->
        match date_at s 0 with
        | Some (y, m, d) when String.length s = 10 ->
            Ok (int_of_float y, int_of_float m, int_of_float d)
        | Some _ | None ->
            Error "This must be a date written as YYYY-MM-DD, like 2026-09-30.")

  let duration ?min_length ?max_length () =
    kind ?min_length ?max_length Duration.of_string

  let uuid ?min_length ?max_length ?version () =
    kind ?min_length ?max_length (fun s ->
        match version with
        | None ->
            refusing
              "This must be a UUID, like 01890a5d-ac96-7a3b-9e5a-5f1c2a7b8c9d."
              Uuid.of_string s
        | Some v ->
            refusing
              ("This must be a version " ^ string_of_int v
             ^ " UUID, like 01890a5d-ac96-7a3b-9e5a-5f1c2a7b8c9d.")
              (Uuid.of_string_version v) s)

  let sextet ~url c =
    if c >= 65. && c <= 90. then true
    else if c >= 97. && c <= 122. then true
    else if digit c then true
    else if url then c = 45. || c = 95.
    else c = 43. || c = 47.

  let base64_check ~url s =
    let n = String.length s in
    let body =
      if url then if n mod 4 = 1 then -1 else n
      else if n mod 4 <> 0 then -1
      else if
        n >= 2 && char_code_at s (n - 1) = 61. && char_code_at s (n - 2) = 61.
      then n - 2
      else if n >= 1 && char_code_at s (n - 1) = 61. then n - 1
      else n
    in
    body >= 0
    && body mod 4 <> 1
    &&
    let ok = ref true in
    for i = 0 to body - 1 do
      ok := !ok && sextet ~url (char_code_at s i)
    done;
    !ok

  let base64 ?min_length ?max_length () =
    kind ?min_length ?max_length (fun s ->
        if base64_check ~url:false s then Ok s
        else Error "This must be bytes in base64, padded to a multiple of four.")

  let base64url ?min_length ?max_length () =
    kind ?min_length ?max_length (fun s ->
        if base64_check ~url:true s then Ok s
        else Error "This must be bytes in base64url, unpadded.")

  (* Held to the browser's URL, as Zod's check is, and not to the server's
     grammar: the one kind the browser reads more loosely. *)
  let uri ?min_length ?max_length () =
    kind ?min_length ?max_length (fun s ->
        if url_can_parse s then Ok s
        else
          Error
            "This must be a URI with its scheme, like https://example.com/path.")

  let all_digits p =
    let ok = ref true in
    for i = 0 to String.length p - 1 do
      ok := !ok && digit (char_code_at p i)
    done;
    !ok

  let ipv4_check s =
    let parts = split s "." in
    Array.length parts = 4
    && Array.for_all
         (fun p ->
           let n = String.length p in
           n >= 1 && n <= 3 && all_digits p
           && (n = 1 || char_code_at p 0 <> 48.)
           && Js.Float.fromString p <= 255.)
         parts

  let ipv6_check s =
    let group g =
      let n = String.length g in
      n >= 1 && n <= 4
      &&
      let ok = ref true in
      for i = 0 to n - 1 do
        ok := !ok && hex (char_code_at g i)
      done;
      !ok
    in
    let groups ~last part =
      if part = "" then Some 0
      else
        let rec count n = function
          | [] -> Some n
          | [ g ] when last && string_includes g "." ->
              if ipv4_check g then Some (n + 2) else None
          | g :: rest -> if group g && n < 8 then count (n + 1) rest else None
        in
        count 0 (Array.to_list (split part ":"))
    in
    let n = String.length s in
    let rec split_double i =
      if i + 1 >= n then None
      else if char_code_at s i = 58. && char_code_at s (i + 1) = 58. then Some i
      else split_double (i + 1)
    in
    match split_double 0 with
    | None -> ( match groups ~last:true s with Some 8 -> true | _ -> false)
    | Some i -> (
        let left = slice s 0 i and right = slice s (i + 2) n in
        let clean part =
          not
            (String.length part > 0
            && (char_code_at part 0 = 58.
               || char_code_at part (String.length part - 1) = 58.))
        in
        if String.length right >= 1 && char_code_at right 0 = 58. then false
        else if not (clean left && clean right) then false
        else
          match (groups ~last:false left, groups ~last:true right) with
          | Some l, Some r -> l + r <= 7
          | _ -> false)

  let ipv4 ?min_length ?max_length () =
    kind ?min_length ?max_length (fun s ->
        if ipv4_check s then Ok s
        else Error "This must be an IPv4 address, like 192.0.2.1.")

  let ipv6 ?min_length ?max_length () =
    kind ?min_length ?max_length (fun s ->
        if ipv6_check s then Ok s
        else Error "This must be an IPv6 address, like 2001:db8::1.")

  (* An enum: its words quoted in a sentence, as the server quotes them. *)
  let enum words (read : string -> 'a option) : 'a reader =
   fun ctx path j ->
    let quoted () = List.map quote_word (Array.to_list words) in
    match Js.Json.classify j with
    | JSONString s -> (
        match read s with
        | Some v -> Some v
        | None ->
            report ctx path Unknown_word (one_of (quoted ()));
            None)
    | _ -> mismatch ctx path ("one of " ^ either (quoted ())) j

  let array ?min_items ?max_items (elt : 'a reader) : 'a array reader =
   fun ctx path j ->
    match Js.Json.classify j with
    | JSONArray items ->
        let failed = ref false in
        let out =
          Array.mapi
            (fun i x ->
              match elt ctx (Index i :: path) x with
              | Some v -> v
              | None ->
                  failed := true;
                  Obj.magic 0)
            items
        in
        let within =
          checked ctx path ()
            (count_bounds "item" ~min:min_items ~max:max_items
               (Array.length items))
        in
        if !failed || Option.is_none within then None else Some out
    | _ -> mismatch ctx path "a list" j

  (* A list of exactly [n] items, each read by the printed code at its
     index. *)
  let tuple n : Js.Json.t array reader =
   fun ctx path j ->
    match Js.Json.classify j with
    | JSONArray items ->
        if Array.length items = n then Some items
        else (
          report ctx path
            (if Array.length items < n then Too_few else Too_many)
            ("This must have exactly " ^ string_of_int n ^ " items.");
          None)
    | _ -> mismatch ctx path "a list" j

  let members : Js.Json.t Js.Dict.t reader =
   fun ctx path j ->
    match Js.Json.classify j with
    | JSONObject o -> Some o
    | _ -> mismatch ctx path "an object" j

  (* A map's members in the document's order, each name read by the key's
     description as the text it is, with what is wrong with a name said at
     the name. A key read as one another was is a name given twice. *)
  let dict ?min_properties ?max_properties (key : 'k reader) (value : 'v reader)
      : ('k * 'v) array reader =
   fun ctx path j ->
    match members ctx path j with
    | None -> None
    | Some o ->
        let failed = ref false in
        let seen = Js.Dict.empty () in
        let out =
          Array.map
            (fun (name, x) ->
              let at = Member name :: path in
              match key ctx (Name name :: path) (Js.Json.string name) with
              | None ->
                  failed := true;
                  Obj.magic 0
              | Some k ->
                  let spelling = Js.Json.stringify (Obj.magic k) in
                  if Option.is_some (Js.Dict.get seen spelling) then (
                    report ctx at Repeated_member
                      "This member is given more than once.";
                    failed := true;
                    Obj.magic 0)
                  else (
                    Js.Dict.set seen spelling true;
                    match value ctx at x with
                    | Some v -> (k, v)
                    | None ->
                        failed := true;
                        Obj.magic 0))
            (Js.Dict.entries o)
        in
        let within =
          checked ctx path ()
            (count_bounds "member" ~min:min_properties ~max:max_properties
               (Array.length out))
        in
        if !failed || Option.is_none within then None else Some out

  (* A member the object requires. *)
  let required (read : 'a reader) ctx path o name : 'a option =
    match Js.Dict.get o name with
    | Some x -> read ctx (Member name :: path) x
    | None ->
        report ctx (Member name :: path) Required "This is required.";
        None

  (* A member that may be left out: [Some None] when it is. *)
  let optional (read : 'a reader) ctx path o name : 'a option option =
    match Js.Dict.get o name with
    | Some x -> Option.map Option.some (read ctx (Member name :: path) x)
    | None -> Some None

  (* A member that may be left out or null, as [opt_mem] reads one. *)
  let optional_or_null (read : 'a reader) ctx path o name : 'a option option =
    match Js.Dict.get o name with
    | Some x -> (
        match Js.Json.classify x with
        | JSONNull -> Some None
        | _ -> Option.map Option.some (read ctx (Member name :: path) x))
    | None -> Some None

  (* A member the object does not describe, where it refuses them. *)
  let unknown ctx path o (names : string array) =
    Array.iter
      (fun name ->
        if not (Array.exists (String.equal name) names) then
          report ctx (Member name :: path) Unknown_member
            "This is not a member this object has.")
      (Js.Dict.keys o)

  (* A union's tag, as the JSON text it is, or [absent]'s where it is left
     out. *)
  let tag ctx path o name ~absent : string option =
    match Js.Dict.get o name with
    | Some x -> Some (Js.Json.stringify x)
    | None -> (
        match absent with
        | Some t -> Some t
        | None ->
            report ctx (Member name :: path) Required "This is required.";
            None)

  let unknown_tag ctx path name tags =
    report ctx (Member name :: path) Unknown_word (one_of tags);
    None

  let nullable (read : 'a reader) : 'a option reader =
   fun ctx path j ->
    match Js.Json.classify j with
    | JSONNull -> Some None
    | _ -> Option.map Option.some (read ctx path j)

  (* What a reading ends with: every problem, in the document's order. *)
  let finish ctx result =
    match result () with
    | exception Enough ->
        Error
          (List.rev
             ({
                Problem.at = "";
                code = Too_many_problems;
                message =
                  "This has more problems than the " ^ string_of_int ctx.limit
                  ^ " listed, and was read no further.";
              }
             :: ctx.problems))
    | Some v when List.is_empty ctx.problems -> Ok v
    | None when List.is_empty ctx.problems ->
        Error
          [
            {
              Problem.at = "";
              code = Malformed;
              message = "This could not be read.";
            };
          ]
    | Some _ | None -> Error (List.rev ctx.problems)

  let of_json (read : 'a reader) j =
    let ctx = create () in
    finish ctx (fun () -> read ctx [] j)

  let decode (read : 'a reader) s =
    match Js.Json.parseExn s with
    | j -> of_json read j
    | exception Js.Exn.Error e ->
        Error
          [
            {
              Problem.at = "";
              code = Syntax;
              message =
                "This is not JSON: "
                ^ Option.value (Js.Exn.message e)
                    ~default:"it could not be parsed"
                ^ ".";
            };
          ]

  (* ---------------------------------------------------------------- *)
  (* Writers *)

  let w_any : Js.Json.t writer = fun _ _ j -> Some j
  let w_null : unit writer = fun _ _ () -> Some Js.Json.null
  let w_bool : bool writer = fun _ _ b -> Some (Js.Json.boolean b)

  let w_number ?min ?max ?above ?below ?multiple_of () : float writer =
   fun ctx path f ->
    if not (is_finite f) then (
      report ctx path Malformed "This number is not finite.";
      None)
    else
      Option.map Js.Json.number
        (checked ctx path f
           (number_bounds ~min ~max ~above ~below ~multiple_of f))

  let w_whole ?min ?max ?multiple_of () : float writer =
   fun ctx path f ->
    if not (is_integer f) then (
      report ctx path Unexpected_type
        "This must be a whole number, not a fraction.";
      None)
    else if Float.abs f >= 9007199254740992. then
      past_type ctx path (f < 0.) "a whole number here"
    else
      Option.map Js.Json.number
        (checked ctx path f (whole_bounds ~min ~max ~multiple_of f))

  let w_int ?min ?max ?multiple_of () : int writer =
   fun ctx path n ->
    w_whole
      ?min:(Option.map float_of_int min)
      ?max:(Option.map float_of_int max)
      ?multiple_of:(Option.map float_of_int multiple_of)
      () ctx path (float_of_int n)

  let w_string ?min_length ?max_length () : string writer =
   fun ctx path s ->
    Option.map Js.Json.string
      (checked ctx path s (length_bounds ~min_length ~max_length s))

  let w_kind ?min_length ?max_length (print : 'a -> (string, string) result) :
      'a writer =
   fun ctx path v ->
    match print v with
    | Ok s -> w_string ?min_length ?max_length () ctx path s
    | Error message ->
        report ctx path Malformed message;
        None

  let w_instant ?min_length ?max_length () =
    w_kind ?min_length ?max_length (fun t -> Ok (Instant.to_string t))

  let w_date ?min_length ?max_length () =
    w_kind ?min_length ?max_length (fun (y, m, d) ->
        let yf = float_of_int y
        and mf = float_of_int m
        and df = float_of_int d in
        if
          yf < 0. || yf > 9999. || mf < 1. || mf > 12. || df < 1.
          || df > days_in_month yf mf
        then
          Error
            (string_of_int y ^ "-" ^ string_of_int m ^ "-" ^ string_of_int d
           ^ " is not a date of the years 0000 to 9999.")
        else Ok (four yf ^ "-" ^ two mf ^ "-" ^ two df))

  let w_duration ?min_length ?max_length () =
    w_kind ?min_length ?max_length (fun t -> Ok (Duration.to_string t))

  let w_uuid ?min_length ?max_length ?version () =
    w_kind ?min_length ?max_length (fun u ->
        let s = Uuid.to_string u in
        match version with
        | Some v when Option.is_none (Uuid.of_string_version v s) ->
            Error ("This UUID is not of version " ^ string_of_int v ^ ".")
        | Some _ | None -> Ok s)

  (* A kind the browser holds as its text, checked again on writing. *)
  let w_checked ?min_length ?max_length check message =
    w_kind ?min_length ?max_length (fun s ->
        if check s then Ok s else Error message)

  let w_base64 ?min_length ?max_length () =
    w_checked ?min_length ?max_length (base64_check ~url:false)
      "This must be bytes in base64, padded to a multiple of four."

  let w_base64url ?min_length ?max_length () =
    w_checked ?min_length ?max_length (base64_check ~url:true)
      "This must be bytes in base64url, unpadded."

  let w_uri ?min_length ?max_length () =
    w_checked ?min_length ?max_length url_can_parse
      "This must be a URI with its scheme, like https://example.com/path."

  let w_ipv4 ?min_length ?max_length () =
    w_checked ?min_length ?max_length ipv4_check
      "This must be an IPv4 address, like 192.0.2.1."

  let w_ipv6 ?min_length ?max_length () =
    w_checked ?min_length ?max_length ipv6_check
      "This must be an IPv6 address, like 2001:db8::1."

  let w_enum (word : 'a -> string) : 'a writer =
   fun _ _ v -> Some (Js.Json.string (word v))

  let w_array ?min_items ?max_items (elt : 'a writer) : 'a array writer =
   fun ctx path items ->
    let failed = ref false in
    let out =
      Array.mapi
        (fun i x ->
          match elt ctx (Index i :: path) x with
          | Some j -> j
          | None ->
              failed := true;
              Js.Json.null)
        items
    in
    let within =
      checked ctx path ()
        (count_bounds "item" ~min:min_items ~max:max_items (Array.length items))
    in
    if !failed || Option.is_none within then None else Some (Js.Json.array out)

  (* A tuple's items, each written by the printed code at its index. *)
  let w_tuple (items : Js.Json.t option array) =
    if Array.for_all Option.is_some items then
      Some (Js.Json.array (Array.map (fun j -> Option.get j) items))
    else None

  let w_dict ?min_properties ?max_properties (key : 'k writer)
      (value : 'v writer) : ('k * 'v) array writer =
   fun ctx path items ->
    let failed = ref false in
    let seen = Js.Dict.empty () in
    let out = Js.Dict.empty () in
    Array.iter
      (fun (k, v) ->
        match key ctx path k with
        | None -> failed := true
        | Some kj -> (
            match Js.Json.decodeString kj with
            | None ->
                report ctx path Malformed "This map's name is not text.";
                failed := true
            | Some name -> (
                let at = Member name :: path in
                if Option.is_some (Js.Dict.get seen name) then (
                  report ctx at Repeated_member
                    "This member is given more than once.";
                  failed := true)
                else Js.Dict.set seen name true;
                match value ctx at v with
                | Some j -> Js.Dict.set out name j
                | None -> failed := true)))
      items;
    let within =
      checked ctx path ()
        (count_bounds "member" ~min:min_properties ~max:max_properties
           (Array.length items))
    in
    if !failed || Option.is_none within then None
    else Some (Js.Json.object_ out)

  (* An object's members as the printed code writes them: [None] is a member
     left out, [Some (name, None)] one that could not be written. *)
  let w_object (members : (string * Js.Json.t option) option array) =
    let out = Js.Dict.empty () in
    let failed = ref false in
    Array.iter
      (function
        | None -> ()
        | Some (_, None) -> failed := true
        | Some (name, Some j) -> Js.Dict.set out name j)
      members;
    if !failed then None else Some (Js.Json.object_ out)

  let w_nullable (write : 'a writer) : 'a option writer =
   fun ctx path -> function
    | None -> Some Js.Json.null
    | Some v -> write ctx path v

  let to_json (write : 'a writer) v =
    let ctx = create () in
    finish ctx (fun () -> write ctx [] v)

  let encode (write : 'a writer) v =
    Result.map Js.Json.stringify (to_json write v)
end

(* ---- The types ---- *)

(* One recursive group, since components hold each other. Two may share a
   field's or a word's name, and a value is told apart by its type. *)
[@@@warning "-30"]

(* A line of an order. *)
type order_line_input = { sku : string; quantity : int; (* how many *) note : string option; price : float; tags : string array; placed : Instant.t; id : Uuid.t; day : int * int * int; ttl : Duration.t; }

(* A line of an order. *)
and order_line = { sku : string; quantity : int; (* how many *) note : string option; price : float; tags : string array; placed : Instant.t; id : Uuid.t; day : int * int * int; ttl : Duration.t; }

(* How much is said. *)
and settings_level = | Debug | Info | In_progress

and settings = { level : settings_level; ratio : float; step : int; amount : float; count : float option; big : float; }

and shape = | Circle of { radius : float; } | Square of { side : float; } | Dot

and tree = { value : float; children : tree array; }

and box_at = { x : float; y : float; }

and box = { kind : string; first_name : string; at : box_at; }

and account_input = { name : string; password : string; }

and account = { id : Uuid.t option; name : string; }

and either = | Number of float | String of string

(* How much is said. *)
and scores_key = | Debug | Info | In_progress

and scores = (scores_key * float) array

and pair = string * float * bool

and maybe = float option

and anything = Js.Json.t

and words = string array

and instant = Instant.t

and date = int * int * int

and duration = Duration.t

and uuid = Uuid.t

and uuid_v7 = Uuid.t

and base64 = string

and base64url = string

and uri = string

and ipv4 = string

and ipv6 = string

(* ---- Reading ---- *)

let rec read_order_line_input : order_line_input Wire.reader = fun ctx path j -> (fun ctx path j -> match Wire.members ctx path j with | None -> None | Some o -> let m_sku = Wire.required (Wire.string ()) ctx path o {js|sku|js} in let m_quantity = Wire.required (Wire.int ~min:(1) ~max:(99) ()) ctx path o {js|quantity|js} in let m_note = Wire.optional_or_null (Wire.string ~max_length:(4) ()) ctx path o {js|note|js} in let m_price = Wire.required (Wire.number ~min:(0.) ()) ctx path o {js|price|js} in let m_tags = Wire.required (Wire.array ~max_items:(2) (Wire.string ())) ctx path o {js|tags|js} in let m_placed = Wire.required (Wire.instant ()) ctx path o {js|placed|js} in let m_id = Wire.required (Wire.uuid ()) ctx path o {js|id|js} in let m_day = Wire.required (Wire.date ()) ctx path o {js|day|js} in let m_ttl = Wire.required (Wire.duration ()) ctx path o {js|ttl|js} in match (m_sku, m_quantity, m_note, m_price, m_tags, m_placed, m_id, m_day, m_ttl) with | (Some m_sku, Some m_quantity, Some m_note, Some m_price, Some m_tags, Some m_placed, Some m_id, Some m_day, Some m_ttl) -> Some (({ sku = m_sku; quantity = m_quantity; note = m_note; price = m_price; tags = m_tags; placed = m_placed; id = m_id; day = m_day; ttl = m_ttl; } : order_line_input)) | _ -> None) ctx path j

and read_order_line : order_line Wire.reader = fun ctx path j -> (fun ctx path j -> match Wire.members ctx path j with | None -> None | Some o -> let m_sku = Wire.required (Wire.string ()) ctx path o {js|sku|js} in let m_quantity = Wire.required (Wire.int ~min:(1) ~max:(99) ()) ctx path o {js|quantity|js} in let m_note = Wire.optional (Wire.string ~max_length:(4) ()) ctx path o {js|note|js} in let m_price = Wire.required (Wire.number ~min:(0.) ()) ctx path o {js|price|js} in let m_tags = Wire.required (Wire.array ~max_items:(2) (Wire.string ())) ctx path o {js|tags|js} in let m_placed = Wire.required (Wire.instant ()) ctx path o {js|placed|js} in let m_id = Wire.required (Wire.uuid ()) ctx path o {js|id|js} in let m_day = Wire.required (Wire.date ()) ctx path o {js|day|js} in let m_ttl = Wire.required (Wire.duration ()) ctx path o {js|ttl|js} in match (m_sku, m_quantity, m_note, m_price, m_tags, m_placed, m_id, m_day, m_ttl) with | (Some m_sku, Some m_quantity, Some m_note, Some m_price, Some m_tags, Some m_placed, Some m_id, Some m_day, Some m_ttl) -> Some (({ sku = m_sku; quantity = m_quantity; note = m_note; price = m_price; tags = m_tags; placed = m_placed; id = m_id; day = m_day; ttl = m_ttl; } : order_line)) | _ -> None) ctx path j

and read_settings : settings Wire.reader = fun ctx path j -> (fun ctx path j -> match Wire.members ctx path j with | None -> None | Some o -> let m_level = Wire.required (Wire.enum [|{js|debug|js}; {js|info|js}; {js|in-progress|js}|] (fun w -> if String.equal w {js|debug|js} then Some (Debug : settings_level) else if String.equal w {js|info|js} then Some (Info : settings_level) else if String.equal w {js|in-progress|js} then Some (In_progress : settings_level) else None)) ctx path o {js|level|js} in let m_ratio = Wire.required (Wire.number ~above:(0.) ~below:(1.) ()) ctx path o {js|ratio|js} in let m_step = Wire.required (Wire.int ~min:(0) ~max:(100) ~multiple_of:(5) ()) ctx path o {js|step|js} in let m_amount = Wire.required (Wire.number ~multiple_of:(0.01) ()) ctx path o {js|amount|js} in let m_count = Wire.optional (Wire.whole ()) ctx path o {js|count|js} in let m_big = Wire.required (Wire.whole ()) ctx path o {js|big|js} in Wire.unknown ctx path o [|{js|level|js}; {js|ratio|js}; {js|step|js}; {js|amount|js}; {js|count|js}; {js|big|js}|]; match (m_level, m_ratio, m_step, m_amount, m_count, m_big) with | (Some m_level, Some m_ratio, Some m_step, Some m_amount, Some m_count, Some m_big) -> Some (({ level = m_level; ratio = m_ratio; step = m_step; amount = m_amount; count = m_count; big = m_big; } : settings)) | _ -> None) ctx path j

and read_shape : shape Wire.reader = fun ctx path j -> (fun ctx path j -> match Wire.members ctx path j with | None -> None | Some o -> (match Wire.tag ctx path o {js|kind|js} ~absent:(Some {js|"dot"|js}) with | None -> None | Some t -> if String.equal t {js|"circle"|js} then (let m_radius = Wire.required (Wire.number ()) ctx path o {js|radius|js} in match (m_radius) with | (Some m_radius) -> Some ((Circle { radius = m_radius; } : shape)) | _ -> None) else if String.equal t {js|"square"|js} then (let m_side = Wire.required (Wire.number ()) ctx path o {js|side|js} in match (m_side) with | (Some m_side) -> Some ((Square { side = m_side; } : shape)) | _ -> None) else if String.equal t {js|"dot"|js} then (Some (Dot : shape)) else Wire.unknown_tag ctx path {js|kind|js} [{js|"circle"|js}; {js|"square"|js}; {js|"dot"|js}])) ctx path j

and read_tree : tree Wire.reader = fun ctx path j -> (fun ctx path j -> match Wire.members ctx path j with | None -> None | Some o -> let m_value = Wire.required (Wire.whole ()) ctx path o {js|value|js} in let m_children = Wire.required (Wire.array read_tree) ctx path o {js|children|js} in match (m_value, m_children) with | (Some m_value, Some m_children) -> Some (({ value = m_value; children = m_children; } : tree)) | _ -> None) ctx path j

and read_box : box Wire.reader = fun ctx path j -> (fun ctx path j -> match Wire.members ctx path j with | None -> None | Some o -> let m_kind = Wire.required (Wire.string ()) ctx path o {js|type|js} in let m_first_name = Wire.required (Wire.string ()) ctx path o {js|first-name|js} in let m_at = Wire.required (fun ctx path j -> match Wire.members ctx path j with | None -> None | Some o -> let m_x = Wire.required (Wire.number ()) ctx path o {js|x|js} in let m_y = Wire.required (Wire.number ()) ctx path o {js|y|js} in match (m_x, m_y) with | (Some m_x, Some m_y) -> Some (({ x = m_x; y = m_y; } : box_at)) | _ -> None) ctx path o {js|at|js} in match (m_kind, m_first_name, m_at) with | (Some m_kind, Some m_first_name, Some m_at) -> Some (({ kind = m_kind; first_name = m_first_name; at = m_at; } : box)) | _ -> None) ctx path j

and read_account_input : account_input Wire.reader = fun ctx path j -> (fun ctx path j -> match Wire.members ctx path j with | None -> None | Some o -> let m_name = Wire.required (Wire.string ()) ctx path o {js|name|js} in let m_password = Wire.required (Wire.string ()) ctx path o {js|password|js} in match (m_name, m_password) with | (Some m_name, Some m_password) -> Some (({ name = m_name; password = m_password; } : account_input)) | _ -> None) ctx path j

and read_account : account Wire.reader = fun ctx path j -> (fun ctx path j -> match Wire.members ctx path j with | None -> None | Some o -> let m_id = Wire.optional (Wire.uuid ()) ctx path o {js|id|js} in let m_name = Wire.required (Wire.string ()) ctx path o {js|name|js} in match (m_id, m_name) with | (Some m_id, Some m_name) -> Some (({ id = m_id; name = m_name; } : account)) | _ -> None) ctx path j

and read_either : either Wire.reader = fun ctx path j -> (fun ctx path j -> match Js.Json.classify j with | JSONNumber _ -> Option.map (fun x -> (Number x : either)) ((Wire.whole ()) ctx path j) | JSONString _ -> Option.map (fun x -> (String x : either)) ((Wire.string ()) ctx path j) | _ -> Wire.mismatch ctx path {js|a number or text|js} j) ctx path j

and read_scores : scores Wire.reader = fun ctx path j -> (Wire.dict ~max_properties:(2) (Wire.enum [|{js|debug|js}; {js|info|js}; {js|in-progress|js}|] (fun w -> if String.equal w {js|debug|js} then Some (Debug : scores_key) else if String.equal w {js|info|js} then Some (Info : scores_key) else if String.equal w {js|in-progress|js} then Some (In_progress : scores_key) else None)) (Wire.whole ())) ctx path j

and read_pair : pair Wire.reader = fun ctx path j -> (fun ctx path j -> match Wire.tuple 3 ctx path j with | None -> None | Some items -> let i0 = (Wire.string ()) ctx (Wire.Index 0 :: path) items.(0) in let i1 = (Wire.whole ()) ctx (Wire.Index 1 :: path) items.(1) in let i2 = Wire.bool ctx (Wire.Index 2 :: path) items.(2) in match (i0, i1, i2) with | (Some i0, Some i1, Some i2) -> Some (i0, i1, i2) | _ -> None) ctx path j

and read_maybe : maybe Wire.reader = fun ctx path j -> (Wire.nullable (Wire.whole ())) ctx path j

and read_anything : anything Wire.reader = fun ctx path j -> Wire.any ctx path j

and read_words : words Wire.reader = fun ctx path j -> (Wire.array ~min_items:(1) (Wire.string ~min_length:(2) ())) ctx path j

and read_instant : instant Wire.reader = fun ctx path j -> (Wire.instant ()) ctx path j

and read_date : date Wire.reader = fun ctx path j -> (Wire.date ()) ctx path j

and read_duration : duration Wire.reader = fun ctx path j -> (Wire.duration ()) ctx path j

and read_uuid : uuid Wire.reader = fun ctx path j -> (Wire.uuid ()) ctx path j

and read_uuid_v7 : uuid_v7 Wire.reader = fun ctx path j -> (Wire.uuid ~version:7 ()) ctx path j

and read_base64 : base64 Wire.reader = fun ctx path j -> (Wire.base64 ()) ctx path j

and read_base64url : base64url Wire.reader = fun ctx path j -> (Wire.base64url ()) ctx path j

and read_uri : uri Wire.reader = fun ctx path j -> (Wire.uri ()) ctx path j

and read_ipv4 : ipv4 Wire.reader = fun ctx path j -> (Wire.ipv4 ()) ctx path j

and read_ipv6 : ipv6 Wire.reader = fun ctx path j -> (Wire.ipv6 ()) ctx path j

(* ---- Writing ---- *)

let rec write_order_line_input : order_line_input Wire.writer = fun ctx path v -> (fun ctx path (v : order_line_input) -> Wire.w_object [|Some ({js|sku|js}, (Wire.w_string ()) ctx (Wire.Member {js|sku|js} :: path) v.sku); Some ({js|quantity|js}, (Wire.w_int ~min:(1) ~max:(99) ()) ctx (Wire.Member {js|quantity|js} :: path) v.quantity); (match v.note with None -> None | Some x -> Some ({js|note|js}, (Wire.w_string ~max_length:(4) ()) ctx (Wire.Member {js|note|js} :: path) x)); Some ({js|price|js}, (Wire.w_number ~min:(0.) ()) ctx (Wire.Member {js|price|js} :: path) v.price); Some ({js|tags|js}, (Wire.w_array ~max_items:(2) (Wire.w_string ())) ctx (Wire.Member {js|tags|js} :: path) v.tags); Some ({js|placed|js}, (Wire.w_instant ()) ctx (Wire.Member {js|placed|js} :: path) v.placed); Some ({js|id|js}, (Wire.w_uuid ()) ctx (Wire.Member {js|id|js} :: path) v.id); Some ({js|day|js}, (Wire.w_date ()) ctx (Wire.Member {js|day|js} :: path) v.day); Some ({js|ttl|js}, (Wire.w_duration ()) ctx (Wire.Member {js|ttl|js} :: path) v.ttl)|]) ctx path v

and write_order_line : order_line Wire.writer = fun ctx path v -> (fun ctx path (v : order_line) -> Wire.w_object [|Some ({js|sku|js}, (Wire.w_string ()) ctx (Wire.Member {js|sku|js} :: path) v.sku); Some ({js|quantity|js}, (Wire.w_int ~min:(1) ~max:(99) ()) ctx (Wire.Member {js|quantity|js} :: path) v.quantity); (match v.note with None -> None | Some x -> Some ({js|note|js}, (Wire.w_string ~max_length:(4) ()) ctx (Wire.Member {js|note|js} :: path) x)); Some ({js|price|js}, (Wire.w_number ~min:(0.) ()) ctx (Wire.Member {js|price|js} :: path) v.price); Some ({js|tags|js}, (Wire.w_array ~max_items:(2) (Wire.w_string ())) ctx (Wire.Member {js|tags|js} :: path) v.tags); Some ({js|placed|js}, (Wire.w_instant ()) ctx (Wire.Member {js|placed|js} :: path) v.placed); Some ({js|id|js}, (Wire.w_uuid ()) ctx (Wire.Member {js|id|js} :: path) v.id); Some ({js|day|js}, (Wire.w_date ()) ctx (Wire.Member {js|day|js} :: path) v.day); Some ({js|ttl|js}, (Wire.w_duration ()) ctx (Wire.Member {js|ttl|js} :: path) v.ttl)|]) ctx path v

and write_settings : settings Wire.writer = fun ctx path v -> (fun ctx path (v : settings) -> Wire.w_object [|Some ({js|level|js}, (Wire.w_enum (fun (v : settings_level) -> match v with | Debug -> {js|debug|js} | Info -> {js|info|js} | In_progress -> {js|in-progress|js})) ctx (Wire.Member {js|level|js} :: path) v.level); Some ({js|ratio|js}, (Wire.w_number ~above:(0.) ~below:(1.) ()) ctx (Wire.Member {js|ratio|js} :: path) v.ratio); Some ({js|step|js}, (Wire.w_int ~min:(0) ~max:(100) ~multiple_of:(5) ()) ctx (Wire.Member {js|step|js} :: path) v.step); Some ({js|amount|js}, (Wire.w_number ~multiple_of:(0.01) ()) ctx (Wire.Member {js|amount|js} :: path) v.amount); (match v.count with None -> None | Some x -> Some ({js|count|js}, (Wire.w_whole ()) ctx (Wire.Member {js|count|js} :: path) x)); Some ({js|big|js}, (Wire.w_whole ()) ctx (Wire.Member {js|big|js} :: path) v.big)|]) ctx path v

and write_shape : shape Wire.writer = fun ctx path v -> (fun ctx path (v : shape) -> match v with | Circle r -> Wire.w_object [|Some ({js|kind|js}, Some (Js.Json.string {js|circle|js})); Some ({js|radius|js}, (Wire.w_number ()) ctx (Wire.Member {js|radius|js} :: path) r.radius)|] | Square r -> Wire.w_object [|Some ({js|kind|js}, Some (Js.Json.string {js|square|js})); Some ({js|side|js}, (Wire.w_number ()) ctx (Wire.Member {js|side|js} :: path) r.side)|] | Dot -> Wire.w_object [|Some ({js|kind|js}, Some (Js.Json.string {js|dot|js}))|]) ctx path v

and write_tree : tree Wire.writer = fun ctx path v -> (fun ctx path (v : tree) -> Wire.w_object [|Some ({js|value|js}, (Wire.w_whole ()) ctx (Wire.Member {js|value|js} :: path) v.value); Some ({js|children|js}, (Wire.w_array write_tree) ctx (Wire.Member {js|children|js} :: path) v.children)|]) ctx path v

and write_box : box Wire.writer = fun ctx path v -> (fun ctx path (v : box) -> Wire.w_object [|Some ({js|type|js}, (Wire.w_string ()) ctx (Wire.Member {js|type|js} :: path) v.kind); Some ({js|first-name|js}, (Wire.w_string ()) ctx (Wire.Member {js|first-name|js} :: path) v.first_name); Some ({js|at|js}, (fun ctx path (v : box_at) -> Wire.w_object [|Some ({js|x|js}, (Wire.w_number ()) ctx (Wire.Member {js|x|js} :: path) v.x); Some ({js|y|js}, (Wire.w_number ()) ctx (Wire.Member {js|y|js} :: path) v.y)|]) ctx (Wire.Member {js|at|js} :: path) v.at)|]) ctx path v

and write_account_input : account_input Wire.writer = fun ctx path v -> (fun ctx path (v : account_input) -> Wire.w_object [|Some ({js|name|js}, (Wire.w_string ()) ctx (Wire.Member {js|name|js} :: path) v.name); Some ({js|password|js}, (Wire.w_string ()) ctx (Wire.Member {js|password|js} :: path) v.password)|]) ctx path v

and write_account : account Wire.writer = fun ctx path v -> (fun ctx path (v : account) -> Wire.w_object [|(match v.id with None -> None | Some x -> Some ({js|id|js}, (Wire.w_uuid ()) ctx (Wire.Member {js|id|js} :: path) x)); Some ({js|name|js}, (Wire.w_string ()) ctx (Wire.Member {js|name|js} :: path) v.name)|]) ctx path v

and write_either : either Wire.writer = fun ctx path v -> (fun ctx path (v : either) -> match v with | Number x -> (Wire.w_whole ()) ctx path x | String x -> (Wire.w_string ()) ctx path x) ctx path v

and write_scores : scores Wire.writer = fun ctx path v -> (Wire.w_dict ~max_properties:(2) (Wire.w_enum (fun (v : scores_key) -> match v with | Debug -> {js|debug|js} | Info -> {js|info|js} | In_progress -> {js|in-progress|js})) (Wire.w_whole ())) ctx path v

and write_pair : pair Wire.writer = fun ctx path v -> (fun ctx path (i0, i1, i2) -> Wire.w_tuple [|(Wire.w_string ()) ctx (Wire.Index 0 :: path) i0; (Wire.w_whole ()) ctx (Wire.Index 1 :: path) i1; Wire.w_bool ctx (Wire.Index 2 :: path) i2|]) ctx path v

and write_maybe : maybe Wire.writer = fun ctx path v -> (Wire.w_nullable (Wire.w_whole ())) ctx path v

and write_anything : anything Wire.writer = fun ctx path v -> Wire.w_any ctx path v

and write_words : words Wire.writer = fun ctx path v -> (Wire.w_array ~min_items:(1) (Wire.w_string ~min_length:(2) ())) ctx path v

and write_instant : instant Wire.writer = fun ctx path v -> (Wire.w_instant ()) ctx path v

and write_date : date Wire.writer = fun ctx path v -> (Wire.w_date ()) ctx path v

and write_duration : duration Wire.writer = fun ctx path v -> (Wire.w_duration ()) ctx path v

and write_uuid : uuid Wire.writer = fun ctx path v -> (Wire.w_uuid ()) ctx path v

and write_uuid_v7 : uuid_v7 Wire.writer = fun ctx path v -> (Wire.w_uuid ~version:7 ()) ctx path v

and write_base64 : base64 Wire.writer = fun ctx path v -> (Wire.w_base64 ()) ctx path v

and write_base64url : base64url Wire.writer = fun ctx path v -> (Wire.w_base64url ()) ctx path v

and write_uri : uri Wire.writer = fun ctx path v -> (Wire.w_uri ()) ctx path v

and write_ipv4 : ipv4 Wire.writer = fun ctx path v -> (Wire.w_ipv4 ()) ctx path v

and write_ipv6 : ipv6 Wire.writer = fun ctx path v -> (Wire.w_ipv6 ()) ctx path v

(* ---- Each component ---- *)

let decode_order_line_input s = Wire.decode read_order_line_input s
let order_line_input_of_json j = Wire.of_json read_order_line_input j
let encode_order_line_input v = Wire.encode write_order_line_input v
let order_line_input_to_json v = Wire.to_json write_order_line_input v

let decode_order_line s = Wire.decode read_order_line s
let order_line_of_json j = Wire.of_json read_order_line j
let encode_order_line v = Wire.encode write_order_line v
let order_line_to_json v = Wire.to_json write_order_line v

let decode_settings s = Wire.decode read_settings s
let settings_of_json j = Wire.of_json read_settings j
let encode_settings v = Wire.encode write_settings v
let settings_to_json v = Wire.to_json write_settings v

let decode_shape s = Wire.decode read_shape s
let shape_of_json j = Wire.of_json read_shape j
let encode_shape v = Wire.encode write_shape v
let shape_to_json v = Wire.to_json write_shape v

let decode_tree s = Wire.decode read_tree s
let tree_of_json j = Wire.of_json read_tree j
let encode_tree v = Wire.encode write_tree v
let tree_to_json v = Wire.to_json write_tree v

let decode_box s = Wire.decode read_box s
let box_of_json j = Wire.of_json read_box j
let encode_box v = Wire.encode write_box v
let box_to_json v = Wire.to_json write_box v

let decode_account_input s = Wire.decode read_account_input s
let account_input_of_json j = Wire.of_json read_account_input j
let encode_account_input v = Wire.encode write_account_input v
let account_input_to_json v = Wire.to_json write_account_input v

let decode_account s = Wire.decode read_account s
let account_of_json j = Wire.of_json read_account j
let encode_account v = Wire.encode write_account v
let account_to_json v = Wire.to_json write_account v

let decode_either s = Wire.decode read_either s
let either_of_json j = Wire.of_json read_either j
let encode_either v = Wire.encode write_either v
let either_to_json v = Wire.to_json write_either v

let decode_scores s = Wire.decode read_scores s
let scores_of_json j = Wire.of_json read_scores j
let encode_scores v = Wire.encode write_scores v
let scores_to_json v = Wire.to_json write_scores v

let decode_pair s = Wire.decode read_pair s
let pair_of_json j = Wire.of_json read_pair j
let encode_pair v = Wire.encode write_pair v
let pair_to_json v = Wire.to_json write_pair v

let decode_maybe s = Wire.decode read_maybe s
let maybe_of_json j = Wire.of_json read_maybe j
let encode_maybe v = Wire.encode write_maybe v
let maybe_to_json v = Wire.to_json write_maybe v

let decode_anything s = Wire.decode read_anything s
let anything_of_json j = Wire.of_json read_anything j
let encode_anything v = Wire.encode write_anything v
let anything_to_json v = Wire.to_json write_anything v

let decode_words s = Wire.decode read_words s
let words_of_json j = Wire.of_json read_words j
let encode_words v = Wire.encode write_words v
let words_to_json v = Wire.to_json write_words v

let decode_instant s = Wire.decode read_instant s
let instant_of_json j = Wire.of_json read_instant j
let encode_instant v = Wire.encode write_instant v
let instant_to_json v = Wire.to_json write_instant v

let decode_date s = Wire.decode read_date s
let date_of_json j = Wire.of_json read_date j
let encode_date v = Wire.encode write_date v
let date_to_json v = Wire.to_json write_date v

let decode_duration s = Wire.decode read_duration s
let duration_of_json j = Wire.of_json read_duration j
let encode_duration v = Wire.encode write_duration v
let duration_to_json v = Wire.to_json write_duration v

let decode_uuid s = Wire.decode read_uuid s
let uuid_of_json j = Wire.of_json read_uuid j
let encode_uuid v = Wire.encode write_uuid v
let uuid_to_json v = Wire.to_json write_uuid v

let decode_uuid_v7 s = Wire.decode read_uuid_v7 s
let uuid_v7_of_json j = Wire.of_json read_uuid_v7 j
let encode_uuid_v7 v = Wire.encode write_uuid_v7 v
let uuid_v7_to_json v = Wire.to_json write_uuid_v7 v

let decode_base64 s = Wire.decode read_base64 s
let base64_of_json j = Wire.of_json read_base64 j
let encode_base64 v = Wire.encode write_base64 v
let base64_to_json v = Wire.to_json write_base64 v

let decode_base64url s = Wire.decode read_base64url s
let base64url_of_json j = Wire.of_json read_base64url j
let encode_base64url v = Wire.encode write_base64url v
let base64url_to_json v = Wire.to_json write_base64url v

let decode_uri s = Wire.decode read_uri s
let uri_of_json j = Wire.of_json read_uri j
let encode_uri v = Wire.encode write_uri v
let uri_to_json v = Wire.to_json write_uri v

let decode_ipv4 s = Wire.decode read_ipv4 s
let ipv4_of_json j = Wire.of_json read_ipv4 j
let encode_ipv4 v = Wire.encode write_ipv4 v
let ipv4_to_json v = Wire.to_json write_ipv4 v

let decode_ipv6 s = Wire.decode read_ipv6 s
let ipv6_of_json j = Wire.of_json read_ipv6 j
let encode_ipv6 v = Wire.encode write_ipv6 v
let ipv6_to_json v = Wire.to_json write_ipv6 v

