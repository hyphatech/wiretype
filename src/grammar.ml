(* The grammars behind the ready-made kinds, each a function of a string.

   Each accepts no string the browser's zod check for it refuses, so a client
   never turns away what the server would take: the regular expressions zod
   4 checks with were read, and where the standard the kind names is looser
   than zod -- RFC 3986 beside the URL standard -- the grammar here is the
   narrower. Every refusal is a sentence for a person. *)

let digit c = match c with '0' .. '9' -> true | _ -> false

let hex c =
  match c with '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true | _ -> false

let alpha c = match c with 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

(* [n] digits at [i], as a number, or [-1]. *)
let fixed s i n =
  if i + n > String.length s then -1
  else
    let rec go k acc =
      if k = n then acc
      else if digit s.[i + k] then
        go (k + 1) ((acc * 10) + Char.code s.[i + k] - 48)
      else -1
    in
    go 0 0

(* ------------------------------------------------------------------ *)
(* Dates and instants *)

let leap y = (y mod 4 = 0 && y mod 100 <> 0) || y mod 400 = 0

let days_in_month y m =
  match m with 2 -> if leap y then 29 else 28 | 4 | 6 | 9 | 11 -> 30 | _ -> 31

(* Days since 1970-01-01 of a date in the proleptic Gregorian calendar, and
   back, by Howard Hinnant's algorithms: exact for every year here. *)
let days_of_date y m d =
  let y = if m <= 2 then y - 1 else y in
  let era = (if y >= 0 then y else y - 399) / 400 in
  let yoe = y - (era * 400) in
  let mp = (m + 9) mod 12 in
  let doy = (((153 * mp) + 2) / 5) + d - 1 in
  let doe = (yoe * 365) + (yoe / 4) - (yoe / 100) + doy in
  (era * 146097) + doe - 719468

let date_of_days z =
  let z = z + 719468 in
  let era = (if z >= 0 then z else z - 146096) / 146097 in
  let doe = z - (era * 146097) in
  let yoe = (doe - (doe / 1460) + (doe / 36524) - (doe / 146096)) / 365 in
  let y = yoe + (era * 400) in
  let doy = doe - ((365 * yoe) + (yoe / 4) - (yoe / 100)) in
  let mp = ((5 * doy) + 2) / 153 in
  let d = doy - (((153 * mp) + 2) / 5) + 1 in
  let m = if mp < 10 then mp + 3 else mp - 9 in
  ((if m <= 2 then y + 1 else y), m, d)

let date_at s i =
  if i + 10 > String.length s || s.[i + 4] <> '-' || s.[i + 7] <> '-' then None
  else
    let y = fixed s i 4 and m = fixed s (i + 5) 2 and d = fixed s (i + 8) 2 in
    if y < 0 || m < 1 || m > 12 || d < 1 || d > days_in_month y m then None
    else Some (y, m, d)

let not_a_date = "This must be a date written as YYYY-MM-DD, like 2026-09-30."

let date_of_string s =
  match date_at s 0 with
  | Some date when String.length s = 10 -> Ok date
  | Some _ | None -> Error not_a_date

let date_to_string (y, m, d) =
  if y < 0 || y > 9999 || m < 1 || m > 12 || d < 1 || d > days_in_month y m then
    Error
      (Printf.sprintf "%d-%d-%d is not a date of the years 0000 to 9999." y m d)
  else Ok (Printf.sprintf "%04d-%02d-%02d" y m d)

let day_ms = 86_400_000

let not_an_instant =
  "This must be an instant written as RFC 3339 has it, like \
   2026-09-30T12:00:00Z."

let instant_of_string s =
  let n = String.length s in
  let clock i = fixed s i 2 in
  match date_at s 0 with
  | None -> Error not_an_instant
  | Some (y, mo, d) -> (
      if n < 20 || s.[10] <> 'T' || s.[13] <> ':' || s.[16] <> ':' then
        Error not_an_instant
      else
        let h = clock 11 and mi = clock 14 and sec = clock 17 in
        if h < 0 || h > 23 || mi < 0 || mi > 59 || sec < 0 || sec > 59 then
          Error not_an_instant
        else
          (* A fraction is read to the millisecond and the rest dropped,
             which for a fraction is toward the past. *)
          let i, ms =
            if n > 19 && s.[19] = '.' then (
              let j = ref 20 in
              while !j < n && digit s.[!j] do
                incr j
              done;
              let digits = String.sub s 20 (!j - 20) in
              let ms =
                if String.length digits = 0 then -1
                else int_of_string (String.sub (digits ^ "00") 0 3)
              in
              (!j, ms))
            else (19, 0)
          in
          let offset =
            if ms < 0 then None
            else if i = n - 1 && s.[i] = 'Z' then Some 0
            else if i = n - 6 && (s.[i] = '+' || s.[i] = '-') && s.[i + 3] = ':'
            then
              let oh = clock (i + 1) and om = clock (i + 4) in
              if oh < 0 || oh > 23 || om < 0 || om > 59 then None
              else
                let minutes = (oh * 60) + om in
                Some (if s.[i] = '+' then minutes else -minutes)
            else None
          in
          match offset with
          | None -> Error not_an_instant
          | Some minutes ->
              Ok
                ((days_of_date y mo d * day_ms)
                + (((((h * 60) + mi) * 60) + sec) * 1000)
                + ms - (minutes * 60_000)))

let instant_to_string t =
  let days = if t >= 0 then t / day_ms else ((t + 1) / day_ms) - 1 in
  let rest = t - (days * day_ms) in
  let y, m, d = date_of_days days in
  if y < 0 || y > 9999 then
    Error (Printf.sprintf "The instant %d is outside the years 0000 to 9999." t)
  else
    let ms = rest mod 1000 and secs = rest / 1000 in
    Ok
      (Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d.%03dZ" y m d (secs / 3600)
         (secs / 60 mod 60)
         (secs mod 60) ms)

(* ------------------------------------------------------------------ *)
(* Durations *)

let not_a_duration =
  "This must be a duration written as ISO 8601 has it, like PT1H30M: weeks, or \
   days, hours, minutes and seconds, and never years or months."

(* What zod's own check accepts, less years and months: the schema's pattern,
   for whoever checks with one, since zod's duration takes both. *)
let duration_pattern =
  {|^P(?:\d+W|(?=\d|T\d)(?:\d+D)?(?:T(?=\d)(?:\d+H)?(?:\d+M)?(?:\d+(?:[.,]\d+)?S)?)?)$|}

(* A duration past a hundred thousand years is not one anybody means. *)
let too_long = 3_155_760_000_000_000

let duration_of_string s =
  let n = String.length s in
  let i = ref 1 in
  let number () =
    let start = !i in
    while !i < n && digit s.[!i] do
      incr i
    done;
    if !i = start || !i - start > 16 then None
    else Some (int_of_string (String.sub s start (!i - start)))
  in
  let fraction () =
    if !i < n && (s.[!i] = '.' || s.[!i] = ',') then (
      incr i;
      let start = !i in
      while !i < n && digit s.[!i] do
        incr i
      done;
      if !i = start then None
      else
        Some
          (int_of_string
             (String.sub (String.sub s start (!i - start) ^ "00") 0 3)))
    else Some 0
  in
  let designator c =
    if !i < n && s.[!i] = c then (
      incr i;
      true)
    else false
  in
  let fail = Error not_a_duration in
  let total ms =
    if ms > too_long then Error "This duration is too long." else Ok ms
  in
  (* A time part: at least one of hours, minutes and seconds, in that order. *)
  let time () =
    let components = ref 0 and ms = ref 0 in
    let step unit c =
      if !i < n && digit s.[!i] then
        let save = !i in
        match number () with
        | None -> false
        | Some v ->
            if designator c then (
              ms := !ms + (v * unit);
              incr components;
              true)
            else (
              i := save;
              true)
      else true
    in
    let seconds () =
      if !i < n && digit s.[!i] then
        match number () with
        | None -> false
        | Some v -> (
            match fraction () with
            | None -> false
            | Some frac ->
                if designator 'S' then (
                  ms := !ms + (v * 1000) + frac;
                  incr components;
                  true)
                else false)
      else true
    in
    if step 3_600_000 'H' && step 60_000 'M' && seconds () && !components > 0
    then Some !ms
    else None
  in
  if n < 2 || s.[0] <> 'P' then fail
  else if s.[1] = 'T' then (
    incr i;
    match time () with Some ms when !i = n -> total ms | Some _ | None -> fail)
  else
    match number () with
    | None -> fail
    | Some v ->
        if designator 'W' then if !i = n then total (v * 7 * day_ms) else fail
        else if designator 'D' then
          let days = v * day_ms in
          if !i = n then total days
          else if designator 'T' then
            match time () with
            | Some ms when !i = n -> total (days + ms)
            | Some _ | None -> fail
          else fail
        else fail

let duration_to_string ms =
  if ms < 0 then Error "A duration is never negative."
  else
    let days = ms / day_ms and rest = ms mod day_ms in
    let h = rest / 3_600_000
    and m = rest / 60_000 mod 60
    and s = rest / 1000 mod 60
    and frac = rest mod 1000 in
    let b = Buffer.create 16 in
    Buffer.add_char b 'P';
    if days > 0 then Buffer.add_string b (Printf.sprintf "%dD" days);
    if rest > 0 || days = 0 then begin
      Buffer.add_char b 'T';
      if h > 0 then Buffer.add_string b (Printf.sprintf "%dH" h);
      if m > 0 then Buffer.add_string b (Printf.sprintf "%dM" m);
      if s > 0 || frac > 0 || rest = 0 then begin
        Buffer.add_string b (string_of_int s);
        if frac > 0 then begin
          let f = Printf.sprintf "%03d" frac in
          let len = ref 3 in
          while f.[!len - 1] = '0' do
            decr len
          done;
          Buffer.add_char b '.';
          Buffer.add_string b (String.sub f 0 !len)
        end;
        Buffer.add_char b 'S'
      end
    end;
    Ok (Buffer.contents b)

(* ------------------------------------------------------------------ *)
(* UUIDs *)

let not_a_uuid version =
  match version with
  | Some v ->
      Printf.sprintf
        "This must be a version %d UUID, like \
         01890a5d-ac96-7a3b-9e5a-5f1c2a7b8c9d."
        v
  | None -> "This must be a UUID, like 01890a5d-ac96-7a3b-9e5a-5f1c2a7b8c9d."

let uuid_of_string ?version s =
  let shaped =
    String.length s = 36
    && List.for_all Fun.id
         (List.init 36 (fun i ->
              match i with 8 | 13 | 18 | 23 -> s.[i] = '-' | _ -> hex s.[i]))
  in
  let variant =
    shaped
    &&
    match s.[19] with
    | '8' | '9' | 'a' | 'b' | 'A' | 'B' -> true
    | _ -> false
  in
  let special =
    String.equal s "00000000-0000-0000-0000-000000000000"
    || String.equal s "ffffffff-ffff-ffff-ffff-ffffffffffff"
  in
  let ok =
    shaped
    &&
    match version with
    | Some v -> Char.equal s.[14] (Char.chr (48 + v)) && variant
    | None ->
        special
        || ((match s.[14] with '1' .. '8' -> true | _ -> false) && variant)
  in
  if ok then Ok (String.lowercase_ascii s) else Error (not_a_uuid version)

(* ------------------------------------------------------------------ *)
(* Base64 *)

let alphabet ~url =
  if url then "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
  else "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let sextet ~url c =
  match c with
  | 'A' .. 'Z' -> Char.code c - 65
  | 'a' .. 'z' -> Char.code c - 71
  | '0' .. '9' -> Char.code c + 4
  | '+' when not url -> 62
  | '/' when not url -> 63
  | '-' when url -> 62
  | '_' when url -> 63
  | _ -> -1

let base64_of_string ~url s =
  let n = String.length s in
  let refused =
    Error
      (if url then "This must be bytes in base64url, unpadded."
       else "This must be bytes in base64, padded to a multiple of four.")
  in
  (* Padding is exactly what makes a length of four, and only at the end. *)
  let body =
    if url then if n mod 4 = 1 then None else Some n
    else if n mod 4 <> 0 then None
    else if n >= 2 && s.[n - 1] = '=' && s.[n - 2] = '=' then Some (n - 2)
    else if n >= 1 && s.[n - 1] = '=' then Some (n - 1)
    else Some n
  in
  match body with
  | None -> refused
  | Some m ->
      let b = Buffer.create (m * 3 / 4) in
      let acc = ref 0 and bits = ref 0 and bad = ref false in
      for i = 0 to m - 1 do
        let v = sextet ~url s.[i] in
        if v < 0 then bad := true
        else begin
          acc := (!acc lsl 6) lor v;
          bits := !bits + 6;
          if !bits >= 8 then begin
            bits := !bits - 8;
            Buffer.add_char b (Char.chr ((!acc lsr !bits) land 0xFF))
          end
        end
      done;
      if !bad || m mod 4 = 1 then refused else Ok (Buffer.contents b)

let base64_to_string ~url s =
  let a = alphabet ~url in
  let n = String.length s in
  let b = Buffer.create ((n + 2) / 3 * 4) in
  let byte i = Char.code s.[i] in
  let i = ref 0 in
  while !i + 2 < n do
    let v = (byte !i lsl 16) lor (byte (!i + 1) lsl 8) lor byte (!i + 2) in
    Buffer.add_char b a.[(v lsr 18) land 63];
    Buffer.add_char b a.[(v lsr 12) land 63];
    Buffer.add_char b a.[(v lsr 6) land 63];
    Buffer.add_char b a.[v land 63];
    i := !i + 3
  done;
  (match n - !i with
  | 1 ->
      let v = byte !i lsl 16 in
      Buffer.add_char b a.[(v lsr 18) land 63];
      Buffer.add_char b a.[(v lsr 12) land 63];
      if not url then Buffer.add_string b "=="
  | 2 ->
      let v = (byte !i lsl 16) lor (byte (!i + 1) lsl 8) in
      Buffer.add_char b a.[(v lsr 18) land 63];
      Buffer.add_char b a.[(v lsr 12) land 63];
      Buffer.add_char b a.[(v lsr 6) land 63];
      if not url then Buffer.add_char b '='
  | _ -> ());
  Buffer.contents b

(* ------------------------------------------------------------------ *)
(* IP addresses *)

(* Dotted decimal: four numbers to 255, none with a leading zero, as zod's
   check has it. *)
let ipv4 s =
  let parts = String.split_on_char '.' s in
  List.length parts = 4
  && List.for_all
       (fun p ->
         let n = String.length p in
         n >= 1 && n <= 3 && String.for_all digit p
         && (n = 1 || p.[0] <> '0')
         && int_of_string p <= 255)
       parts

(* The full and compressed forms, and -- where [dotted] -- a dotted IPv4
   address in the last thirty-two bits, as RFC 4291 §2.2 writes one. *)
let ipv6 ~dotted s =
  let group g =
    String.length g >= 1 && String.length g <= 4 && String.for_all hex g
  in
  let groups part =
    if String.equal part "" then Some 0
    else
      let gs = String.split_on_char ':' part in
      let rec count = function
        | [] -> Some 0
        | [ last ] when dotted && String.contains last '.' ->
            if ipv4 last then Some 2 else None
        | g :: rest ->
            if group g then Option.map (( + ) 1) (count rest) else None
      in
      count gs
  in
  let rec split_double i =
    if i + 1 >= String.length s then None
    else if s.[i] = ':' && s.[i + 1] = ':' then Some i
    else split_double (i + 1)
  in
  match split_double 0 with
  | None -> ( match groups s with Some 8 -> true | Some _ | None -> false)
  | Some i -> (
      let left = String.sub s 0 i
      and right = String.sub s (i + 2) (String.length s - i - 2) in
      (* One "::" only, and no single colon at either end of what it joins. *)
      let clean part =
        not
          (String.length part > 0
          && (part.[0] = ':' || part.[String.length part - 1] = ':'))
      in
      if String.length right >= 1 && right.[0] = ':' then false
      else if not (clean left && clean right) then false
      else
        match (groups left, groups right) with
        | Some l, Some r -> l + r <= 7
        | _ -> false)

let ipv4_of_string s =
  if ipv4 s then Ok s else Error "This must be an IPv4 address, like 192.0.2.1."

let ipv6_of_string s =
  if ipv6 ~dotted:true s then Ok s
  else Error "This must be an IPv6 address, like 2001:db8::1."

(* ------------------------------------------------------------------ *)
(* URIs *)

let not_a_uri =
  "This must be a URI with its scheme, like https://example.com/path."

let unreserved c =
  alpha c || digit c
  || match c with '-' | '.' | '_' | '~' -> true | _ -> false

let sub_delim c =
  match c with
  | '!' | '$' | '&' | '\'' | '(' | ')' | '*' | '+' | ',' | ';' | '=' -> true
  | _ -> false

(* Every character of [s] between [i] and [j] is one [ok] allows or a
   percent-encoded octet. *)
let chars ok s i j =
  let rec go k =
    if k >= j then true
    else if s.[k] = '%' then
      k + 2 < j && hex s.[k + 1] && hex s.[k + 2] && go (k + 3)
    else ok s.[k] && go (k + 1)
  in
  go i

let pchar c = unreserved c || sub_delim c || c = ':' || c = '@'

(* ------------------------------------------------------------------ *)
(* Internationalised hosts *)

(* Whether [x] is in sorted ranges written lo, hi, lo, hi. *)
let in_ranges table x =
  let rec go lo hi =
    if lo >= hi then false
    else
      let mid = (lo + hi) / 2 in
      if x < table.(2 * mid) then go lo mid
      else if x > table.((2 * mid) + 1) then go (mid + 1) hi
      else true
  in
  go 0 (Array.length table / 2)

(* A code point's bidirectional class, as the generator numbers them: 0 L,
   1 R, 2 AL, 3 AN, 4 EN, 5 ES, 6 CS, 7 ET, 8 ON, 9 BN, 10 NSM, 11 any other. *)
let bidi_class x =
  let t = Idna_data.bidi in
  let rec go lo hi =
    if lo >= hi then 0
    else
      let mid = (lo + hi) / 2 in
      if x < t.(3 * mid) then go lo mid
      else if x > t.((3 * mid) + 1) then go (mid + 1) hi
      else t.((3 * mid) + 2)
  in
  go 0 (Array.length t / 3)

(* A Punycode label's code points, RFC 3492 section 6.2, the label already
   lower-cased and past its [xn--]. [None] where it does not decode, or
   decodes a code point no label could hold. *)
let punycode s =
  let base = 36 and tmin = 1 and tmax = 26 and skew = 38 and damp = 700 in
  let adapt delta points first =
    let delta = if first then delta / damp else delta / 2 in
    let delta = delta + (delta / points) in
    let rec go delta k =
      if delta > (base - tmin) * tmax / 2 then
        go (delta / (base - tmin)) (k + base)
      else k + ((base - tmin + 1) * delta / (delta + skew))
    in
    go delta 0
  in
  let n = String.length s in
  let basic, start =
    match String.rindex_opt s '-' with
    | Some b -> (String.sub s 0 b, b + 1)
    | None -> ("", 0)
  in
  let out =
    ref (List.init (String.length basic) (fun i -> Char.code basic.[i]))
  in
  let len = ref (String.length basic) in
  let limit = 0x10FFFF * 64 in
  let rec decode pos code i bias =
    if pos >= n then Some (Array.of_list !out)
    else
      let rec digits pos i w k =
        if pos >= n then None
        else
          let d =
            match s.[pos] with
            | 'a' .. 'z' as c -> Char.code c - 97
            | '0' .. '9' as c -> Char.code c - 22
            | _ -> -1
          in
          if d < 0 || d > (limit - i) / w then None
          else
            let i = i + (d * w) in
            let t =
              if k <= bias then tmin
              else if k >= bias + tmax then tmax
              else k - bias
            in
            if d < t then Some (pos + 1, i)
            else if w > limit / (base - t) then None
            else digits (pos + 1) i (w * (base - t)) (k + base)
      in
      match digits pos i 1 base with
      | None -> None
      | Some (pos, i') ->
          let bias = adapt (i' - i) (!len + 1) (i = 0) in
          let code = code + (i' / (!len + 1)) and at = i' mod (!len + 1) in
          if code < 0x80 || code > 0x10FFFF || (code >= 0xD800 && code <= 0xDFFF)
          then None
          else begin
            out :=
              List.filteri (fun j _ -> j < at) !out
              @ (code :: List.filteri (fun j _ -> j >= at) !out);
            incr len;
            decode pos code (at + 1) bias
          end
  in
  decode start 128 0 72

(* A label's code points once decoded, as UTS #46's validity criteria read
   them for a browser's URL -- no hyphen rules, no STD3 rules -- and stricter
   than them in two places: a joiner (U+200C, U+200D) is refused rather than
   judged by its context, and so is a code point that may not be in NFC,
   rather than normalised; either way no browser takes a host this refuses
   when it would. A label that decodes to ASCII alone is written as itself,
   and refused. *)
let unicode_label cps =
  let n = Array.length cps in
  n > 0
  && Array.exists (fun c -> c >= 0x80) cps
  && Array.for_all
       (fun c ->
         in_ranges Idna_data.valid c
         && c <> 0x2E && c <> 0x200C && c <> 0x200D
         && not (in_ranges Idna_data.not_nfc c))
       cps
  && (not (in_ranges Idna_data.mark cps.(0)))
  && not
       (n >= 4
       && cps.(0) = 0x78
       && cps.(1) = 0x6E
       && cps.(2) = 0x2D
       && cps.(3) = 0x2D)

(* RFC 5893's rule, for each label of a host that has right-to-left text. *)
let bidi_label cps =
  let cls = Array.map bidi_class cps in
  let n = Array.length cls in
  let rec last i =
    if i < 0 then -1 else if cls.(i) = 10 then last (i - 1) else cls.(i)
  in
  let only allowed = Array.for_all (fun c -> List.mem c allowed) cls in
  n > 0
  &&
  match cls.(0) with
  | 1 | 2 ->
      only [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ]
      && List.mem (last (n - 1)) [ 1; 2; 3; 4 ]
      && not
           (Array.exists (fun c -> c = 3) cls
           && Array.exists (fun c -> c = 4) cls)
  | 0 -> only [ 0; 4; 5; 6; 7; 8; 9; 10 ] && List.mem (last (n - 1)) [ 0; 4 ]
  | _ -> false

(* The schemes the URL standard calls special, whose host must be a domain
   or an address: for them a host is required, and read narrowly -- letters,
   digits, [-] and [_] in dotted labels, and one ending in a number is a
   dotted IPv4 address -- because the URL standard refuses much of what RFC
   3986's registered name allows. A label in [xn--] is decoded and checked
   as the URL standard checks one, against Unicode 15.0.0's own tables,
   which current browsers have and older ones do not all have: a code point
   newer than that is refused. *)
let special scheme =
  match String.lowercase_ascii scheme with
  | "http" | "https" | "ws" | "wss" | "ftp" -> true
  | _ -> false

let domain host =
  let host =
    if String.length host > 1 && host.[String.length host - 1] = '.' then
      String.sub host 0 (String.length host - 1)
    else host
  in
  let labels = String.split_on_char '.' host in
  (* A label's code points, as the URL standard reads it: lower-cased, and
     an [xn--] one decoded; [None] where it may not be a label. *)
  let label l =
    if
      String.length l >= 1
      && String.for_all (fun c -> alpha c || digit c || c = '-' || c = '_') l
    then
      let l = String.lowercase_ascii l in
      if String.length l >= 4 && String.equal (String.sub l 0 4) "xn--" then
        match punycode (String.sub l 4 (String.length l - 4)) with
        | Some cps when unicode_label cps -> Some cps
        | Some _ | None -> None
      else Some (Array.init (String.length l) (fun i -> Char.code l.[i]))
    else None
  in
  let last = List.nth_opt labels (List.length labels - 1) in
  let numeric =
    match last with
    | Some l -> String.length l > 0 && digit l.[0]
    | None -> false
  in
  if numeric then ipv4 host
  else
    let read = List.map label labels in
    List.for_all Option.is_some read
    &&
    let cps = List.filter_map Fun.id read in
    let rtl = Array.exists (fun c -> List.mem (bidi_class c) [ 1; 2; 3 ]) in
    (not (List.exists rtl cps)) || List.for_all bidi_label cps

let port s =
  String.for_all digit s
  && (String.equal s "" || (String.length s <= 5 && int_of_string s <= 65535))

let authority ~special s =
  (* At most one '@', since neither side may hold one. *)
  let userinfo, hostport =
    match String.index_opt s '@' with
    | Some i ->
        (Some (String.sub s 0 i), String.sub s (i + 1) (String.length s - i - 1))
    | None -> (None, s)
  in
  let userinfo_ok =
    match userinfo with
    | None -> true
    | Some u ->
        chars
          (fun c -> unreserved c || sub_delim c || c = ':')
          u 0 (String.length u)
  in
  let host, port_ok =
    if String.length hostport > 0 && hostport.[0] = '[' then
      match String.index_opt hostport ']' with
      | None -> (None, false)
      | Some j ->
          let literal = String.sub hostport 1 (j - 1) in
          let after =
            String.sub hostport (j + 1) (String.length hostport - j - 1)
          in
          let port_ok =
            String.equal after ""
            || after.[0] = ':'
               && port (String.sub after 1 (String.length after - 1))
          in
          (* An IPvFuture literal is RFC 3986's and not the URL standard's. *)
          if ipv6 ~dotted:true literal then (Some `Literal, port_ok)
          else (None, false)
    else
      let h, p =
        match String.index_opt hostport ':' with
        | Some i ->
            ( String.sub hostport 0 i,
              Some
                (String.sub hostport (i + 1) (String.length hostport - i - 1))
            )
        | None -> (hostport, None)
      in
      let port_ok = match p with None -> true | Some p -> port p in
      let ok =
        if special then (not (String.equal h "")) && domain h
        else chars (fun c -> unreserved c || sub_delim c) h 0 (String.length h)
      in
      if ok then (Some (`Name h), port_ok) else (None, false)
  in
  userinfo_ok && port_ok && Option.is_some host

let uri_of_string s =
  let n = String.length s in
  let colon = String.index_opt s ':' in
  match colon with
  | None -> Error not_a_uri
  | Some c ->
      let scheme = String.sub s 0 c in
      let scheme_ok =
        c >= 1
        && alpha s.[0]
        && String.for_all
             (fun ch ->
               alpha ch || digit ch || ch = '+' || ch = '-' || ch = '.')
             scheme
      in
      let fragment_at =
        Option.value (String.index_from_opt s c '#') ~default:n
      in
      let query_at =
        match String.index_from_opt s c '?' with
        | Some q when q < fragment_at -> q
        | Some _ | None -> fragment_at
      in
      let tail_ok from until =
        from >= until
        || chars (fun ch -> pchar ch || ch = '/' || ch = '?') s (from + 1) until
      in
      let hier = String.sub s (c + 1) (query_at - c - 1) in
      let hier_ok =
        if String.length hier >= 2 && hier.[0] = '/' && hier.[1] = '/' then
          let rest = String.sub hier 2 (String.length hier - 2) in
          let slash =
            Option.value
              (String.index_opt rest '/')
              ~default:(String.length rest)
          in
          let auth = String.sub rest 0 slash in
          authority ~special:(special scheme) auth
          && chars
               (fun ch -> pchar ch || ch = '/')
               rest slash (String.length rest)
        else
          (* With no authority a special scheme has no host, which the URL
             standard reads as the path's first segment: refused here, so a
             host is always written as one. *)
          (not (special scheme))
          && chars (fun ch -> pchar ch || ch = '/') hier 0 (String.length hier)
      in
      if
        scheme_ok && hier_ok
        && tail_ok query_at fragment_at
        && tail_ok fragment_at n
      then Ok s
      else Error not_a_uri
