(* A double's shortest digits, by Raffaello Giulietti's Schubfach: the
   fewest decimal digits that read back as the double, and of those the
   nearest to it -- found with three 126-bit products against a table of
   powers of ten, where formatting digits and parsing them back to check
   was most of what a number cost to write. *)

let c_min = 1 lsl 52
let q_min = -1074
let mask63 = 0x7FFF_FFFF_FFFF_FFFFL

(* floor (e log10 2), floor (log10 (3/4 2^e)), floor (e log2 10): exact over
   every exponent a double has. *)
let flog10pow2 e = (e * 661_971_961_083) asr 41

let flog10_three_quarters_pow2 e =
  ((e * 661_971_961_083) - 274_743_187_321) asr 41

let flog2pow10 e = (e * 913_124_641_741) asr 38

(* The high 64 bits of an unsigned 64-bit product, a half at a time. *)
let high a b =
  let lo x = Int64.logand x 0xFFFF_FFFFL
  and hi x = Int64.shift_right_logical x 32 in
  let a0 = lo a and a1 = hi a and b0 = lo b and b1 = hi b in
  let p00 = Int64.mul a0 b0 and p01 = Int64.mul a0 b1 in
  let p10 = Int64.mul a1 b0 and p11 = Int64.mul a1 b1 in
  let mid = Int64.add (Int64.add (hi p00) (lo p01)) (lo p10) in
  Int64.add (Int64.add (Int64.add p11 (hi p01)) (hi p10)) (hi mid)

(* g cp, rounded to odd: what the digits are chosen from. *)
let rop g1 g0 cp =
  let x1 = high g0 cp and y0 = Int64.mul g1 cp and y1 = high g1 cp in
  let z = Int64.add (Int64.shift_right_logical y0 1) x1 in
  let vbp = Int64.add y1 (Int64.shift_right_logical z 63) in
  Int64.logor vbp
    (Int64.shift_right_logical (Int64.add (Int64.logand z mask63) mask63) 63)

let ( <=. ) a b = Int64.compare a b <= 0
let i64 = Int64.of_int

(* The digits [f] and exponent [e] of [c 2^q] = [f 10^e], shortest. *)
let decimal q c dk =
  let out = i64 (c land 1) in
  let cb = c lsl 2 in
  let cbr = cb + 2 in
  let cbl, k =
    if c <> c_min || q = q_min then (cb - 2, flog10pow2 q)
    else (cb - 1, flog10_three_quarters_pow2 q)
  in
  let h = q + flog2pow10 (-k) + 2 in
  let i = 2 * (k - Powers.k_min) in
  let g1 = Powers.g.(i) and g0 = Powers.g.(i + 1) in
  let vb = rop g1 g0 (Int64.shift_left (i64 cb) h) in
  let vbl = rop g1 g0 (Int64.shift_left (i64 cbl) h) in
  let vbr = rop g1 g0 (Int64.shift_left (i64 cbr) h) in
  let s = Int64.to_int (Int64.shift_right vb 2) in
  let four x = Int64.shift_left (i64 x) 2 in
  let tens =
    if s >= 100 then
      let sp10 = 10 * (s / 10) in
      let tp10 = sp10 + 10 in
      let upin = Int64.add vbl out <=. four sp10 in
      let wpin = Int64.add (four tp10) out <=. vbr in
      if upin <> wpin then Some ((if upin then sp10 else tp10), k) else None
    else None
  in
  match tens with
  | Some d -> d
  | None ->
      let t = s + 1 in
      let uin = Int64.add vbl out <=. four s in
      let win = Int64.add (four t) out <=. vbr in
      if uin <> win then ((if uin then s else t), k + dk)
      else
        let cmp = Int64.sub vb (Int64.shift_left (i64 (s + t)) 1) in
        let c = Int64.compare cmp 0L in
        ((if c < 0 || (c = 0 && s land 1 = 0) then s else t), k + dk)

(* [f 10^e] for a positive, finite, non-zero double. *)
let shortest f =
  let bits = Int64.bits_of_float f in
  let t = Int64.to_int (Int64.logand bits 0xF_FFFF_FFFF_FFFFL) in
  let bq = Int64.to_int (Int64.shift_right_logical bits 52) land 0x7FF in
  if bq <> 0 then
    let mq = 1075 - bq in
    let c = c_min lor t in
    if 0 < mq && mq < 53 && (c lsr mq) lsl mq = c then (c lsr mq, 0)
    else decimal (-mq) c 0
    (* The two smallest doubles, whose one digit the algorithm keeps a second
     beside, as a printer of two digits at least would: their shortest are
     5e-324 and 1e-323. *)
  else if t = 1 then (5, -324)
  else if t = 2 then (1, -323)
  else decimal q_min t 0

(* The digits laid out as [%g] lays them out: fixed where the first digit's
   exponent is from -4 to one short of the precision -- sixteen digits, or
   seventeen where the number needs them -- and scientific otherwise, with a
   signed exponent of at least two digits. *)
let spelled x =
  if Float.equal x 0. then if Float.sign_bit x then "-0" else "0"
  else
    let f, e = shortest (Float.abs x) in
    let rec trim f e = if f mod 10 = 0 then trim (f / 10) (e + 1) else (f, e) in
    let f, e = trim f e in
    let digits = string_of_int f in
    let n = String.length digits in
    let exp = e + n - 1 in
    let b = Buffer.create 24 in
    if Float.sign_bit x then Buffer.add_char b '-';
    if exp >= -4 && exp < max 16 n then
      begin if e >= 0 then (
        Buffer.add_string b digits;
        Buffer.add_string b (String.make e '0'))
      else if exp >= 0 then (
        Buffer.add_string b (String.sub digits 0 (exp + 1));
        Buffer.add_char b '.';
        Buffer.add_string b (String.sub digits (exp + 1) (n - exp - 1)))
      else (
        Buffer.add_string b "0.";
        Buffer.add_string b (String.make (-exp - 1) '0');
        Buffer.add_string b digits)
      end
    else begin
      Buffer.add_char b digits.[0];
      if n > 1 then (
        Buffer.add_char b '.';
        Buffer.add_string b (String.sub digits 1 (n - 1)));
      Buffer.add_char b 'e';
      Buffer.add_char b (if exp < 0 then '-' else '+');
      if abs exp < 10 then Buffer.add_char b '0';
      Buffer.add_string b (string_of_int (abs exp))
    end;
    Buffer.contents b

let number b f =
  if not (Float.is_finite f) then Buffer.add_string b "null"
    (* An integer below 2^53 is exact, and its digits are the shortest
       spelling there is. Most numbers in a document are one, and formatting
       is most of what writing one costs. [-0] keeps its sign. *)
  else if
    Float.is_integer f
    && Float.abs f < 0x1p53
    && not (Float.sign_bit f && Float.equal f 0.)
  then Buffer.add_string b (string_of_int (Float.to_int f))
  else Buffer.add_string b (spelled f)

let escaped = function
  | '"' | '\\' | '\x00' .. '\x1F' | '\x7F' -> true
  | _ -> false

(* The text between escapes is written a run at a time: most strings have
   none, and a byte at a time was most of what writing one cost. *)
let string b s =
  let n = String.length s in
  Buffer.add_char b '"';
  let rec run from i =
    if i = n then Buffer.add_substring b s from (i - from)
    else
      let c = String.unsafe_get s i in
      if not (escaped c) then run from (i + 1)
      else begin
        Buffer.add_substring b s from (i - from);
        (match c with
        | '"' -> Buffer.add_string b {|\"|}
        | '\\' -> Buffer.add_string b {|\\|}
        | '\n' -> Buffer.add_string b {|\n|}
        | '\r' -> Buffer.add_string b {|\r|}
        | '\t' -> Buffer.add_string b {|\t|}
        | c -> Buffer.add_string b (Printf.sprintf "\\u%04X" (Char.code c)));
        run (i + 1) (i + 1)
      end
  in
  run 0 0;
  Buffer.add_char b '"'

let quote s =
  let b = Buffer.create (String.length s + 2) in
  string b s;
  Buffer.contents b
