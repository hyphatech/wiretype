(* The table the double printer reads, written as OCaml at build time: for
   each k from -324 to 292, the 126 bits of g where

     10^-k = beta 2^r,  2^125 <= beta < 2^126,  g = floor beta + 1

   as two 63-bit halves, g1 and g0. It is exact integer arithmetic on
   numbers of up to a thousand bits, which no double can do, so it is done
   here once rather than approximated. *)

let limb = 24
let limbs = 60
let mask = (1 lsl limb) - 1

(* A natural number, little-endian limbs of [limb] bits, and room for 1440
   bits: 10^324 is 1077 of them. *)
let zero () = Array.make limbs 0

let times_ten a =
  let r = zero () and carry = ref 0 in
  for i = 0 to limbs - 1 do
    let v = (a.(i) * 10) + !carry in
    r.(i) <- v land mask;
    carry := v lsr limb
  done;
  assert (!carry = 0);
  r

let bit a i = (a.(i / limb) lsr (i mod limb)) land 1

let bit_length a =
  let rec top i =
    if i < 0 then 0
    else if a.(i) = 0 then top (i - 1)
    else
      let v = a.(i) in
      let n = ref 0 in
      while v lsr !n > 0 do
        incr n
      done;
      (i * limb) + !n
  in
  top (limbs - 1)

let at_least a b =
  let rec go i =
    if i < 0 then true
    else if a.(i) > b.(i) then true
    else if a.(i) < b.(i) then false
    else go (i - 1)
  in
  go (limbs - 1)

let subtract a b =
  let borrow = ref 0 in
  for i = 0 to limbs - 1 do
    let v = a.(i) - b.(i) - !borrow in
    if v < 0 then (
      a.(i) <- v + (1 lsl limb);
      borrow := 1)
    else (
      a.(i) <- v;
      borrow := 0)
  done

let double_plus a b =
  let carry = ref b in
  for i = 0 to limbs - 1 do
    let v = (a.(i) lsl 1) lor !carry in
    a.(i) <- v land mask;
    carry := v lsr limb
  done;
  assert (!carry = 0)

let powers = Array.make 325 (zero ())

let () =
  powers.(0) <-
    (let a = zero () in
     a.(0) <- 1;
     a);
  for m = 1 to 324 do
    powers.(m) <- times_ten powers.(m - 1)
  done

(* floor beta, as 126 bits, least significant first. *)
let beta k =
  if k <= 0 then
    let n = powers.(-k) in
    let r = bit_length n - 126 in
    Array.init 126 (fun i -> if i + r < 0 then 0 else bit n (i + r))
  else
    let d = powers.(k) in
    let s = 125 + bit_length d in
    let rest = zero () and q = Array.make 126 0 in
    for i = s downto 0 do
      double_plus rest (if i = s then 1 else 0);
      if at_least rest d then begin
        subtract rest d;
        assert (i < 126);
        q.(i) <- 1
      end
    done;
    q

let halves k =
  let g = beta k in
  (* + 1, which never carries out of 126 bits. *)
  let rec add i =
    assert (i < 126);
    if g.(i) = 1 then (
      g.(i) <- 0;
      add (i + 1))
    else g.(i) <- 1
  in
  add 0;
  let word from =
    let w = ref 0L in
    for i = from + 62 downto from do
      w := Int64.logor (Int64.shift_left !w 1) (Int64.of_int g.(i))
    done;
    !w
  in
  (word 63, word 0)

let () =
  print_string
    "(* Written by gen/powers.exe: g for each k from -324 to 292, g1 then g0. \
     *)\n\n\
     let k_min = -324\n\n\
     let g = [|\n";
  for k = -324 to 292 do
    let g1, g0 = halves k in
    Printf.printf "  0x%016LXL; 0x%016LXL;\n" g1 g0
  done;
  print_string "|]\n"
