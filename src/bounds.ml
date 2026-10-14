(* A sentence is made only for a bound that is broken, so a value within its
   bounds -- as nearly every value the writer checks is -- allocates
   nothing. *)

module S = Shape
module P = Problem

type broken = (P.code * string) list

let at_least show m =
  [ (P.Too_small, Printf.sprintf "This must be at least %s." (show m)) ]

let at_most show m =
  [ (P.Too_large, Printf.sprintf "This must be at most %s." (show m)) ]

let not_a_multiple show m =
  [
    (P.Not_a_multiple, Printf.sprintf "This must be a multiple of %s." (show m));
  ]

let int (b : S.int_bounds) n =
  (match b.min with
    | Some m when n < m -> at_least string_of_int m
    | Some _ | None -> [])
  @ (match b.max with
    | Some m when n > m -> at_most string_of_int m
    | Some _ | None -> [])
  @
  match b.multiple_of with
  | Some m when m <> 0 && n mod m <> 0 -> not_a_multiple string_of_int m
  | Some _ | None -> []

let int64 (b : S.int64_bounds) n =
  (match b.min with
    | Some m when Int64.compare n m < 0 -> at_least Int64.to_string m
    | Some _ | None -> [])
  @ (match b.max with
    | Some m when Int64.compare n m > 0 -> at_most Int64.to_string m
    | Some _ | None -> [])
  @
  match b.multiple_of with
  | Some m when (not (Int64.equal m 0L)) && not (Int64.equal (Int64.rem n m) 0L)
    ->
      not_a_multiple Int64.to_string m
  | Some _ | None -> []

let spelled f =
  let b = Buffer.create 24 in
  Text.number b f;
  Buffer.contents b

(* Zod's own check, so a browser and the server decide alike: the quotient
   within four epsilons of a whole number, scaled by its size, since the
   value and the step were each rounded to a double before the division
   rounded again -- 19.99 is a multiple of 0.01, though 19.99 /. 0.01 is
   1998.9999999999998. *)
let multiple f m =
  let ratio = f /. m in
  Float.abs (ratio -. Float.round ratio)
  < 4. *. epsilon_float *. Float.max (Float.abs ratio) 1.

let number (b : S.number_bounds) f =
  (match b.min with
    | Some m when not (f >= m) -> at_least spelled m
    | Some _ | None -> [])
  @ (match b.max with
    | Some m when not (f <= m) -> at_most spelled m
    | Some _ | None -> [])
  @ (match b.above with
    | Some m when not (f > m) ->
        [
          ( P.Too_small,
            Printf.sprintf "This must be greater than %s." (spelled m) );
        ]
    | Some _ | None -> [])
  @ (match b.below with
    | Some m when not (f < m) ->
        [
          (P.Too_large, Printf.sprintf "This must be less than %s." (spelled m));
        ]
    | Some _ | None -> [])
  @
  match b.multiple_of with
  | Some m when not (multiple f m) -> not_a_multiple spelled m
  | Some _ | None -> []

let code_points s =
  String.fold_left
    (fun n c -> if Char.code c land 0xC0 = 0x80 then n else n + 1)
    0 s

let counted noun k =
  if k = 1 then "1 " ^ noun else Printf.sprintf "%d %ss" k noun

let length (l : S.length) s =
  match (l.min_length, l.max_length) with
  | None, None -> []
  | min, max -> (
      let n = code_points s in
      let chars k = counted "character" k in
      (match min with
        | Some m when n < m ->
            [
              ( P.Too_short,
                Printf.sprintf "This must be at least %s long." (chars m) );
            ]
        | Some _ | None -> [])
      @
      match max with
      | Some m when n > m ->
          [
            ( P.Too_long,
              Printf.sprintf "This must be at most %s long." (chars m) );
          ]
      | Some _ | None -> [])

let count noun ~min ~max n =
  (match min with
    | Some m when n < m ->
        [
          ( P.Too_few,
            Printf.sprintf "This must have at least %s." (counted noun m) );
        ]
    | Some _ | None -> [])
  @
  match max with
  | Some m when n > m ->
      [
        ( P.Too_many,
          Printf.sprintf "This must have at most %s." (counted noun m) );
      ]
  | Some _ | None -> []

let items = count "item"
let members = count "member"
