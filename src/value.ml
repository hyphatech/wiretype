type t =
  | Null
  | Bool of bool
  | Number of float
  | String of string
  | Array of t list
  | Object of (string * t) list

let rec equal a b =
  match (a, b) with
  | Null, Null -> true
  | Bool x, Bool y -> Bool.equal x y
  | Number x, Number y -> Float.equal x y
  | String x, String y -> String.equal x y
  | Array xs, Array ys -> List.equal equal xs ys
  | Object xs, Object ys ->
      List.equal (fun (n, x) (m, y) -> String.equal n m && equal x y) xs ys
  | (Null | Bool _ | Number _ | String _ | Array _ | Object _), _ -> false

let find name = function
  | Object members -> List.assoc_opt name members
  | Null | Bool _ | Number _ | String _ | Array _ -> None

let rec write b = function
  | Null -> Buffer.add_string b "null"
  | Bool x -> Buffer.add_string b (if x then "true" else "false")
  | Number f -> Text.number b f
  | String s -> Text.string b s
  | Array items ->
      Buffer.add_char b '[';
      List.iteri
        (fun i v ->
          if i > 0 then Buffer.add_char b ',';
          write b v)
        items;
      Buffer.add_char b ']'
  | Object members ->
      Buffer.add_char b '{';
      List.iteri
        (fun i (name, v) ->
          if i > 0 then Buffer.add_char b ',';
          Text.string b name;
          Buffer.add_char b ':';
          write b v)
        members;
      Buffer.add_char b '}'

let to_string v =
  let b = Buffer.create 256 in
  write b v;
  Buffer.contents b

let pp ppf v = Format.pp_print_string ppf (to_string v)
