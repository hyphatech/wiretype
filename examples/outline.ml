module S = Wiretype.Shape

(* What a description reads, in a few words. *)
let rec sort : type a. a S.t -> string = function
  | Null _ -> "null"
  | Bool -> "a boolean"
  | Int _ | Int64 _ -> "an integer"
  | Number _ -> "a number"
  | String _ -> "text"
  | Enum (_, e) -> "one of " ^ String.concat ", " (List.map fst e.words)
  | List l -> "a list of " ^ sort l.elt
  | Tuple _ -> "a tuple"
  | Dict d -> "a map of " ^ sort d.value
  | Nullable t -> sort t ^ ", or null"
  | Object (about, _) -> named about "an object"
  | Any (about, _) -> named about "a value of several sorts"
  | Map (about, m) -> named about (sort m.inner)
  | Value -> "any JSON"
  | Rec _ -> "a description of itself"

and named (about : S.about) otherwise =
  if String.equal about.kind "" then otherwise else about.kind

(* An object's members, each with what it reads, in the order declared. *)
let rec members : type o f. (o, f) S.fields -> (string * string) list = function
  | Build _ | Unread -> []
  | Mem (rest, m) -> members rest @ [ (m.name, sort m.shape) ]
  | Cases (rest, Tagged t) -> members rest @ [ (t.name, "the tag") ]

let print_members (type o) (description : o S.t) =
  match description with
  | Object (_, o) ->
      List.iter
        (fun (name, what) -> Printf.printf "%s: %s\n" name what)
        (members o.fields)
  | _ -> print_endline (sort description)
