# Walking a description

A description is `Wiretype.Shape.t`, a public GADT: the representation the
combinators make, and what everything that reads a description walks --
the decoder, the encoder, the schema printers, and anybody with a use of
their own. `'a Wiretype.t` is `'a Shape.t`.

It is the library's API and its promise. `Shape.t` is closed: a
constructor added to it is a new, breaking version, so a walk that matches
every case is told by the compiler.

## A walk of your own

A function over every case says what a description reads, in a few words,
and another lists an object's members:

```ocaml title="outline.ml"
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
```

A program uses it on a derived description:

```ocaml title="walk.ml"
type size = Small | Large [@@deriving wiretype]

type order = {
  id : Uuidm.t;
  placed : Ptime.t;
  lines : (string * int) list; [@dict]
  size : size;
  gift : string option;
}
[@@deriving wiretype]

let () = Outline.print_members order_json
```

```console
$ dune exec ./walk.exe
id: uuid
placed: instant
lines: a map of an integer
size: one of small, large
gift: text, or null
```

A walk over `'a Shape.t` is a function of type `type a. a Shape.t -> ...`,
since each case refines `a`: an `Int` is an `int Shape.t`, a `List` an
`'a list Shape.t`.

## The cases

| Case | A description of |
|---|---|
| `Null v` | JSON `null`, read as `v` |
| `Bool`, `Int`, `Int64`, `Number`, `String` | a scalar, with its bounds |
| `Enum (about, enum)` | a value written as its word |
| `List elements` | a JSON array of one description, with its counts |
| `Tuple items` | a JSON array of exactly its items |
| `Dict dict` | a JSON object read as a map, in the document's order |
| `Nullable t` | the description, or `null` |
| `Object (about, obj)` | an object: its members, and what it does with one it does not describe |
| `Any (about, any)` | a value of several JSON sorts, a description per sort |
| `Map (about, map)` | another type over an inner description: a kind, or a plain change of type |
| `Value` | any JSON |
| `Rec lazy_t` | a description of itself |

`about` is what a description is called -- a component's name, where it is
an object -- and what it is, for whoever documents it; both are empty when
it has neither. Each object `Object.finish` makes has an `about` of its
own, and the schema and the writer tell objects apart by it.

An object's `fields` are, in the order declared, the function that builds
its value (`Build`), or `Unread` for one that is written and never read,
and each member applied to it (`Mem`) or the union that depends on one
member's value (`Cases`). A member says its name, its OCaml field, its doc,
its description, what it reads as when absent, which values it writes by
leaving it out, how it is found in a value, its access, whether it is
deprecated, and its examples.

## Forcing a description of itself

A `Rec` holds a lazy. The library forces it where the description is first
read, written or walked, by whichever domain reaches it first, while
another then waits. A lazy forced outside the library is forced as OCaml
forces it: forced from two domains at once, one is told `Lazy.Undefined`.
A description of itself holds itself, so a walk that descends into every
`Rec` it meets can go round for ever; the walk above stops at one.

## Building one directly

A description is built with the combinators. One built from `Shape`'s
constructors directly is as good to read, since everything that reads one
checks what it reads, and is trusted to be written as `Object.finish`
would let it be: no member named twice in an object.
