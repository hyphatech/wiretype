# Descriptions by hand

The deriver writes what you could write yourself. A description is an
ordinary OCaml value of type `'a Wiretype.t`, built from the library's
combinators, and one written by hand reads, writes and prints exactly as a
derived one does.

## An object

The quick start's line, by hand:

```ocaml title="line_by_hand.ml"
module J = Wiretype

type line = { sku : string; quantity : int; note : string option }

let line_json =
  J.Object.map ~kind:"line" ~doc:"A line of an order." (fun sku quantity note ->
      { sku; quantity; note })
  |> J.Object.mem "sku" J.string ~enc:(fun l -> l.sku)
  |> J.Object.mem "quantity" (J.int_bounded ~min:1 ~max:99 ()) ~doc:"how many"
       ~enc:(fun l -> l.quantity)
  |> J.Object.opt_mem "note" (J.string_bounded ~max_length:200 ())
       ~enc:(fun l -> l.note)
  |> J.Object.finish
```

`Object.map` takes the function that builds the value, and each
`Object.mem` gives it one argument, in order. `~enc` is how the member is
found in a value; an object whose members have no `~enc` only reads.
`~kind` names the object, and makes it a component of a schema; `~doc` is
what it is, for whoever reads the schema.

A member is one of:

- `Object.mem`: required, unless `~absent` gives the value it reads as
  when left out. `~omit` says which values are written by leaving the
  member out, and needs `~absent` where the object is read.
- `Object.opt_mem`: a member that may be left out. Absent or `null` reads
  as `None`, since clients write both for "nothing", and `None` is written
  by leaving it out.
- `Object.case_mem`: a union's tag, below.

Each takes `~doc`, `~deprecated` and `~examples`, which a schema prints,
and `~access`, which says which of a request and an answer the member is in
([Writing](writing.md#read-only-and-write-only) says how).
`~field` names the OCaml field a member is in, where that is not its name:
the [Melange printer](melange.md) names its record's field
so.

A member the object does not describe is skipped, unless
`Object.error_unknown` makes it a problem. `Object.enc_only` starts an
object that is written and never read -- a request to another server --
whose members need no function to build a value from.

## Scalars

`J.null v`, `J.bool`, `J.int`, `J.int64`, `J.number` and `J.string` are
JSON's scalars. `J.int_bounded`, `J.int64_bounded`, `J.number_bounded` and
`J.string_bounded` state bounds, which [Bounds](bounds.md)
covers. The ready-made kinds -- instants, dates, durations, UUIDs, base64,
URIs and IP addresses -- are on [their own page](kinds.md).

## Enums

An enum writes each value as its word, and reads it back from it. Its
words are in the schema, exactly:

```ocaml title="size.ml"
module J = Wiretype

type size = Small | Medium | Large

let size_json =
  J.enum ~kind:"size"
    (function Small -> "small" | Medium -> "medium" | Large -> "large")
    [ Small; Medium; Large ]
```

## Lists, tuples, maps and nullables

```ocaml title="collections.ml"
module J = Wiretype

let tags = J.list ~max_items:10 J.string
let point = J.tuple2 J.number J.number
let scores = J.dict ~key:J.string ~value:J.int ()
let parent = J.nullable (J.uuid ())
```

- `J.list` is a JSON array of one description, with an optional count.
- `J.tuple2`, `J.tuple3` and `J.tuple4` are arrays of exactly that many
  items, each read by its own description; `J.Tuple` builds one of any
  length as an object is built, a function and each item it takes.
- `J.dict` is a JSON object read as a map: every member, in the document's
  order, its name read by `~key` as the JSON string it is written as --
  `J.string`, an enum, a kind over text such as `J.uuid` -- and its value
  by `~value`. A name that is wrong is a problem at the name.
- `J.nullable` is the description, or `null` for `None`.

## Unions on a tag

A union is an object whose tag member -- read first, wherever it is --
decides which case's members the rest are:

```ocaml title="figure.ml"
module J = Wiretype

type figure = Circle of float | Square of float

let circle =
  J.Object.Case.map "circle"
    (J.Object.map Fun.id
    |> J.Object.mem "radius" J.number ~enc:Fun.id
    |> J.Object.finish)
    ~dec:(fun r -> Circle r)

let square =
  J.Object.Case.map "square"
    (J.Object.map Fun.id
    |> J.Object.mem "side" J.number ~enc:Fun.id
    |> J.Object.finish)
    ~dec:(fun s -> Square s)

let figure_json =
  J.Object.map ~kind:"figure" Fun.id
  |> J.Object.case_mem "type" J.string ~enc:Fun.id
       ~enc_case:(function
         | Circle r -> J.Object.Case.value circle r
         | Square s -> J.Object.Case.value square s)
       [ J.Object.Case.make circle; J.Object.Case.make square ]
  |> J.Object.finish
```

`Object.Case.map tag object ~dec` is the case `tag` stands for, whose
members are `object`'s; without `~dec` it is written and never read.
`~enc_case` gives the case of a value, and the tag is written first. An
object has at most one union, since the tag is read before the members it
decides. `~absent` is the tag of a value that has none.

## Values of several sorts

`J.any` reads a value by the description of the JSON sort it is, and
writes it by the one `~enc` picks:

```ocaml title="flexible_id.ml"
module J = Wiretype

(* An id a client may send as a number or as text, always written as a
   number. *)
let id_json =
  J.any ~dec_number:J.int
    ~dec_string:
      (J.kind ~kind:"id"
         ~parse:(fun s ->
           match int_of_string_opt s with
           | Some n -> Ok n
           | None -> Error "This must be a whole number.")
         J.string)
    ~enc:(fun _ -> J.int)
    ()
```

## Kinds of your own

A value with a grammar of its own -- an id, a coordinate -- is a kind over
another description. `~parse`'s `Error` is a sentence for a person, and a
`malformed` problem at the value's place:

```ocaml title="order_number.ml"
module J = Wiretype

let is_digit c = c >= '0' && c <= '9'

(* An order's number, written as text: ord_ and its digits. *)
let order_number_json =
  J.kind ~kind:"order number" ~doc:"An order, as ord_ and its digits."
    ~parse:(fun s ->
      match String.split_on_char '_' s with
      | [ "ord"; digits ]
        when String.length digits > 0 && String.for_all is_digit digits -> (
          match int_of_string_opt digits with
          | Some n -> Ok n
          | None -> Error "This number is too large.")
      | _ -> Error "This must be ord_ and its digits.")
    ~print:(fun n -> Printf.sprintf "ord_%d" n)
    J.string
```

Without `~print`, a kind is only read. `J.map` is a plain change of type,
`~parse` from what the inner description reads and `~print` back; a
direction left out is one the description is never used in, and a `parse`
that can fail is a kind's. `~format` gives a kind a schema's format, from
`Shape.format`.

## Descriptions of themselves

`J.rec'` takes a lazy description, so a description can hold itself:

```ocaml title="comment.ml"
module J = Wiretype

type comment = { text : string; replies : comment list }

let rec comment =
  lazy
    (J.Object.map ~kind:"comment" (fun text replies -> { text; replies })
    |> J.Object.mem "text" J.string ~enc:(fun c -> c.text)
    |> J.Object.mem "replies"
         (J.list (J.rec' comment))
         ~absent:[]
         ~enc:(fun c -> c.replies)
    |> J.Object.finish)

let comment_json = J.rec' comment
```

The lazy is forced where the description is first read, written or walked,
by whichever domain reaches it first, while another reaching it then waits,
so a description may be shared between domains from the start. One that
comes back to itself before it reads or writes anything --
`let rec t = lazy (rec' t)` -- describes nothing: `decode` refuses what is
there as `malformed`, and `encode` the value as `unspellable`.

## What these write and read

A program writes a value of each of the modules above, and reads a few
documents back:

```ocaml title="by_hand.ml"
module J = Wiretype

let show name = function
  | Ok s -> Printf.printf "%s: %s\n" name s
  | Error (u : J.Unwritable.t) ->
      Printf.printf "%s: %s\n" name (J.Unwritable.to_string u)

let read name description text =
  match J.decode description text with
  | Ok _ -> Printf.printf "%s: read\n" name
  | Error problems ->
      Printf.printf "%s: %s\n" name (J.Problem.list_to_string problems)

let () =
  show "line"
    (J.encode Line_by_hand.line_json { sku = "A1"; quantity = 2; note = None });
  show "size" (J.encode Size.size_json Medium);
  show "tags" (J.encode Collections.tags [ "tea"; "green" ]);
  show "point" (J.encode Collections.point (1.5, -2.));
  show "scores" (J.encode Collections.scores [ ("ada", 3); ("alan", 2) ]);
  show "parent" (J.encode Collections.parent None);
  show "figure" (J.encode Figure.figure_json (Circle 2.));
  show "id" (J.encode Flexible_id.id_json 7);
  show "order number" (J.encode Order_number.order_number_json 42);
  show "comment"
    (J.encode Comment.comment_json
       { text = "First"; replies = [ { text = "Second"; replies = [] } ] });
  read "figure" Figure.figure_json {|{"side": 3, "type": "square"}|};
  read "figure" Figure.figure_json {|{"type": "hexagon"}|};
  read "id" Flexible_id.id_json {|"7"|};
  read "id" Flexible_id.id_json {|"seven"|};
  read "order number" Order_number.order_number_json {|"ord_x"|}
```

```console
$ dune exec ./by_hand.exe
line: {"sku":"A1","quantity":2}
size: "medium"
tags: ["tea","green"]
point: [1.5,-2]
scores: {"ada":3,"alan":2}
parent: null
figure: {"type":"circle","radius":2}
id: 7
order number: "ord_42"
comment: {"text":"First","replies":[{"text":"Second","replies":[]}]}
figure: read
figure: type: This must be one of "circle" or "square". (unknown_word)
id: read
id: the document: This must be a whole number. (malformed)
order number: the document: This must be ord_ and its digits. (malformed)
```

## A description that can mean nothing

A description is a constant written in source, so one that can mean
nothing raises `Invalid_argument` where it is built: the mistake is found
when the program starts, or in its first test, and never on a request.
Among them are a bound no value meets, two enum values with one word, a
member described twice, an object with two unions, a member written by
leaving it out that is read as required, and a nullable over what already
writes `null`. Each function's interface says when it raises.

A description is `Wiretype.Shape.t`, a public GADT, so you can
[walk it too](walking.md).
