# The deriver, every attribute

`[@@deriving wiretype]` writes a type's description as the value
`<type>_json` -- `json` for a type named `t` -- and, in a signature, its
`val`. It reads records, enums, tagged unions, polymorphic variants, type
parameters and recursive types. Bounds, defaults, `[@with d]`, read-only,
write-only, deprecated and examples are attributes, and a doc comment is
the doc.

What it writes is the library's public combinators, so a derived
description is one you could have [written by hand](by-hand.md).

## Records

A record is an object named by the type -- a `t` by its module -- each
field a member:

```ocaml title="product.ml"
type product = {
  id : Uuidm.t; [@read_only] [@default Uuidm.nil]  (** given by the server *)
  name : string; [@min_length 1] [@max_length 80]
  price : float; [@above 0.] [@multiple_of 0.01]
  stock : int; [@default 0] [@min 0]
  tags : string list; [@max_items 10] [@default []]
  colour : string option; [@key "color"] [@examples [ "red" ]]
  code : string; [@wiretype.deprecated]  (** use [id] *)
  secret : string option; [@write_only]
}
[@@deriving wiretype]
(** A product in the catalogue. *)
```

On a field:

| Attribute | What it does |
|---|---|
| `[@default e]` | a missing member reads as `e` |
| `[@key "k"]` | names the member on the wire |
| `[@with d]` | describes the field by `d`; on an `option`, the inner type's |
| `[@min]`, `[@max]`, `[@multiple_of]` | bound an `int`, `int64` or `float` |
| `[@above]`, `[@below]` | bound a `float`, greater and less than |
| `[@min_length]`, `[@max_length]` | bound a `string`, in code points |
| `[@min_items]`, `[@max_items]` | bound a `list` |
| `[@dict]` | a `(key * value) list` is a map, below |
| `[@min_properties]`, `[@max_properties]` | bound a map |
| `[@read_only]` | in answers alone: never read, and in no request's schema |
| `[@write_only]` | in requests alone: never written, and in no answer's schema |
| `[@wiretype.deprecated]` | the schema says it is deprecated |
| `[@examples [...]]` | the schema's examples; on an `option`, values of the inner type |

An `option` field may be left out or `null`, as `Object.opt_mem` reads it.
An option of an option, or of `unit`, is refused wherever it is written --
a field, a list's item, a tuple's -- since `None` and `Some None` would
both be written `null`. A least and a most written as literals with no
value between them are refused. A read-only field is never read, so one
with no `[@default e]` that is no `option` is refused, since nothing would
build it.

A field's doc comment is its doc, as the type's is the object's, the
enum's or the union's. OCaml's own `[@deprecated]` is OCaml's, never a
document's: a field or a constructor it marks is derived as any other, and
what is derived uses it with no alert.

## Enums

A variant of constant constructors is an enum, a word per constructor: its
name with the first letter lowered, or `[@name "w"]`:

```ocaml title="status.ml"
type status = Draft | Published | Archived [@name "gone"]
[@@deriving wiretype]
```

## Unions

A variant with arguments is a union on a tag -- `"type"`, or `[@@tag "k"]`,
which only a union has. Each case is an inline record's members, the
members of its one argument's object, or none:

```ocaml title="drawing.ml"
type point = { x : float; y : float } [@@deriving wiretype]

(** Something drawn. *)
type drawing = Dot of point | Line of { from : point; to_ : point } | Blank
[@@deriving wiretype] [@@tag "kind"]
```

A case whose one argument is a scalar, a time, a UUID, a list, an array, an
option or a tuple is refused, since a case is an object's members: write an
inline record instead.

## Polymorphic variants

A polymorphic variant is either an enum or a union, the same way:

```ocaml title="answer.ml"
type answer = [ `Yes | `No | `Maybe ] [@@deriving wiretype]
```

## Names

`[@@rename_all camel]`, `kebab` or `snake` (the default) spells every
member and word from its OCaml name, and a trailing `_` after a keyword --
`type_` -- is dropped. `[@@kind "k"]` names the object:

```ocaml title="person.ml"
type t = { first_name : string; last_name : string; type_ : string }
[@@deriving wiretype] [@@rename_all camel] [@@kind "person"]
```

## Type parameters

A type parameter `'a` is an argument, its description, in order:

```ocaml title="page.ml"
type 'a page = { items : 'a list; next : string option } [@@deriving wiretype]

let products = page_json Product.product_json
```

A type with parameters has no kind unless `[@@kind]` gives it one, so a
schema writes it where it is used -- `'a page` a page of users in one
answer and of orders in another -- since a kind names one component, and
two arguments under one are a schema's `Kind_shared` error.

## Recursive types

A type of the group that refers to itself is described through `J.rec'`:

```ocaml title="tree.ml"
type tree = { label : string; children : tree list } [@@deriving wiretype]
```

A recursive type with no parameter is built as its module loads; one with
parameters is a function, each of whose descriptions is built a level at a
time as it is first read, written or walked. Either may be shared between
domains.

## Every other type

```ocaml title="event.ml"
type event = {
  at : Ptime.t;
  lasted : Mtime.Span.t;
  where : float * float;
  scores : (string * int) list; [@dict] [@max_properties 20]
  extra : Wiretype.Value.t;
  state : Status.status;
  total : float; [@with Wiretype.number_bounded ~min:0. ()]
}
[@@deriving wiretype]
```

- An `int`, `int64`, `float`, `string` and `bool` are their own
  descriptions, and `unit` is `null`. A `list` and an `array` are a JSON
  array, and an `option` is its type or `null`.
- A `Ptime.t` is an instant (`J.instant`), an `Mtime.Span.t` or an
  `Mtime.span` a duration (`J.duration`), and a `Uuidm.t` a UUID
  (`J.uuid ()`).
- A tuple is a JSON array of exactly its items, each by its own
  description.
- A `(key * value) list` marked `[@dict]` -- on its field, or on the type
  itself -- is a map (`J.dict`). Unmarked, it is a list of pairs, since
  which one it is cannot be read from the type.
- Any other type `M.t` is described by `M.json`, and `u` by `u_json`. A
  `Wiretype.Value.t`, by whatever alias it is written, is any JSON.
- An `int32`, a `nativeint`, a `char` and `bytes` have no description, and
  are refused, saying what to write instead.

Each of these is the same by its stdlib module's name -- `String.t`,
`'a List.t`, `Stdlib.Int32.t`. The deriver reads only the name written, so
a module of one of these names declared here is read as the stdlib's, or
as Ptime's, Mtime's or Uuidm's: its type is described with
`[@with String.json]`.

## What it prints

The descriptions above, walked as answers and printed as Zod:

```ocaml title="deriver.ml"
module J = Wiretype

let () =
  let ctx = J.Schema.create () in
  let walk name description =
    ignore (J.Schema.walk ctx Encode ~at:name description : J.Schema.t)
  in
  walk "product" Product.product_json;
  walk "drawing" Drawing.drawing_json;
  walk "answer" Answer.answer_json;
  walk "person" Person.json;
  walk "products" Page.products;
  walk "tree" Tree.tree_json;
  walk "event" Event.event_json;
  print_string (J.Schema.Zod.components (J.Schema.components ctx))
```

Of what it prints, the product, the drawing, the person and the tree.
`secret` is write-only, so no answer has it; `answer` and `products` have
no kind, so neither is a component, and each would be written where it is
used:

```ts
/** A product in the catalogue. */
export const ProductSchema = z.object({
  /** given by the server */
  id: z.optional(z.uuid()),
  name: z.string().check(z.minLength(1), z.maxLength(80)),
  price: z.number().check(z.gt(0), z.multipleOf(0.01)),
  stock: z.optional(z.int().check(z.gte(0))),
  tags: z.optional(z.array(z.string()).check(z.maxLength(10))),
  color: z.optional(z.string()),
  /** @deprecated use [id] */
  code: z.string(),
});
export type Product = z.infer<typeof ProductSchema>;

/** Something drawn. */
export const DrawingSchema = z.discriminatedUnion("kind", [
  z.object({
    kind: z.literal("dot"),
    x: z.number(),
    y: z.number(),
  }),
  z.object({
    kind: z.literal("line"),
    from: PointSchema,
    to: PointSchema,
  }),
  z.object({
    kind: z.literal("blank"),
  }),
]);
export type Drawing = z.infer<typeof DrawingSchema>;

export const PersonSchema = z.object({
  firstName: z.string(),
  lastName: z.string(),
  type: z.string(),
});
export type Person = z.infer<typeof PersonSchema>;

export const TreeSchema = z.object({
  label: z.string(),
  get children() {
    return z.array(TreeSchema);
  },
});
export type Tree = z.infer<typeof TreeSchema>;
```

## What it refuses

An attribute spelt `[@wiretype.x]` for no `x` above, or one of these where
it is not read -- a `[@key]` on a constructor -- is refused, since it would
say nothing; a short name that is none of these is left to another
deriver.

A function, an object type and a variant constructor with more than one
argument have no JSON shape here. Each refusal is an error at its place in
the derived code, with what to write instead, so an editor shows every one
in a file, and the rest of the file is still derived.

The deriver refuses at compile time what it sees in the type: a name given
twice, two constructors written alike, an option of an option or of
`unit`, a least and a most written as literals with no value between them,
a read-only field with no default that is no option, and a case whose one
argument is a scalar, a list, an option or a tuple. The rest it leaves to
the library, when the program starts: a bound that means nothing alone,
though written as a literal (`[@multiple_of 0]`, a negative
`[@min_length]`), and what it cannot see, such as a case whose one argument
is a type named elsewhere that is no object.
