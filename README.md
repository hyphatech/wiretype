# wiretype

[![ci](https://img.shields.io/github/actions/workflow/status/hyphatech/wiretype/ci.yml?branch=main&label=ci)](https://github.com/hyphatech/wiretype/actions/workflows/ci.yml)
[![release](https://img.shields.io/github/v/release/hyphatech/wiretype?label=release)](https://github.com/hyphatech/wiretype/releases)
[![license](https://img.shields.io/github/license/hyphatech/wiretype)](LICENSE)
![OCaml 5.4+](https://img.shields.io/badge/OCaml-5.4%2B-EC6813?logo=ocaml&logoColor=white)

JSON contracts for OCaml.

Describe a JSON shape once, or derive it from your type. That one value
reads a document and reports every problem in it, writes a value, checks
the bounds it states, and prints itself as JSON Schema 2020-12 and as zod,
so a server, a browser and a model's tool call read the same contract.
Depends on nothing.

## Install

```sh
opam pin add https://github.com/hyphatech/wiretype.git
```

```lisp
(libraries wiretype)
(preprocess (pps ppx_wiretype))
```

## Quick start

```ocaml
module J = Wiretype

(** A line of an order. *)
type line = {
  sku : string;
  quantity : int; [@min 1] [@max 99]  (** how many *)
  note : string option; [@max_length 200]
} [@@deriving wiretype]

let () =
  match J.decode line_json {|{"sku": "A1", "quantity": 120, "note": 7}|} with
  | Ok _ -> ()
  | Error problems ->
      List.iter (fun p -> print_endline (J.Problem.to_string p)) problems
```

```
quantity: This must be at most 99. (too_large)
note: This must be text, not a number. (unexpected_type)
```

The same description, as the schemas a client reads -- an answer here;
walked as a request (`Decode`) it is `LineInput`, whose `note` may be
`null`:

```ocaml
let ctx = J.Schema.create ()
let root = J.Schema.walk ctx Encode ~at:"line" line_json
let json_schema = J.Schema.Json_schema.document (J.Schema.components ctx) root
let zod = J.Schema.Zod.components (J.Schema.components ctx)
```

```ts
/** A line of an order. */
export const LineSchema = z.object({
  sku: z.string(),
  /** how many */
  quantity: z.int().check(z.gte(1), z.lte(99)),
  note: z.optional(z.string().check(z.maxLength(200))),
});
export type Line = z.infer<typeof LineSchema>;
```

## By hand

The deriver writes what you could write yourself:

```ocaml
J.Object.map ~kind:"line" ~doc:"A line of an order." (fun sku quantity note ->
    { sku; quantity; note })
|> J.Object.mem "sku" J.string ~enc:(fun l -> l.sku)
|> J.Object.mem "quantity" (J.int_bounded ~min:1 ~max:99 ()) ~doc:"how many"
     ~enc:(fun l -> l.quantity)
|> J.Object.opt_mem "note" (J.string_bounded ~max_length:200 ()) ~enc:(fun l ->
    l.note)
|> J.Object.finish
```

Beside objects there are enums, lists, tuples, maps (`J.dict`), nullables,
unions on a tag (`Object.case_mem`), values of several JSON sorts
(`J.any`), kinds of your own (`J.kind`) and recursive descriptions
(`J.rec'`). A description is `Wiretype.Shape.t`, a public GADT, so you can
walk it too.

## Reading and writing

- **Every problem, in one pass**: each with its path (`items[2].count`), a
  code a client branches on (`required`, `too_large`, `unknown_word`, ...)
  and a sentence for a person.
- **Exact numbers**: an `int` is read from its digits, and a double is
  written in the fewest digits that read back as itself.
- **Strict where readers disagree**: UTF-8 is checked, and a member given
  twice is refused, as I-JSON has it. JSONTestSuite runs whole.
- **Absent and `null` said once**: `opt_mem` reads either as `None` and
  writes `None` by leaving the member out; `mem ~absent` gives a default.
- **What it writes, it reads**: text that is not UTF-8, a member written
  twice or nesting past 512 is refused on writing, at its path with a
  code (`J.Unwritable`), as a kind's value it cannot spell is.

## Kinds

Each is a plain OCaml type, with its schema's `format` and its zod check,
and accepts no string that check refuses.

| Kind | OCaml | Reads |
|---|---|---|
| `J.instant` | `int`, epoch ms | RFC 3339 `date-time` |
| `J.date` | `int * int * int` | `YYYY-MM-DD` |
| `J.duration` | `int`, ms | ISO 8601, weeks or days to seconds |
| `J.uuid ?version ()` | `string` | RFC 9562 |
| `J.base64`, `J.base64url` | `string`, the bytes | RFC 4648 |
| `J.uri` | `string` | RFC 3986, held to the URL standard, IDNA included |
| `J.ipv4`, `J.ipv6` | `string` | dotted decimal, IPv6 |

## Schemas

`Wiretype.Schema` walks a description once and prints it as JSON Schema
2020-12 and as zod/mini, so the two cannot disagree. A named object is a
component; a request's schema and an answer's differ where a member is
read-only, write-only or optional, and the request's is then
`<Name>Input`, whatever else is walked. An object that refuses members it
does not describe says so in both. Bounds -- ranges, `multiple_of`,
lengths, counts -- are in both.

## The deriver

`[@@deriving wiretype]` writes `<type>_json` (`json` for a type named `t`)
from records, enums, tagged unions, polymorphic variants, type parameters
and recursive types. Bounds, defaults, `[@with d]`, read-only, write-only,
deprecated and examples are attributes, and a doc comment is the doc.
[Its `.mli`](ppx/ppx_wiretype.mli) lists every attribute.

Each module's `.mli` is its reference.

## What it does not do

- Keep members an object does not describe.
- Indent its output.
- Say where a problem is by line and column; it says by path.
- Read a document as it arrives, or one larger than memory.
- Write a 64-bit integer as a string.
- Query or edit a `Value.t` by path.

## Packages

| Package | What it is |
|---|---|
| `wiretype` | descriptions: reading, writing, bounds, kinds, schemas; links nothing |
| `ppx_wiretype` | `[@@deriving wiretype]`, which writes a type's description |

## Contributing

See [AGENTS.md](AGENTS.md).

## Licence

MIT, copyright Hypha Technologies Ltd, but for the Unicode data in
`src/unicode/` and `test/idna-test/`, which is Unicode's. See
[LICENSE](LICENSE).
