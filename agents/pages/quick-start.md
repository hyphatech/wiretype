# Quick start

Describe a JSON shape once, or derive it from your type. That one value
reads a document and reports every problem in it, writes a value, checks
the bounds it states, and prints itself as JSON Schema 2020-12, as Zod and
as Melange, so a server, a browser and a model's tool call read
the same contract.

## Read a document

A line of an order, with bounds on its quantity and its note, in a module
of its own:

```ocaml title="line.ml"
type line = {
  sku : string;
  quantity : int; [@min 1] [@max 99]  (** how many *)
  note : string option; [@max_length 200]
}
[@@deriving wiretype]
(** A line of an order. *)
```

A program reads a document that gets both wrong:

```ocaml title="quick_start.ml"
module J = Wiretype

let () =
  match
    J.decode Line.line_json {|{"sku": "A1", "quantity": 120, "note": 7}|}
  with
  | Ok _ -> ()
  | Error problems ->
      List.iter (fun p -> print_endline (J.Problem.to_string p)) problems
```

```console
$ dune exec ./quick_start.exe
quantity: This must be at most 99. (too_large)
note: This must be text, not a number. (unexpected_type)
```

What happened:

- `[@@deriving wiretype]` wrote `line_json`, the type's description. The
  doc comments are its docs, and `[@min]`, `[@max]` and `[@max_length]`
  its bounds. [The deriver](guide/deriver.md) lists every
  attribute.
- `J.decode` read the document in one pass and found both problems, each
  at its path with a code a client branches on and a sentence for a
  person. [Reading](guide/reading.md) says what else it checks.

## Print the schemas

The same description, as the schemas a client reads -- an answer here;
walked as a request (`Decode`) it is `LineInput`, whose `note` may be
`null`:

```ocaml title="schemas.ml"
module J = Wiretype

let ctx = J.Schema.create ()
let root = J.Schema.walk ctx Encode ~at:"line" Line.line_json

let () =
  match J.Schema.errors ctx with
  | [] -> ()
  | e :: _ ->
      prerr_endline (J.Schema.error_to_string e);
      exit 1

let json_schema = J.Schema.Json_schema.document (J.Schema.components ctx) root
let zod = J.Schema.Zod.components (J.Schema.components ctx)

let () =
  print_endline (J.Value.to_string json_schema);
  print_string zod
```

`zod` is this TypeScript:

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

and `json_schema` this JSON Schema:

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$ref": "#/$defs/Line",
  "$defs": {
    "Line": {
      "description": "A line of an order.",
      "type": "object",
      "properties": {
        "sku": { "type": "string" },
        "quantity": { "type": "integer", "minimum": 1, "maximum": 99, "description": "how many" },
        "note": { "type": "string", "maxLength": 200 }
      },
      "required": ["sku", "quantity"]
    }
  }
}
```

## Where next

- [Descriptions by hand](guide/by-hand.md), for a shape the
  deriver does not write.
- [JSON Schema](guide/json-schema.md), [Zod](guide/zod.md)
  and [Melange](guide/melange.md), for what each printer
  writes.
