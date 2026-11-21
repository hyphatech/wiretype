# Zod

`Schema.Zod.components` prints the components of a walk as TypeScript,
with the functions of Zod's `zod/mini` module, which tree-shake. Each
component is an `export const <Name>Schema` and its `type`, in an order
where a reference to one written later is a getter.

## Printing

```ocaml title="zod.ml"
module J = Wiretype

type point = { x : float; y : float } [@@deriving wiretype]

(** Something drawn. *)
type drawing = Dot of point | Line of { from : point; to_ : point }
[@@deriving wiretype]

type note = {
  id : int; [@read_only] [@default 0]
  text : string; [@max_length 500]
  drawing : drawing;
  hidden : bool option;
}
[@@deriving wiretype]
(** A note on a drawing. *)

let () =
  let ctx = J.Schema.create () in
  ignore (J.Schema.walk ctx Decode ~at:"body" note_json : J.Schema.t);
  ignore (J.Schema.walk ctx Encode ~at:"answer" note_json : J.Schema.t);
  match J.Schema.errors ctx with
  | e :: _ ->
      prerr_endline (J.Schema.error_to_string e);
      exit 1
  | [] ->
      print_string {|import * as z from "zod/mini";|};
      print_string "\n\n";
      print_string (J.Schema.Zod.components (J.Schema.components ctx))
```

```ts
import * as z from "zod/mini";

export const PointSchema = z.object({
  x: z.number(),
  y: z.number(),
});
export type Point = z.infer<typeof PointSchema>;

/** Something drawn. */
export const DrawingSchema = z.discriminatedUnion("type", [
  z.object({
    type: z.literal("dot"),
    x: z.number(),
    y: z.number(),
  }),
  z.object({
    type: z.literal("line"),
    from: PointSchema,
    to: PointSchema,
  }),
]);
export type Drawing = z.infer<typeof DrawingSchema>;

/** A note on a drawing. */
export const NoteInputSchema = z.object({
  text: z.string().check(z.maxLength(500)),
  drawing: DrawingSchema,
  hidden: z.optional(z.nullable(z.boolean())),
});
export type NoteInput = z.infer<typeof NoteInputSchema>;

/** A note on a drawing. */
export const NoteSchema = z.object({
  id: z.optional(z.int()),
  text: z.string().check(z.maxLength(500)),
  drawing: DrawingSchema,
  hidden: z.optional(z.boolean()),
});
export type Note = z.infer<typeof NoteSchema>;
```

The request's schema is `NoteInput`: it has no `id`, which is read-only,
and its `hidden` may be `null`. The answer's is `Note`. A doc is the
comment on the declaration or the member it stands for, and a deprecated
member's comment says `@deprecated`.

`Zod.of_t` prints one schema, as the expression it is.

## Checked against Zod

The Zod wiretype prints is checked in TypeScript, against Zod itself, on
every test run. Each ready-made [kind](kinds.md) accepts no
string the check it prints refuses: one file of rows is read by wiretype's
own suite and by Zod's, so a browser never turns away what the server
would take.

## Said exactly

A bound is wiretype's to decide, and the printer says it exactly. Zod's
`multipleOf` decides a number's as wiretype does, and an integer's loosely,
so an integer's is printed as an exact `z.refine`. An integer past 2^53 is
one a JavaScript client cannot hold exactly, and `z.int()` refuses it: an
id that may be larger travels as text, through
[a kind of your own](by-hand.md#kinds-of-your-own).
