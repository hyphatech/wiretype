# Changes

## Unreleased

First release.

- `wiretype`: a JSON shape described once -- scalars with their bounds,
  enums, lists, tuples, maps, nullables, objects, unions on a tag, values
  of several JSON sorts, kinds over another description, any JSON, and
  descriptions of themselves -- read in one pass with every problem found
  at its path, written minified with each double in its fewest digits, and
  printed as JSON Schema 2020-12 and as zod/mini from one walk; ready-made
  kinds for instants, dates, durations, UUIDs, base64, URIs and IP
  addresses, each accepting no string its zod check refuses.
- `ppx_wiretype`: `[@@deriving wiretype]` for records, enums, tagged
  unions, polymorphic variants, type parameters and recursive types, with
  bounds, defaults, docs and examples as attributes.
- A description that can mean nothing -- a `multiple_of` that is not
  positive, two values with one word, a member described twice, two
  unions in one object, a case with a union of its own -- raises
  `Invalid_argument` where it is built, and the deriver refuses at compile
  time the ones it can see in the type.
- `uuid ?version` takes RFC 9562's versions as `` `V1 `` to `` `V8 ``.
- `instant` refuses an instant outside the years 0000 to 9999 once in UTC,
  and `duration` one past a hundred thousand years, so whatever a kind
  reads it can write back; a duration's parts no longer overflow into a
  negative number of milliseconds.
- A case's `error_unknown` is kept when the union is read; a member given
  twice inside a union's member is one problem, not two; a bad `\u`
  escape is said at its first wrong digit.
- Schemas: a request's component is `<Name>Input` exactly where its
  description differs by direction, whatever else is walked and in what
  order, and two different descriptions under one kind are always an
  error; `error_unknown` is printed as `additionalProperties: false` and
  `z.strictObject`; a recursion with no kind is reported loose where its
  expansion stops; a kind that names no identifier and an example
  that cannot be written are errors; an `opt_mem` over a nullable answers
  as nullable. `Schema.obj.additional` is a variant (breaking).
- `Wiretype.value` is `Wiretype.Value.json` (breaking), the name the
  deriver writes for any `M.t`, so a field typed through any alias of
  `Wiretype.Value.t` is derived, and a user's own `Value.t` is no longer
  taken for it.
- `opt_mem` takes `read_only`, `write_only` and `examples`, so the deriver's
  attributes of those names work on an `option` field.
- The deriver writes each refusal as an error at its place instead of
  stopping at the first, and an `array` field no longer depends on which
  `Array` is in scope.
- A number's `multiple_of` is decided as zod's `multipleOf` is, so `19.99`
  is a multiple of `0.01`, where `19.99 /. 0.01` made it not one.
- `encode` and `to_value` return `Unwritable.t` -- a path, a code and a
  sentence -- in place of a string (breaking), and refuse what `decode`
  would: text that is not UTF-8, a member of any JSON given twice, nesting
  past 512. `Schema.errors` returns `Schema.error`, with a code (breaking).
- `mem ?read_only ?write_only` is `mem ?access` (breaking): one of
  `` `Read_write ``, `` `Read_only `` and `` `Write_only ``, so a member
  cannot be both; the deriver refuses both attributes on one field.
