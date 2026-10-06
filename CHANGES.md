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
