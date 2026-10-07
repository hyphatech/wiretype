# Changes

## Unreleased

- Breaking: text that is not JSON, or nested past the limit, is refused
  with that one problem alone. A plain object or list also reported the
  problems it had found before it and a union never did; now every
  description answers as a parse before the reading would. A value that is
  not JSON is no longer also said to be the wrong sort, guessed from its
  first byte: `nope` as "not null".
- A member a union refuses is said before any problem inside it, in the
  document's order, as a plain object's always was.

## 0.1.0 (2026-10-06)

First release.

- `wiretype`: a JSON shape described once -- scalars with their bounds,
  enums, lists, tuples, maps, nullables, objects, unions on a tag, values
  of several JSON sorts, kinds over another description, any JSON, and
  descriptions of themselves.
- Reading in one pass, with every problem found at its path as a code and
  a sentence; a member given twice, anywhere in a document, is refused.
- Writing minified, with each double in its fewest digits, refusing what
  reading would: text that is not UTF-8, a member given twice, nesting
  past 512.
- JSON Schema 2020-12 and zod/mini printed from one walk, a request's
  component named `<Name>Input` exactly where it differs by direction, and
  what cannot be said exactly reported as loose.
- Ready-made kinds for instants, dates, durations, UUIDs of RFC 9562's
  versions, base64, URIs and IP addresses, each accepting no string its zod
  check refuses, and each bound decided as zod decides it.
- `ppx_wiretype`: `[@@deriving wiretype]` for records, enums, tagged
  unions, polymorphic variants, type parameters and recursive types, with
  bounds, defaults, read-only and write-only members, docs and examples
  as attributes, writing only the public combinators and refusing at
  compile time, at its place, every type it cannot describe.
- A description that can mean nothing raises `Invalid_argument` where it
  is built, when the program starts; nothing else raises.
