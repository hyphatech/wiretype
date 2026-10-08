# Changes

## 0.2.0 (2026-10-08)

- Breaking: text that is not JSON, or nested past the limit, is refused
  with that one problem alone. A plain object or list also reported the
  problems it had found before it and a union never did; now every
  description answers as a parse before the reading would. A value that is
  not JSON is no longer also said to be the wrong sort, guessed from its
  first byte: `nope` as "not null".
- A member a union refuses is said before any problem inside it, in the
  document's order, as a plain object's always was.
- Breaking: an int's or an int64's `multiple_of` is printed to zod as an
  exact `z.refine`, since zod's `multipleOf` takes `3000000000000001` for a
  multiple of 3 and the reader, rightly, does not.
- Breaking: an `int` or `int64` past a double, as `1e400`, is `too_large`
  or `too_small`, where it was said to be a fraction (`unexpected_type`).
- Breaking: a path writes a member's name quoted as JSON where it is empty
  or holds anything but letters, digits, `_`, `-`, `$` and non-ASCII text
  (`items["a.b"]`), so a problem is one line for a log whatever a document
  names its members, and two paths are never spelt alike.
- Breaking: `encode` refuses, as `unspellable`, a float that is not finite,
  which it wrote `null` though `number` reads no `null`; a value an enum has
  no word for; and a case its union does not list. It refuses a member a
  description built by hand writes twice. `Object.finish` raises where an
  object that is read has a member or a tag with `omit` and no `absent`,
  which it wrote by leaving out and read as required.
- Breaking: `Schema.number` is gone; `Schema.Number` and `Schema.Integer`
  hold a `Shape.number_bounds`, which it repeated field for field.
- Breaking: the schemas say what the reader means or report that they
  cannot. A value of several sorts that nothing reads is `Never` (`{"not":
  {}}`, `z.never()`) in a request, and loose in an answer, where it was
  `null`; an integer's bound past 2^53 is left out and reported loose, where
  it was rounded; a UUID of one version has its `pattern` in JSON Schema; a
  member named `__proto__` is a computed key in zod, where it set the
  prototype. `Schema.t` gains `Never`.
- Breaking: `number_bounded` raises for a bound that is no finite number,
  which printed as `null` and could mean nothing.
- Breaking: two descriptions under one kind that differ only in a doc, an
  example or `deprecated` are a `kind_shared` error, where the second was
  dropped.
- A schema walks each component once, where a chain of kinds each used
  twice took twice as long for every link.
- A union's case may be an object reached through `rec'`, as a derived
  union whose case is a record of its own group is: `Object.Case.map`
  refused it as no object when the program started.
- `ppx_wiretype` derives a recursive group that mixes a type with
  parameters and one without, a parameter beside a type its argument was
  named after, and a type where the caller has a `|>` of their own: each
  derived code that did not compile, or used the wrong description.
- Breaking: `ppx_wiretype` refuses an attribute spelt `[@wiretype.x]` for
  no attribute it has, one of its attributes where it does not read it, and
  `[@@tag]` on a type that is no union, each of which it ignored; and
  `[@above]` or `[@below]` on an `int` with what to write instead. Its
  refusals say what to write, and two constructors written alike are
  refused at the second. `[@above]` and `[@below]` are in its `.mli`.
- A union's object is read in time linear in its members; each was said
  again with every problem found before it, so 40,000 members took 23
  seconds.
- `uri` refuses an `xn--` label longer than DNS's 63 octets before
  decoding it, since decoding one takes time growing with the square of
  its length.
- `decode`'s `max_depth` is taken to 10,000 at most, past which the reader's
  own recursion could overflow the stack; and `name` names a description of
  itself, where it overflowed the stack.
- `ipv6` refuses a dotted IPv4 address anywhere but at the end, as in
  `1.2.3.4::`, and `uri` refuses a `file:` URI with userinfo, a port or a
  host that is no domain or address, and an empty host after userinfo or
  before a port: each a string the URL standard, and so zod, refuses.

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
