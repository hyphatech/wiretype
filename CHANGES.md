# Changes

## Unreleased

- A description of itself is built by the domain that first reaches it,
  and another reaching it then waits for it, where it was told
  `Lazy.Undefined` and `decode` and `encode` raised it. A recursive type
  with parameters, `'a tree`, is built a level at a time as it is first
  read, so four domains reading one at once raised it at every level
  none had reached yet; the 0.4.0 fix built only those with none. A
  hand-written `rec'` need no longer be forced before it is shared.

## 0.4.0 (2026-10-13)

- A union inside a union's member is read in time that grows with the
  document alone, where each union went through everything inside it
  again to find its tag: a recursive variant 500 deep around a megabyte
  of text took a second to read, and takes what one level does.
- The deriver builds a recursive type's description as its module loads,
  where it was built on first use, and two domains reading it first at
  once raised `Lazy.Undefined`. `rec'` says to force a hand-written one
  before sharing it between domains.
- A map whose key is read in more than one spelling -- a `uuid` in
  either case, an `instant` at any offset -- refuses a name that spells a
  key already given as `repeated_member`, as the map's odoc says, where
  it read both, and `encode` then refused the map it had read.
- Breaking: a component's schema is walked from its own root wherever
  it is reached, so kinds that hold each other in a ring of three or more
  refer to each other, where the third's member back to the first was
  printed as any JSON (`a: z.optional(z.unknown())`, now
  `get a() { return z.optional(ASchema); }`) and reported loose. A
  request's `<Name>Input` is decided the same way, so a ring whose
  difference is more than three kinds away is named `Input` from
  wherever it is walked. A recursive description with no kind is
  expanded to the same depth inside every component, and past a hundred
  kinds each inside the last -- reached only by one that makes a new
  kind at every level -- the walk prints any JSON and reports it.
- Breaking: the deriver gives a type with parameters no kind unless
  `[@@kind]` names it, where it was named by the type whatever its
  arguments, so `type 'a page` used as `string page` and `int page` in
  one document was `kind_shared` and both printed as one `PageSchema`.
  Each use is now written where it is (`users: z.object({ items:
  z.array(z.string()), ... })`), and `Wiretype.name` of one is `object`.
  A recursive type with parameters is expanded as far as any recursion
  with no kind, unless `[@@kind]` names it.

## 0.3.0 (2026-10-11)

- Breaking: `instant` is a `Ptime.t t` and `duration` an `Mtime.Span.t t`,
  where each was an `int t` of milliseconds, an `int` that said neither
  its unit nor that it was a time. What each reads and writes, and the
  JSON Schema and Zod each prints, are as they were. An instant is
  written to the millisecond, a fraction past it dropped toward the past,
  and can no longer fail to be written, since a `Ptime.t` is of the years
  0000 to 9999. A duration is written to the millisecond, and is never
  negative.
- Breaking: a duration longer than an `Mtime.Span.t` holds, 2^64 - 1
  nanoseconds or about 584 years, is refused as `malformed`, where one up
  to a hundred thousand years was read.
- The `wiretype` package depends on `ptime` and `mtime`, where it
  depended on nothing; both depend on nothing themselves.
- The deriver describes a `Ptime.t` as `Wiretype.instant`, and an
  `Mtime.Span.t` or an `Mtime.span` as `Wiretype.duration`, where the
  derived code failed to compile on an unbound `Ptime.json`, and refuses
  a case whose one argument is one, as it refuses a scalar.

- Breaking: `dict` labels its two descriptions and ends in `()`:
  `J.dict ~key:J.string ~value:(J.uuid ()) ()`, where
  `J.dict J.string (J.uuid ())` and `J.dict (J.uuid ()) J.string` both
  compiled as a `(string * string) list t`, the one reading a uuid's
  values and the other its names. The `()` is what lets
  `?min_properties` and `?max_properties` be left out. The deriver
  writes the new call.

- A description that comes back to itself before it reads anything --
  `let rec t = lazy (J.rec' t)`, or through a `map` or an `any` -- is
  refused at its place, by `decode` as `malformed` and by `encode` as
  `unspellable`, where reading or writing went round for ever or ended
  in `Stack_overflow`.
- Breaking: the deriver reads a type written by its stdlib module's name -- `String.t`,
  `'a List.t`, `Stdlib.Int64.t` -- as its short name, bounds and an
  option's member alike, where the derived code failed to compile on an
  unbound `String.json`; `Stdlib.Int32.t` and its kin are refused as
  `int32` is. A module of the user's own named `String` is read as the
  stdlib's: describe its type with `[@with String.json]`.
- Breaking: a value `encode` refuses as `too_deep` is at no more than the
  first 16 segments of its path, its sentence saying how many levels
  below (`[0][0]...: This is nested more than 512 deep, 496 levels below
  here.`), as `decode`'s problem is, where its path went the whole way
  down, 512 segments and more.
- The deriver refuses an `int32`, a `nativeint`, a `char` and `bytes`,
  or `Int32.t`, `Nativeint.t`, `Char.t` and `Bytes.t`, where each is
  written, saying what to write instead, where the derived code failed
  to compile on an unbound `int32_json` or `Int32.json`.
- Breaking: `decode` lists at most a hundred problems, or as many as a new
  `?max_problems` says; past it, reading stops and the list ends with one
  problem of a new code, `too_many_problems` (`Problem.Too_many_problems`,
  which a match on `Problem.code` wants a case for), at the document. A
  body of a megabyte of wrong items was a quarter of a million problems.
- Breaking: a `too_deep` problem is at no more than the first 16 segments
  of its path, its sentence saying how many levels below
  (`[0][0]...: This is nested more than 512 deep, 496 levels below
  here.`), where its path went the whole way down: 1.5 KB at the default
  limit, and 30 KB at 10,000.
- Below 0, `decode ~max_depth` is 0, as one past 10,000 is 10,000: a
  scalar alone is read, and the problem says it is nested more than 0
  deep, where it said more than a negative number.
- Breaking: a path writes a member's name quoted as JSON where it holds a C1
  control (NEL among them), U+2028, U+2029 or a bidirectional control
  (U+061C, U+200E, U+200F, U+202A to U+202E, U+2066 to U+2069), each as
  JSON's `\u` escape (`items["a\u2028b"]`), where it wrote them as they
  were: a problem printed for a log could end its line there, or show its
  text in another order than it has.
- Breaking: the doc of an enum, a kind, a map, a value of several sorts
  (`any`) and a union is in the schema, as an object's and a member's
  were: JSON Schema's `description` on its subschema, and Zod's comment
  on the declaration or member it stands for. A derived enum's or
  union's doc comment is so too. `Schema.t` has a new constructor,
  `Described`, which wraps a shape with its doc; a match on `Schema.t`
  wants a case for it. A member's doc wins over its shape's on its
  property, which held two `description`s where the member's shape was an
  object with a doc and no kind. The ready-made kinds print no doc of
  their own, since their `format` says what they are.
- Breaking: OCaml 5.5 or later is required. The opam files asked for 5.4,
  which CI tested and nothing else did; every switch the code is written
  in is 5.5.
- Breaking: an `int` or `int64` written with a fraction or an exponent is
  whole where its digits are, never where the double it rounds to is:
  `1.0000000000000000001` and `1.5e-400` are fractions, where they were
  read as `1` and `0`. One of 2^53 or more so written is `too_large` or
  `too_small`, since a double cannot say which whole number it is:
  `9007199254740993.0` was read as `9007199254740992`.
- Breaking: `encode` refuses, as `unspellable` at its path, a
  `Value.Number` that is not finite in any JSON, which it wrote `null`
  though its documentation said such a float is not written. `Value.write`
  still writes it `null`, and says so.
- Breaking: `Object.opt_mem` over a `nullable` raises `Invalid_argument`,
  and the deriver refuses an `option option` field, since `Some None` was
  written `null` and read back as `None`. A member that tells a missing
  value from `null` is `mem ~absent` over a `nullable`: `[@default e]` on
  an `option` field.
- Breaking: `nullable` and `Object.opt_mem` raise `Invalid_argument` over
  any description that writes `null` for a value of its own -- a
  `nullable` wherever it is (a list's item, a map's value, a member with
  `~absent`), `null`, `Value.json`, an `any` that reads `null`, or a `map`
  or `kind` over one -- since `None` and `Some` of that value were both
  written `null`, and read back as `None`. A description of itself still
  being made cannot be looked into, so `encode` refuses, as
  `unspellable`, a `Some` it would write as `null`. The deriver refuses an
  option of an option, or of `unit`, wherever it is written, a field with
  `[@default e]` among them. A member that may hold any JSON is `mem
  ~absent:Value.Null` over `Value.json`.

- Breaking: a bound no value meets raises `Invalid_argument` where the
  description is built -- a least past its most in `int_bounded`,
  `int64_bounded`, `number_bounded` (`~above:1. ~max:1.` among them),
  `string_bounded`, `list` and `dict`, or a negative length or count --
  where it was taken and every document then refused. The deriver refuses
  such a pair written as literals (`[@min 5] [@max 3]`).
- Breaking: a derived variant whose one argument is no object -- `A of int`,
  a list, an array, an option, a tuple -- is refused where it is written,
  saying to give it a name in an inline record, where it compiled and
  raised when the program started. `Object.Case.map`, refusing one, says
  what the description is and to describe an object.

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
  no word for; and a case its union does not list. `Object.finish` raises
  where an
  object that is read has a member or a tag with `omit` and no `absent`,
  which it wrote by leaving out and read as required.
- Breaking: the schemas say what the reader means or report that they
  cannot. A value of several sorts that nothing reads is reported loose,
  where it was printed as `null`; an integer's bound past 2^53 is left out and reported loose, where
  it was rounded; a UUID of one version has its `pattern` in JSON Schema; a
  member named `__proto__` is a computed key in zod, where it set the
  prototype.
- Breaking: `number_bounded` raises for a bound no number meets --
  `~max:neg_infinity`, or NaN -- and takes one every number meets,
  `~max:infinity`, as none; each was printed as `null`.
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
- Breaking: `decode`'s `max_depth` is taken to 10,000 at most, past which the reader's
  own recursion could overflow the stack; and `name` names a description of
  itself, where it overflowed the stack.
- Breaking: `ipv6` refuses a dotted IPv4 address anywhere but at the end, as in
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
