# AGENTS.md

The wiretype library describes a JSON shape once: it reads a document with
every problem found, writes a value, and prints itself as JSON Schema and Zod.
`ppx_wiretype` derives a description from a type. This file is for anyone
changing it, human or agent. Users start at [README.md](README.md).

## Commands

```sh
make setup   # once: local opam switch in ./_opam, all dependencies, and Zod
make test    # every suite, and the kinds' rows through Zod
make lint    # formatting, odoc, and the release build
make fmt     # format in place
```

The Zod rows need Node 24 or later; `make test-zod` runs them alone. CI
runs `make lint` and `make test` on OCaml 5.5.

## Layout

```
src/                 wiretype: depends on Ptime, Mtime and Uuidm alone
  shape              the GADT a description is, public: the library's promise
  wiretype           the combinators, the kinds, decode and encode
  decode, encode     one pass over text with a description in hand; the writer
  bounds             the bounds a description states, held alike by both
  grammar            the kinds' grammars, hand-written
  itself             a description of itself, built by the first domain to
                     reach it while another waits
  schema             one walk into a schema, printed as JSON Schema and Zod
  problem,           what a reader is told, what a writer is told, any JSON,
  unwritable,        and the two spellings
  value, text
  gen/               build-time programs: the powers of ten the double printer
                     reads, and the IDNA tables from unicode/
  unicode/           Unicode 15.0.0's own data files
ppx/                 ppx_wiretype: [@@deriving wiretype]
test/
  test_wiretype      reading, writing, the kinds and derived descriptions;
                     JSONTestSuite and IdnaTestV2 run whole
  test_schema        what JSON Schema and Zod each print for a description
  test_ppx_wiretype  what the deriver refuses, and the words it refuses in
  zod/               the kinds' rows as Zod reads them, on Node's own runner
  json-test-suite/   JSONTestSuite's cases, pinned
  idna-test/         Unicode's IDNA conformance file
```

Each module's contract is its `.mli`. Read the `.mli` before changing a
module.

## Rules that must hold

Each rule comes with why it exists and the test or search that catches a
break.

- **The `wiretype` package depends on Ptime, Mtime and Uuidm alone.**
  `src/dune` names `ptime`, `mtime` and `uuidm` and no other library, and
  the package depends on OCaml, Ptime, Mtime and Uuidm alone, which depend
  on nothing themselves. Why: a contract library is reached for by
  programs on any stack, and one that brings a framework or a parser with
  it is one they do not take; but an instant is a `Ptime.t` and a duration
  an `Mtime.Span.t` wherever a program keeps time, and a UUID a `Uuidm.t`
  wherever it keeps an identifier -- rowtype's column and postgres-eio's
  parameter are one too, so one identifier crosses the ecosystem as one
  type -- and a kind over any other type is a conversion its user writes
  at every call. Symptom: `rg -n libraries src/dune` names a library but
  `ptime`, `mtime` and `uuidm`, or `wiretype.opam` depends on a package
  but those, `ocaml` and `dune` outside a test's or a developer's filter.
- **A member given twice is refused, anywhere in a document.** The wiretype
  library reads no document a second reader could read another way, as I-JSON
  (RFC 7493 §2.3) has it: which of two members of one name wins is not RFC
  8259's to say. Why: a body a proxy and a server read as two different
  requests. Test: the parser's rows in `test_wiretype`, the two JSONTestSuite
  `y_` files with a repeated member among them.
- **A kind accepts no string a check it prints refuses.** Each ready-made
  kind reads a subset of what the check its schema prints reads -- Zod's,
  today -- so a browser never turns away what the server would take. Why:
  a form that will not send what the API would accept. Test: the same rows
  in `test_wiretype`'s `kinds` and in `test/zod/kinds.test.ts`; a row
  changed in one is changed in the other.
- **A bound is wiretype's to decide, and a printer says it exactly.**
  `multiple_of` on a number is a whole quotient to within a few rounding
  errors, so `19.99` is a multiple of `0.01`; on an integer it is exact.
  Zod's `multipleOf` decides the first alike and the second loosely, so an
  integer's is printed as an exact `z.refine`; what a target cannot say
  exactly is reported as loose. Why: Zod is one target of many a
  description is printed to, and a form must not send what the API then
  refuses. Test: `multiple_rows` and `int_multiple_rows` in
  `test_wiretype`, and `multiples` and `intMultiples` in
  `test/zod/kinds.test.ts`; a row changed in one is changed in the other.
- **JSON Schema and Zod are printed from one walk.** `Schema.walk` makes
  one intermediate schema and both printers read it, and what cannot be
  said exactly -- any JSON -- is reported as loose, never guessed at. Why:
  two walks are two contracts that drift. Test: `test_schema`.
- **The deriver writes only the public combinators.** A derived description
  is one a person could have written by hand, and the deriver refuses a
  shape it cannot describe where it is written, saying what to write
  instead. Why: a derived description that reaches into the library's
  inside breaks with any refactoring of it. Test: `test_wiretype`'s derived
  descriptions are compiled outside the library; `test_ppx_wiretype`'s
  refusals.
- **`invalid_arg` only where a description is built and can mean
  nothing** -- a bound no value meets, alone or with another, a negative
  length or count, a `multiple_of` that is not positive, two values with one
  word, a member described twice, two unions in one object, a case that is
  not an object, a member written by leaving it out that is read as
  required, a read-only member with no `absent` in an object that is read,
  a `nullable` or an `opt_mem` over what writes null for a value of its own
  -- since a description is a constant written in source, and
  the mistake is found when the program starts, never on a request. Each
  function's `.mli` says when it raises. The deriver refuses these at compile
  time, where it sees them in the type: a name given twice, two
  constructors written alike, an option of an option or of `unit` wherever
  it is, a least and a most written as literals with no value between
  them, a read-only field with no default that is no option, a case whose
  one argument is a scalar, a list, an option or a tuple. The rest it
  leaves to the library, when the program starts: a bound that means
  nothing alone, though written as a literal
  (`[@multiple_of 0]`, a negative `[@min_length]`, `[@min nan]`), and
  what it cannot see -- a case whose one argument is a type named
  elsewhere that is no object, an option of a type named elsewhere that
  writes null (`Value.t`, an alias of an option). A description of itself
  still being made cannot be looked into, so `encode` refuses the `Some`
  it would write as null.
  Nothing else raises across the library's boundary. Test:
  `test_wiretype`'s malformed descriptions; `test_ppx_wiretype`'s
  refusals. Symptom: `rg -n -w invalid_arg src ppx` finds it outside
  `src/wiretype.ml`. A stdlib call that raises (`Char.chr`, `String.sub`)
  is not found by a search, and is checked by reading.
- **The opens are `Tuple`'s, inside `Tuple.( ... )` as its `.mli` says, and
  the deriver's `Ppxlib` and `Ast_pattern`**, which every ppxlib rewriter
  opens to read the AST it matches. Symptom:
  `rg -n '\bopen |\b[A-Z]\w*\.[(\[{]' src ppx` finds any other.

## House style

The house style, the OCaml checklist and the OCaml tools are
[OCAML.md](OCAML.md), the same in every Hypha OCaml repository and changed
in all of them together, never in one. It is imported here, so it is read
with this file:

@OCAML.md

## Git

How work becomes a commit -- its format, where it happens, what is pushed
-- is [GIT.md](GIT.md), the same in every Hypha repository and changed in
all of them together, never in one. It is imported here, so it is read
with this file:

@GIT.md

## Changes

- A user-visible change adds a line under `## Unreleased` in
  [CHANGES.md](CHANGES.md), in the same commit. A breaking one says so.
- A change that makes a sentence in a document false edits that sentence in
  the same commit.
- A user-visible change updates the `.mli` it touches; a new supported
  feature or a removed limitation updates the README.

## Releases

[Semantic Versioning 2.0.0](https://semver.org). Before 1.0, a breaking
change bumps the minor version and anything else the patch; from 1.0, a
breaking change waits for the next major version, an addition bumps the
minor and a fix the patch. The two packages are released together, at one
version.

Breaking means code that follows these rules may stop compiling or behave
differently: removing or renaming anything in an `.mli`, changing a type,
adding a required argument, or changing a default or documented behaviour.
Adding a function, a module, an optional argument or a constructor to a
public variant is not breaking: a variant grows in a minor version, so a
match on one that must compile across versions ends in a catch-all arm. Nor
is the wording of a problem's sentence. A problem's code, an attribute the
deriver reads, the name it writes and what a schema prints for a
description are behaviour: changing any is breaking. A constructor added to
`Shape.t` is breaking, since walking it is the library's promise.

The version lives only in the git tag (`0.1.0`, no `v`). A release renames
`## Unreleased` in CHANGES.md to the version and date, and tags it. The GitHub
release notes are that entry with each paragraph and bullet on one line,
since GitHub keeps every line break in release notes. A release is
pushed first and tagged only once CI is green on the commit the tag
names: a broken build is never tagged.
