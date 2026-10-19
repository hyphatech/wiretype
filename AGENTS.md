# AGENTS.md

wiretype is an OCaml library of Hypha Stack. It describes a JSON shape once:
it reads a document with every problem found, writes a value, and prints
itself as JSON Schema, as Zod and as OCaml for Melange. `ppx_wiretype`
derives a description from a type. This file is for anyone changing it,
human or agent. Users start at [README.md](README.md).

## Commands

```sh
make setup   # once: the switch hypha-wiretype, all dependencies, and Zod
make test    # every suite, the kinds' rows through Zod, and the shared rows
             # through the printed Melange module
make lint    # formatting, odoc, and the release build
make fmt     # format in place
```

The Zod rows and the Melange rows need Bun 1.3 or later; `make test-zod`
runs the Zod rows alone. Melange is a test dependency, installed with the
rest. CI runs `make lint` and `make test` on OCaml 5.5.

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
  schema             one walk into a schema, printed as JSON Schema, Zod and
                     OCaml for Melange
  problem,           what a reader is told, what a writer is told, any JSON,
  unwritable,        and the two spellings
  value, text
  melange/           what every printed Melange module opens with -- its
                     Problem, the kinds' types, the readers and writers --
                     kept as the OCaml it is and carried into schema as text
  gen/               build-time programs: the powers of ten the double printer
                     reads, and the IDNA tables from unicode/
  unicode/           Unicode 15.0.0's own data files
ppx/                 ppx_wiretype: [@@deriving wiretype]
test/
  test_wiretype      reading, writing, the kinds and derived descriptions;
                     JSONTestSuite and IdnaTestV2 run whole
  test_schema        what JSON Schema and Zod each print for a description
  test_shapes        the shapes' rows on the server
  test_ppx_wiretype  what the deriver refuses, and the words it refuses in
  print_shapes       the Melange module printed for the fixtures' shapes
  zod/               the kinds' rows as Zod reads them, on Bun's runner
  fixtures/          the kinds' rows and the shapes' rows, one file each, read
                     by the OCaml suites, the Zod rows and the Melange rows;
                     and the shapes the rows are read by
  melange/           the shapes' rows in the browser's runtime: the printed
                     module, committed and compared with what the printer
                     writes, compiled by Melange beside the runner, run by Bun
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
  and the printed Melange module's -- so a browser never turns away what
  the server would take. Why: a form that will not send what the API would
  accept. Test: `test/fixtures/kinds.json`, one file of rows read by
  `test_wiretype`'s `kinds`, by `test/zod/kinds.test.ts` and by
  `test/melange/run.ml`, so a row is changed in one place and all three
  read it; the printed module decides as the server does, but for a URI,
  where a row says so.
- **The printed Melange module decides as the server does.** Each row of
  `test/fixtures/shapes.json` reads to the same problems, by place and
  code, through `decode` and through the module printed for the same
  descriptions, and what reads back is written back alike -- but where a
  row says the browser is the looser, which is a whole number past 2^53,
  a member given twice, nesting, a URI, and the place of a syntax problem.
  Why: the browser holds the server's shapes as its own types, and a form
  that disagrees with the API is the gap the printer exists to close. Test:
  `test_shapes` and `test/melange/run.ml`, one file of rows; the printed
  module under `test/melange` is compared with what the printer writes
  today, and `dune promote` takes the new one.
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
- **JSON Schema, Zod and Melange are printed from one walk.** `Schema.walk`
  makes one intermediate schema and the three printers read it, and what
  cannot be said exactly -- any JSON -- is reported as loose, never guessed
  at. Why: two walks are two contracts that drift. Test: `test_schema`, and
  the shapes' rows, which hold the printed Melange module to the server's
  verdict.
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

## Changes and releases

Its two packages are released together. `Shape.t` is closed: a
constructor added to it is breaking, since walking it is the library's
promise. A problem's sentence is wording; a problem's code, an attribute
the deriver reads, the name it writes and what a schema prints for a
description, in any of its three targets, are behaviour: changing any is
breaking.
