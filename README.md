# wiretype

[![ci](https://img.shields.io/github/actions/workflow/status/hyphatech/wiretype/ci.yml?branch=main&label=ci)](https://github.com/hyphatech/wiretype/actions/workflows/ci.yml)
[![release](https://img.shields.io/github/v/release/hyphatech/wiretype?label=release)](https://github.com/hyphatech/wiretype/releases)
[![license](https://img.shields.io/github/license/hyphatech/wiretype)](LICENSE)
![OCaml 5.5+](https://img.shields.io/badge/OCaml-5.5%2B-EC6813?logo=ocaml&logoColor=white)
[![Hypha Stack](https://img.shields.io/badge/Hypha-Stack-d4a820?labelColor=15120e)](https://hyphatech.io/stack/)

JSON contracts for modern OCaml.

Strict, light and derived from your types. Built for JSON Schema, Zod and Melange.

**Documentation**: [hyphatech.github.io/wiretype](https://hyphatech.github.io/wiretype/)

## Features

- **Derived from your types**: your records, variants and recursive types are the contract, their bounds and docs included.
- **Every problem at once**: invalid input is refused with every mistake, each at its path with a code a client can act on.
- **JSON Schema, Zod and Melange**: one walk prints all three, and the Zod and the Melange module are held to the server's verdict on every test run, so a form finds what is wrong before it sends.

## Install

```sh
opam repo add hypha https://github.com/hyphatech/opam-repository.git
opam install wiretype ppx_wiretype
```

Add to your `dune`:

```lisp
(libraries wiretype)
(preprocess (pps ppx_wiretype))
```

## Example

```ocaml
type line = {
  sku : string;
  quantity : int; [@min 1] [@max 99]  (** how many *)
  note : string option; [@max_length 200]
}
[@@deriving wiretype]
(** A line of an order. *)
```

Reading a document that gets two members wrong:

```ocaml
Wiretype.decode line_json {|{"sku": "A1", "quantity": 120, "note": 7}|}
```

is `Error` with both problems, each printed by `Problem.to_string` as its
path, a sentence and its code:

```text
quantity: This must be at most 99. (too_large)
note: This must be text, not a number. (unexpected_type)
```

The [quick start](https://hyphatech.github.io/wiretype/quick-start/) runs it,
and prints the same description as JSON Schema and Zod.

## Contributing

See [AGENTS.md](AGENTS.md).

## Licence

MIT, copyright Hypha Technologies Ltd. See [LICENSE](LICENSE).
