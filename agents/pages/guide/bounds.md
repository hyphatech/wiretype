# Bounds

A bound is part of a description. It is checked while reading, refused on
writing, and stated in each schema the description prints -- ranges,
`multiple_of`, lengths and counts alike.

## The bounded descriptions

```ocaml title="bounded.ml"
module J = Wiretype

let quantity = J.int_bounded ~min:1 ~max:99 ()
let even = J.int_bounded ~multiple_of:2 ()
let price = J.number_bounded ~above:0. ~multiple_of:0.01 ()
let ratio = J.number_bounded ~min:0. ~below:1. ()
let name = J.string_bounded ~min_length:1 ~max_length:4 ()
let tags = J.list ~max_items:2 J.string
let scores = J.dict ~min_properties:1 ~key:J.string ~value:J.int ()
```

| Description | Bounds |
|---|---|
| `int_bounded`, `int64_bounded` | `~min`, `~max`, `~multiple_of` |
| `number_bounded` | `~min` and `~max`, at least and at most; `~above` and `~below`, greater and less than; `~multiple_of` |
| `string_bounded` | `~min_length`, `~max_length` |
| `list` | `~min_items`, `~max_items` |
| `dict` | `~min_properties`, `~max_properties` |

Derived, each is an attribute of the same name on the field:
`[@min 1]`, `[@above 0.]`, `[@max_length 200]`, `[@max_items 10]`, and
`[@min_properties]` on a `[@dict]`.

## What they read

```ocaml title="bounds.ml"
module J = Wiretype
module B = Bounded

let read name description text =
  match J.decode description text with
  | Ok _ -> Printf.printf "%-8s %s: read\n" name text
  | Error problems ->
      Printf.printf "%-8s %s: %s\n" name text
        (J.Problem.list_to_string problems)

let () =
  read "quantity" B.quantity "0";
  read "quantity" B.quantity "1e1";
  read "even" B.even "7";
  read "price" B.price "19.99";
  read "price" B.price "19.999";
  read "ratio" B.ratio "1";
  read "name" B.name {|"café"|};
  read "name" B.name {|"cafés"|};
  read "tags" B.tags {|["a", "b", "c"]|};
  read "scores" B.scores "{}"
```

```console
quantity 0: the document: This must be at least 1. (too_small)
quantity 1e1: read
even     7: the document: This must be a multiple of 2. (not_a_multiple)
price    19.99: read
price    19.999: the document: This must be a multiple of 0.01. (not_a_multiple)
ratio    1: the document: This must be less than 1. (too_large)
name     "café": read
name     "cafés": the document: This must be at most 4 characters long. (too_long)
tags     ["a", "b", "c"]: the document: This must have at most 2 items. (too_many)
scores   {}: the document: This must have at least 1 member. (too_few)
```

- A length is in code points, as JSON Schema counts one, so `café` is
  four long.
- `multiple_of` on an integer is exact. On a number it holds where the
  quotient is a whole number to within a few rounding errors, so `19.99`
  is a multiple of `0.01`, as the decimals written mean.
- An infinite bound every number meets -- `~max:infinity` -- is none.

## Said exactly in every schema

A bound is wiretype's to decide, and a printer says it exactly. Zod's
`multipleOf` decides a number's alike, and an integer's loosely, so an
integer's is printed as an exact `z.refine`. What a schema can say only
loosely is printed loosely and listed: an integer's bound past 2^53, which
a double holds only roughly, is left out and reported in
[`Schema.loose`](json-schema.md#loose-and-wrong).

## A bound no value meets

A bound no value meets, alone or with another -- a least above a most, a
negative length or count, a `multiple_of` that is not positive -- raises
`Invalid_argument` where the description is built, so the mistake is found
when the program starts:

```ocaml title="no_value.ml"
module J = Wiretype

let () =
  match J.int_bounded ~min:10 ~max:1 () with
  | (_ : int J.t) -> ()
  | exception Invalid_argument message -> print_endline message
```

```console
Wiretype.int_bounded: min is 10 and max 1, which no integer meets
```

The deriver refuses a least and a most written as literals with no value
between them at compile time, and leaves a bound that means nothing alone
-- `[@multiple_of 0]` -- to the library.
