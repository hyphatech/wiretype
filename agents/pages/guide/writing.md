# Writing

`Wiretype.encode` writes a value as JSON text, minified, or says the first
place that could not be written, and why:

```ocaml
val encode : 'a t -> 'a -> (string, Unwritable.t) result
```

## What it writes

```ocaml title="account.ml"
type account = {
  id : int; [@read_only] [@default 0]
  email : string; [@max_length 254]
  password : string; [@write_only] [@default ""]
  balance : float;
}
[@@deriving wiretype]
```

```ocaml title="write.ml"
module J = Wiretype

let () =
  match
    J.encode Account.account_json
      {
        id = 7;
        email = "ada@example.com";
        password = "hunter2";
        balance = 0.1 +. 0.2;
      }
  with
  | Ok text -> print_endline text
  | Error u -> print_endline (J.Unwritable.to_string u)
```

```console
{"id":7,"email":"ada@example.com","balance":0.30000000000000004}
```

A double is written in the fewest digits that read back as itself, and of
those the nearest to it -- `0.1`, `0.30000000000000004`, `5e-324` -- and an
integer below 2^53 as one. `None` in an `opt_mem` is written by leaving the
member out.

## Read-only and write-only

A member's access says which of a request and an answer it is in, as
OpenAPI's `readOnly` and `writeOnly` say:

- A **read-only** member -- an `id` -- is the server's to give. It is
  never read: a request that sends one back has it skipped, and it reads as
  its default. It is in no request's schema.
- A **write-only** member -- a password -- is the client's. It is never
  written, and is in no answer's schema.

By hand, `Object.mem ~access:Read_only` or `~access:Write_only`; derived,
`[@read_only]` or `[@write_only]`. Above, `password` is not written.

## What it reads, it writes

What `encode` writes, `decode` reads. A value outside a bound its
description states, text that is not UTF-8, a member written twice or
nesting past 512 is refused on writing, at its path with a code, as a
kind's value it cannot spell is:

```ocaml title="refused.ml"
module J = Wiretype

let print = function
  | Ok text -> print_endline text
  | Error u -> print_endline (J.Unwritable.to_string u)

let () =
  print
    (J.encode Account.account_json
       { id = 7; email = String.make 300 'a'; password = ""; balance = 0. });
  print (J.encode J.number Float.nan);
  print (J.encode J.string "\xff");
  print (J.encode (J.map ~parse:String.length J.string) 3)
```

```console
email: This must be at most 254 characters long. (out_of_bounds)
the value: nan is no JSON number. (unspellable)
the value: This text is not UTF-8. (not_utf8)
the value: This value is read and never written. (read_only)
```

An `Unwritable.t` is `{ at; code; message }`, its path printed as a
problem's is. Writing stops at the first, since a document written in part
is no document.

| Code | What could not be written |
|---|---|
| `read_only` | a description made only to read: a member or a union with no `enc`, a `map` or a `kind` with no `print`, an `any` with no `enc` |
| `unspellable` | a value its kind cannot write: a float that is not finite, a value an enum has no word for, a date past the year 9999 |
| `not_utf8` | text that is not UTF-8, which no JSON reader takes |
| `repeated_member` | a member, or a map's name, written twice |
| `name_not_text` | a map's name its key's description writes as no text |
| `too_deep` | nested past what `decode` reads by default |
| `out_of_bounds` | a value outside a bound its description states, in the words `decode` says it in |

## As a value

`Wiretype.to_value` gives the JSON `encode` would write, as a `Value.t`:

```ocaml title="to_value.ml"
module J = Wiretype

let () =
  match J.to_value (J.tuple2 J.int J.string) (1, "one") with
  | Ok value -> print_endline (J.Value.to_string value)
  | Error u -> print_endline (J.Unwritable.to_string u)
```

```console
[1,"one"]
```

`Value.write` and `Value.to_string` write a `Value.t` in the same
spellings, but check none of what `encode` checks: a number that is not
finite is written `null`, and text that is not UTF-8, a member given twice
and nesting of any depth are written as they are. `encode` with
`Value.json` writes only what `decode` reads. `Wiretype.Text` holds JSON's
two spellings that are not obvious, a number and a quoted string, for a
caller writing JSON by hand -- a log line -- so the two spell alike.

## Options of what writes `null`

An option of what already writes `null` -- an option of an option, of
`J.null`, of any JSON -- is refused where it is built, since `None` and
`Some` would be written alike:

```ocaml title="nullable_twice.ml"
module J = Wiretype

let () =
  match J.nullable (J.nullable J.int) with
  | (_ : int option option J.t) -> ()
  | exception Invalid_argument message -> print_endline message
```

```console
Wiretype.nullable: integer or null writes null for a value of its own, so None and Some of it would both be written as null; tell them apart with a variant
```

`Object.mem ~absent` over that description reads a missing member as one
value and `null` as its own: `~absent:None` over a `nullable`,
`~absent:Value.Null` over `Value.json`.
