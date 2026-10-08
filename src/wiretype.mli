(** JSON as descriptions: one value describes a shape, reads it, writes it and
    says what it is.

    {[
    module J = Wiretype

    type line = { sku : string; quantity : int; note : string option }

    let line =
      J.Object.map ~kind:"order line" (fun sku quantity note ->
          { sku; quantity; note })
      |> J.Object.mem "sku" J.string ~enc:(fun l -> l.sku)
      |> J.Object.mem "quantity" (J.int_bounded ~min:1 ~max:99 ())
           ~enc:(fun l -> l.quantity)
      |> J.Object.opt_mem "note" (J.string_bounded ~max_length:200 ())
           ~enc:(fun l -> l.note)
      |> J.Object.finish

    (* Error: quantity is too_large, note is unexpected_type -- every problem *)
    let read = J.decode line {|{"sku": "A1", "quantity": 120, "note": 7}|}
    ]}

    {b Reading} is one pass over the text with the description in hand, and
    reports every problem the document has, each at its place with a code
    ({!Problem}) -- but for text that is not JSON, or nested past the limit,
    which is that one problem alone. A number is read from its digits by the
    description that wants it, so an integer is exact; a member given twice is
    refused, as I-JSON (RFC 7493 §2.3) has it, because two readers that choose
    differently read two documents.

    {b Writing} is minified, with each double in the fewest digits that read
    back as itself.

    {b What a description is} is {!Shape}, public and walked by the decoder, the
    encoder and {!Schema}, which prints JSON Schema and zod from it.

    {b A description that can mean nothing} -- such as a bound that is no finite
    number, a [multiple_of] that is not positive, two values written as one
    word, a member described twice, an object with two unions, or a member
    written by leaving it out that is read as required -- raises
    [Invalid_argument] where it is built, as each function below says: a
    description is a constant written in source, so the mistake is found when
    the program starts, or in its first test, and never on a request. *)

(** Any JSON value, and its description. *)
module Value : sig
  include module type of struct
    include Value
  end

  val json : t Shape.t
  (** Any JSON. It says nothing of its shape, and a schema says so. Its name is
      the one [[@@deriving wiretype]] writes for any [M.t], so a field typed
      [Wiretype.Value.t], or by any alias of it, is described by it. *)
end

module Text = Text
module Problem = Problem
module Unwritable = Unwritable
module Shape = Shape
module Schema = Schema

type 'a t = 'a Shape.t

(** {1 Reading and writing} *)

val decode : ?max_depth:int -> 'a t -> string -> ('a, Problem.t list) result
(** [max_depth] is how deep the document may nest, 512 unless given and never
    more than 10,000. *)

val encode : 'a t -> 'a -> (string, Unwritable.t) result
(** [Error] is the first place that could not be written, and why: a description
    made only to read, a value its kind cannot spell -- a float that is not
    finite, a value an enum has no word for, a case its union does not have --
    or what would make a document {!decode} refuses: text that is not UTF-8, a
    member or a map's name written twice, nesting past 512. What [encode]
    writes, [decode] reads. *)

val to_value : 'a t -> 'a -> (Value.t, Unwritable.t) result
(** The value as the JSON {!encode} writes for it. *)

val of_value : 'a t -> Value.t -> ('a, Problem.t list) result

val name : 'a t -> string
(** What the description is called where it is spoken of: its kind, or its sort.
*)

(** {1 Scalars} *)

val null : 'a -> 'a t
(** JSON [null], read as the value; any value is written [null]. *)

val bool : bool t

val int : int t
(** A whole number, read from its digits. A number written with a fraction that
    is whole -- [2.0], [1e3] -- is one, as JSON Schema's [integer] is, up to
    2{^ 53}, past which a fraction's digits are not exact. A JavaScript client
    holds an integer exactly only up to 2{^ 53}, and zod's [z.int()] refuses one
    past it: an id that may be larger travels as text, through {!kind}. *)

val int_bounded : ?min:int -> ?max:int -> ?multiple_of:int -> unit -> int t
(** Checked while reading, and stated in the schema; [multiple_of] exactly.
    Raises [Invalid_argument] where [multiple_of] is not positive. *)

val int64 : int64 t
(** As {!int}, to 2{^ 63}: past 2{^ 53}, a JavaScript client can neither hold it
    exactly nor pass zod's check. *)

val int64_bounded :
  ?min:int64 -> ?max:int64 -> ?multiple_of:int64 -> unit -> int64 t
(** Raises [Invalid_argument] where [multiple_of] is not positive. *)

val number : float t
(** A JSON number, as a double; one that overflows a double is too large. A
    non-finite float is no JSON number, and is not written. *)

val number_bounded :
  ?min:float ->
  ?max:float ->
  ?above:float ->
  ?below:float ->
  ?multiple_of:float ->
  unit ->
  float t
(** [min] and [max] are at least and at most; [above] and [below] greater and
    less than. [multiple_of] holds where the quotient is a whole number to
    within a few rounding errors, so [19.99] is a multiple of [0.01], as the
    decimals written mean. Raises [Invalid_argument] where a bound is not a
    finite number, or [multiple_of] is not a positive one. *)

val string : string t
(** Text, checked as UTF-8. *)

val string_bounded : ?min_length:int -> ?max_length:int -> unit -> string t
(** A length in code points, as JSON Schema counts one. *)

val enum : ?kind:string -> ?doc:string -> ('a -> string) -> 'a list -> 'a t
(** [enum word values]: each value written as its word, and read from it. Its
    words are in the schema, exactly. Raises [Invalid_argument] where two values
    have one word. *)

(** {1 Ready-made kinds}

    Each over a plain OCaml type, so no library's type is in an application's
    API; each with its schema's [format] and its zod check, and accepting no
    string that check refuses. *)

val instant : int t
(** Epoch milliseconds, as RFC 3339 [date-time]: [2026-09-30T12:00:00.000Z],
    read with any fraction and offset -- a fraction past the millisecond is
    dropped -- and written in UTC with milliseconds, so one instant has one
    spelling. A leap second is refused, and so is an instant that is outside the
    years 0000 to 9999 once in UTC -- [9999-12-31T23:59:59-01:00] -- since it
    could not be written back. *)

val date : (int * int * int) t
(** [(year, month, day)], as RFC 3339 [full-date]: [2026-09-30]. *)

val duration : int t
(** Milliseconds, as ISO 8601: [PT1H30M]. Weeks, or days, hours, minutes and
    seconds; never years or months, which are no number of milliseconds. A day
    is 24 hours, and one past a hundred thousand years is refused. *)

val uuid : ?version:Shape.uuid_version -> unit -> string t
(** RFC 9562, lower-cased: versions 1 to 8, with the nil and the max UUID, or
    the one version asked for. The max UUID is read in lower case alone, as zod
    reads it. *)

val base64 : string t
(** Bytes, as RFC 4648 §4's padded base64. *)

val base64url : string t
(** Bytes, as RFC 4648 §5's unpadded base64url. *)

val uri : string t
(** RFC 3986, less what the URL standard refuses: a port past 65535, an
    [IPvFuture] host, an empty host after userinfo or before a port, an [http],
    [https], [ws], [wss] or [ftp] URI with no host or a host that is not a
    domain or an address, and a [file] URI with userinfo, a port, or a host that
    is not empty, a domain or an address. An [xn--] label is decoded as Punycode
    and held to UTS #46 as the URL standard applies it, with Unicode 15.0.0's
    tables, the bidi rule of RFC 5893 across the host included. It is refused,
    where a browser might take it, when it holds a joiner or a code point that
    may not be in NFC, since both need a context or a normalisation the tables
    alone cannot check, when it is longer than the 63 octets DNS allows a label,
    or when it decodes to nothing but ASCII or to another [xn--]. *)

val ipv4 : string t
(** Dotted decimal: four numbers to 255, none with a leading zero. *)

val ipv6 : string t
(** RFC 4291 §2.2's full and compressed forms, with a dotted IPv4 address in the
    last thirty-two bits, kept as written: [FE80::1] stays in upper case. A zone
    ([%eth0]) is no part of an address, and is refused. *)

(** {1 Descriptions of descriptions} *)

val list : ?min_items:int -> ?max_items:int -> 'a t -> 'a list t

val dict :
  ?min_properties:int -> ?max_properties:int -> 'k t -> 'v t -> ('k * 'v) list t
(** A JSON object as a map: every member, in the document's order, its name read
    by the first description as the JSON string it is written as -- {!string},
    an {!enum}, a kind over text such as {!uuid} -- and its value by the second.
    A name that is wrong is a problem at the name ({!Problem.Name}). A name
    given twice is refused both ways: in a document, and in a list written with
    one twice. A key that is not written as text is refused on writing, and is
    an error of the schema. *)

val tuple2 : 'a t -> 'b t -> ('a * 'b) t
(** A JSON array of exactly two items, each read by its own description: a list
    of another length is [too_few] or [too_many], and a wrong item a problem at
    its index. *)

val tuple3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
val tuple4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t

(** A tuple of any length, built as an object is: a function, and each item it
    takes, in order.

    {[
    J.Tuple.(
      map (fun x y z -> { x; y; z })
      |> item J.number ~enc:(fun p -> p.x)
      |> item J.number ~enc:(fun p -> p.y)
      |> item J.number ~enc:(fun p -> p.z)
      |> finish)
    ]} *)
module Tuple : sig
  type ('t, 'dec) map

  val map : 'dec -> ('t, 'dec) map
  val item : 'a t -> enc:('t -> 'a) -> ('t, 'a -> 'b) map -> ('t, 'b) map
  val finish : ('t, 't) map -> 't t
end

val nullable : 'a t -> 'a option t
(** The description, or [null] for [None]. *)

val rec' : 'a t Lazy.t -> 'a t
(** A description of itself. *)

val map :
  ?kind:string ->
  ?doc:string ->
  ?dec:('a -> 'b) ->
  ?enc:('b -> 'a) ->
  'a t ->
  'b t
(** Another type over [t]. A direction left out is one the description is never
    used in: reading it, or writing it, is a problem. *)

val kind :
  name:string ->
  ?doc:string ->
  ?format:Shape.format ->
  parse:('a -> ('b, string) result) ->
  ?print:('b -> 'a) ->
  'a t ->
  'b t
(** A value with a grammar of its own -- an id, a coordinate -- over [t]:
    [parse]'s [Error] is a sentence for a person, a {!Problem.Malformed} at the
    value's place. Without [print] it is only read. *)

val any :
  ?kind:string ->
  ?doc:string ->
  ?dec_null:'a t ->
  ?dec_bool:'a t ->
  ?dec_number:'a t ->
  ?dec_string:'a t ->
  ?dec_array:'a t ->
  ?dec_object:'a t ->
  ?enc:('a -> 'a t) ->
  unit ->
  'a t
(** A value that is one of several JSON sorts, read by the description of the
    sort it is, and written by the one [enc] picks. *)

(** {1 Objects} *)

module Object : sig
  type ('o, 'dec) map
  (** An object being described: [map f] and each member [f] takes, in order. *)

  val map : ?kind:string -> ?doc:string -> 'dec -> ('o, 'dec) map
  (** [kind] names it, and makes it a component of a schema. *)

  val enc_only : ?kind:string -> ?doc:string -> unit -> ('o, 'dec) map
  (** An object that is written and never read -- a request to another server --
      whose members need no function to build a value from: reading one is a
      problem. *)

  val mem :
    ?doc:string ->
    ?absent:'a ->
    ?omit:('a -> bool) ->
    ?enc:('o -> 'a) ->
    ?access:Shape.access ->
    ?deprecated:bool ->
    ?examples:'a list ->
    string ->
    'a t ->
    ('o, 'a -> 'b) map ->
    ('o, 'b) map
  (** A member. Without [absent] it is required; with it, it may be left out and
      reads as [absent]. [omit] says which values are written by leaving it out,
      and needs [absent] where the object is read. [enc] is how it is found in a
      value; without it the object only reads. [`Read_only] leaves it out of a
      request's schema and [`Write_only] out of an answer's. *)

  val opt_mem :
    ?doc:string ->
    ?enc:('o -> 'a option) ->
    ?access:Shape.access ->
    ?deprecated:bool ->
    ?examples:'a list ->
    string ->
    'a t ->
    ('o, 'a option -> 'b) map ->
    ('o, 'b) map
  (** A member that may be left out: absent or [null] reads as [None], since
      clients write both for "nothing", and [None] is written by leaving it out.
      [access], [deprecated] and [examples] are {!mem}'s; an example is a value
      the member holds when it is there. *)

  val case_mem :
    ?doc:string ->
    ?absent:'tag ->
    ?omit:('tag -> bool) ->
    ?enc:('o -> 'c) ->
    enc_case:('c -> ('c, 'tag) Shape.case_value) ->
    string ->
    'tag t ->
    ('c, 'tag) Shape.case list ->
    ('o, 'c -> 'b) map ->
    ('o, 'b) map
  (** A union by a tag: the member of that name, read first wherever it is,
      decides which case's members the rest are. [absent] is the tag of a value
      that has none. It is written first. *)

  val error_unknown : ('o, 'dec) map -> ('o, 'dec) map
  (** A member it does not describe is a problem; by default it is skipped. *)

  val finish : ('o, 'o) map -> 'o t
  (** Raises [Invalid_argument] where a member's name is given twice -- among
      the object's members, its union's tag, and each case's members -- where
      two cases' tags are written alike or one cannot be written, where the
      object has two unions -- the tag is read before the members it decides, so
      an object has one -- or where an object that is read has a member, or a
      tag, with [omit] and no [absent], which it would write by leaving out and
      read as required. *)

  (** The cases of a union. *)
  module Case : sig
    type ('c, 'k, 'tag) map = ('c, 'k, 'tag) Shape.case_map

    val map : ?dec:('k -> 'c) -> 'tag -> 'k t -> ('c, 'k, 'tag) map
    (** [map tag object ~dec]: the case [tag] stands for, whose members are
        [object]'s. Without [dec] it is written and never read. Raises
        [Invalid_argument] where [object] is not one, or has a union of its own.
    *)

    val make : ('c, 'k, 'tag) map -> ('c, 'tag) Shape.case
    val value : ('c, 'k, 'tag) map -> 'k -> ('c, 'tag) Shape.case_value
  end
end
