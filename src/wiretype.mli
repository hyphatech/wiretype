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
    which is that one problem alone, and past a hundred problems, where reading
    stops and says there were more. A number is read from its digits by the
    description that wants it, so an integer is exact; a member given twice is
    refused, as I-JSON (RFC 7493 §2.3) has it, because two readers that choose
    differently read two documents.

    {b Writing} is minified, with each double in the fewest digits that read
    back as itself.

    {b What a description is} is {!Shape}, public and walked by the decoder, the
    encoder and {!Schema}, which prints JSON Schema and Zod from it.

    {b A description that can mean nothing} -- such as a bound no number meets,
    a [multiple_of] that is not positive, two values written as one word, a
    member described twice, an object with two unions, a member written by
    leaving it out that is read as required, a read-only member with nothing to
    be read as in an object that is read, or an option of a description that
    writes [null] for a value of its own -- raises [Invalid_argument] where it
    is built, as each function below says: a description is a constant written
    in source, so the mistake is found when the program starts, or in its first
    test, and never on a request. *)

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

val decode :
  ?max_depth:int ->
  ?max_problems:int ->
  'a t ->
  string ->
  ('a, Problem.t list) result
(** [max_depth] is how deep the document may nest, 512 unless given and never
    more than 10,000: [[]] is nested one deep, and a limit of 0 reads a scalar
    alone, as any below it does. [max_problems] is how many problems are listed,
    100 unless given: past it, reading stops -- what follows, even text that is
    not JSON, is not read -- and the list ends with a
    {!Problem.Too_many_problems}. A limit below 0 is 0. *)

val encode : 'a t -> 'a -> (string, Unwritable.t) result
(** [Error] is the first place that could not be written, and why: a description
    made only to read, a value its kind cannot spell -- a float that is not
    finite, a value an enum has no word for, a case its union does not have, a
    [Some] that would be written [null] by a description of itself {!nullable}
    could not look into, any value of a description that is only itself -- or
    what would make a document {!decode} refuses: a value outside a bound its
    description states, said in the words [decode] says it in, text that is not
    UTF-8, a member or a map's name written twice, nesting past 512. What
    [encode] writes, [decode] reads. *)

val to_value : 'a t -> 'a -> (Value.t, Unwritable.t) result
(** The value as the JSON {!encode} writes for it. *)

val of_value : 'a t -> Value.t -> ('a, Problem.t list) result
(** The value read as {!decode} reads the JSON it is, with its limits. *)

(** {1 A value as one text}

    What a URL's segment, a query parameter, a header or a form's field carries:
    a scalar as the JSON it is written as, but a string -- and so an enum's
    word, or a kind over text -- as it is, unquoted. It is what a printed client
    sends, and is read and written by the description a body's member is, with
    its bounds, its grammar and its problems.

    {[
    J.of_text
      (J.int_bounded ~min:1 ~max:100 ())
      "20" (* Ok 20 *) J.of_text
      (J.int_bounded ~min:1 ~max:100 ())
      "200" (* too_large *) J.of_text (J.uuid ())
      "3f2504e0-4f89-41d3-9a0c-0305e82c3301"
    ]} *)

val of_text : 'a t -> string -> ('a, Problem.t list) result
(** Text that is a JSON number, [true] or [false] is read as that JSON, and any
    other text as a string, so a number's text has JSON's grammar -- [007] and
    [+3] are no number -- and text a description cannot read is
    {!Problem.Unexpected_type}, said as what it must be:
    ["This must be a whole number."]. A description of no scalar reads no text
    but as a string. *)

val to_text : 'a t -> 'a -> (string, Unwritable.t) result
(** What {!encode} writes, a string's quotes and escapes taken off, so [of_text]
    reads it back; refused where [encode] refuses. *)

val name : 'a t -> string
(** What the description is called where it is spoken of: its kind, or its sort.
*)

(** {1 Scalars} *)

val null : 'a -> 'a t
(** JSON [null], read as the value; any value is written [null]. *)

val bool : bool t

val int : int t
(** A whole number, read from its digits. One written with a fraction or an
    exponent -- [2.0], [1e3] -- is read where its digits are whole, as JSON
    Schema's [integer] is, so [1.0000000000000000001] is a fraction, and only
    below 2{^ 53}, from which a double cannot tell which whole number it is. A
    JavaScript client holds an integer exactly only up to 2{^ 53}, and Zod's
    [z.int()] refuses one past it: an id that may be larger travels as text,
    through {!kind}. *)

val int_bounded : ?min:int -> ?max:int -> ?multiple_of:int -> unit -> int t
(** Checked while reading, and stated in the schema; [multiple_of] exactly.
    Raises [Invalid_argument] where [multiple_of] is not positive, or [min] is
    more than [max]. *)

val int64 : int64 t
(** As {!int}, to 2{^ 63}: past 2{^ 53}, a JavaScript client can neither hold it
    exactly nor pass Zod's check. *)

val int64_bounded :
  ?min:int64 -> ?max:int64 -> ?multiple_of:int64 -> unit -> int64 t
(** Raises [Invalid_argument] where [multiple_of] is not positive, or [min] is
    more than [max]. *)

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
    decimals written mean. An infinite bound every number meets --
    [~max:infinity] -- is none. Raises [Invalid_argument] where a bound is one
    no number meets, or no number, where no number is between a least and a
    most, or where [multiple_of] is not a positive finite number. *)

val string : string t
(** Text, checked as UTF-8. *)

val string_bounded : ?min_length:int -> ?max_length:int -> unit -> string t
(** A length in code points, as JSON Schema counts one. Raises
    [Invalid_argument] where a length is negative, or [min_length] is more than
    [max_length]. *)

val enum : ?kind:string -> ?doc:string -> ('a -> string) -> 'a list -> 'a t
(** [enum word values]: each value written as its word, and read from it. Its
    words are in the schema, exactly. Raises [Invalid_argument] where two values
    have one word. *)

(** {1 Ready-made kinds}

    Each over a plain OCaml type, so no library's type is in an application's
    API, but for time and identity: an instant is a [Ptime.t], a duration an
    [Mtime.Span.t] and a UUID a [Uuidm.t], the types OCaml's libraries hold them
    in. Each with its schema's [format] and its Zod check, and accepting no
    string that check refuses. What each is, its [format] says, so a schema
    prints no [description] of the library's for it: a doc is its user's, a
    member's or one {!map} gives around the kind. *)

val instant : Ptime.t t
(** An instant, as RFC 3339 [date-time]: [2026-09-30T12:00:00.000Z], read with
    any fraction and offset and written in UTC with milliseconds, so one instant
    has one spelling; a fraction past the millisecond is dropped, toward the
    past, in reading and in writing. A leap second is refused, and so is an
    instant that is outside the years 0000 to 9999 once in UTC --
    [9999-12-31T23:59:59-01:00] -- which no [Ptime.t] is. *)

val date : (int * int * int) t
(** A date, [(year, month, day)], as RFC 3339 [full-date]: [2026-09-30]. *)

val duration : Mtime.Span.t t
(** A duration, as ISO 8601: [PT1H30M]. Weeks, or days, hours, minutes and
    seconds, to the millisecond; never years or months, which are no number of
    milliseconds. A fraction past the millisecond is dropped, in reading and in
    writing. A day is 24 hours, and one longer than an [Mtime.Span.t] holds --
    2{^ 64} - 1 nanoseconds, about 584 years -- is refused, as is a negative
    one, which ISO 8601 has no spelling for. *)

val uuid : ?version:Shape.uuid_version -> unit -> Uuidm.t t
(** A UUID, as RFC 9562 writes one: versions 1 to 8, with the nil and the max
    UUID, or the one version asked for, read in either case and written in lower
    case. The max UUID is read in lower case alone, as Zod reads it. Any 128
    bits are a [Uuidm.t], so one of another version, or of a variant RFC 9562
    does not define, is {!Unwritable.Unspellable}. *)

val base64 : string t
(** Bytes, as RFC 4648 §4's padded base64. *)

val base64url : string t
(** Bytes, as RFC 4648 §5's unpadded base64url. *)

val uri : string t
(** A URI, as RFC 3986 writes one, less what the URL standard refuses: a port
    past 65535, an [IPvFuture] host, an empty host after userinfo or before a
    port, an [http], [https], [ws], [wss] or [ftp] URI with no host or a host
    that is not a domain or an address, and a [file] URI with userinfo, a port,
    or a host that is not empty, a domain or an address. An [xn--] label is
    decoded as Punycode and held to UTS #46 as the URL standard applies it, with
    Unicode 15.0.0's tables, the bidi rule of RFC 5893 across the host included.
    It is refused, where a browser might take it, when it holds a joiner or a
    code point that may not be in NFC, since both need a context or a
    normalisation the tables alone cannot check, when it is longer than the 63
    octets DNS allows a label, or when it decodes to nothing but ASCII or to
    another [xn--]. *)

val ipv4 : string t
(** An IPv4 address, in dotted decimal: four numbers to 255, none with a leading
    zero. *)

val ipv6 : string t
(** An IPv6 address, in RFC 4291 §2.2's full and compressed forms, with a dotted
    IPv4 address in the last thirty-two bits, kept as written: [FE80::1] stays
    in upper case. A zone ([%eth0]) is no part of an address, and is refused. *)

(** {1 Descriptions of descriptions} *)

val list : ?min_items:int -> ?max_items:int -> 'a t -> 'a list t
(** Raises [Invalid_argument] where a count is negative, or [min_items] is more
    than [max_items]. *)

val dict :
  ?min_properties:int ->
  ?max_properties:int ->
  key:'k t ->
  value:'v t ->
  unit ->
  ('k * 'v) list t
(** A JSON object as a map: every member, in the document's order, its name read
    by [key] as the JSON string it is written as -- {!string}, an {!enum}, a
    kind over text such as {!uuid} -- and its value by [value]:
    [J.dict ~key:J.string ~value:(J.uuid ()) ()]. A name that is wrong is a
    problem at the name ({!Problem.Name}). A name given twice is refused both
    ways: in a document, and in a list written with one twice. A key that is not
    written as text is refused on writing, and is an error of the schema. Raises
    [Invalid_argument] where a count is negative, or [min_properties] is more
    than [max_properties]. *)

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
(** The description, or [null] for [None]. Raises [Invalid_argument] where the
    description writes [null] for a value of its own -- a {!nullable}, {!null},
    {!Value.json}, an {!any} that reads [null], or a {!map} or {!kind} over one
    -- since [None] and [Some] of that value would both be written [null]: a
    variant tells them apart. A description of itself still being made, which it
    cannot look into, is left to {!encode}, which refuses such a [Some]. *)

val rec' : 'a t Lazy.t -> 'a t
(** A description of itself. One that comes back to itself before it reads or
    writes anything -- [let rec t = lazy (rec' t)] -- describes nothing:
    {!decode} refuses what is there as {!Problem.Malformed}, and {!encode} the
    value as {!Unwritable.Unspellable}.

    The lazy is forced where the description is first read, written or walked,
    by whichever domain reaches it first, while another reaching it then waits
    for it, so a description may be shared between domains from the start. A
    lazy forced outside the library is forced as OCaml forces it: forced from
    two domains at once, one is told [Lazy.Undefined]. *)

val map :
  ?kind:string ->
  ?doc:string ->
  ?parse:('a -> 'b) ->
  ?print:('b -> 'a) ->
  'a t ->
  'b t
(** Another type over [t]: [parse] from what [t] reads, [print] back to what it
    writes. A direction left out is one the description is never used in:
    reading it, or writing it, is a problem. A [parse] that can fail is a
    {!kind}'s. *)

val kind :
  kind:string ->
  ?doc:string ->
  ?format:Shape.format ->
  parse:('a -> ('b, string) result) ->
  ?print:('b -> 'a) ->
  'a t ->
  'b t
(** A value with a grammar of its own -- an id, a coordinate -- over [t], called
    [kind] as every description is named: [parse]'s [Error] is a sentence for a
    person, a {!Problem.Malformed} at the value's place. Without [print] it is
    only read. *)

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
    ?field:string ->
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
  (** A member. [field] is the OCaml field it is in, where that is not its name:
      [Schema.Melange] names the field so, where it would otherwise make one
      from the member's name -- [kind] for a member ["type"], not [type_]. The
      deriver gives it wherever a field is renamed. Without [absent] it is
      required; with it, it may be left out and reads as [absent]. [omit] says
      which values are written by leaving it out, and needs [absent] where the
      object is read. [enc] is how it is found in a value; without it the object
      only reads. [~access:Read_only] makes it the server's to give: it is never
      read -- one a request holds is skipped, as a client sends back what it was
      given, and it reads as [absent] -- and is in no request's schema.
      [~access:Write_only] makes it the client's: it is never written, and is in
      no answer's schema. *)

  val opt_mem :
    ?field:string ->
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
      the member holds when it is there, and [field] {!mem}'s. Raises
      [Invalid_argument] where the description writes [null] for a value of its
      own, as {!nullable} says, since [Some] of it would be written [null] and
      read back as [None]: {!mem} with [~absent] over that description reads a
      missing member as one value and [null] as its own -- [~absent:None] over a
      {!nullable}, [~absent:Value.Null] over {!Value.json}. *)

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
      read as required, or a read-only member with no [absent], which it never
      reads and so could not build a value without: an object that is only
      written is made by {!enc_only}. *)

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
