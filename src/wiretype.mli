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
    after which nothing can be read. A number is read from its digits by the
    description that wants it, so an integer is exact; a member given twice is
    refused, as I-JSON (RFC 7493 §2.3) has it, because two readers that choose
    differently read two documents.

    {b Writing} is minified, with each double in the fewest digits that read
    back as itself.

    {b What a description is} is {!Shape}, public and walked by the decoder, the
    encoder and {!Schema}, which prints JSON Schema and zod from it. *)

module Value = Value
module Text = Text
module Problem = Problem
module Shape = Shape
module Schema = Schema

type 'a t = 'a Shape.t

(** {1 Reading and writing} *)

val decode : ?max_depth:int -> 'a t -> string -> ('a, Problem.t list) result
(** [max_depth] is how deep the document may nest, 512 unless given. *)

val encode : 'a t -> 'a -> (string, string) result
(** [Error] says what could not be written: a description made only to read, a
    value its kind cannot spell. *)

val to_value : 'a t -> 'a -> (Value.t, string) result
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
    2{^ 53}, past which a fraction's digits are not exact. *)

val int_bounded : ?min:int -> ?max:int -> ?multiple_of:int -> unit -> int t
(** Checked while reading, and stated in the schema. *)

val int64 : int64 t

val int64_bounded :
  ?min:int64 -> ?max:int64 -> ?multiple_of:int64 -> unit -> int64 t

val number : float t
(** A JSON number, as a double; one that overflows a double is too large. A
    non-finite float is written [null]. *)

val number_bounded :
  ?min:float ->
  ?max:float ->
  ?above:float ->
  ?below:float ->
  ?multiple_of:float ->
  unit ->
  float t
(** [min] and [max] are at least and at most; [above] and [below] greater and
    less than. *)

val string : string t
(** Text, checked as UTF-8. *)

val string_bounded : ?min_length:int -> ?max_length:int -> unit -> string t
(** A length in code points, as JSON Schema counts one. *)

val enum : ?kind:string -> ?doc:string -> ('a -> string) -> 'a list -> 'a t
(** [enum word values]: each value written as its word, and read from it. Its
    words are in the schema, exactly. *)

(** {1 Ready-made kinds}

    Each over a plain OCaml type, so no library's type is in an application's
    API; each with its schema's [format] and its zod check, and accepting no
    string that check refuses. *)

val instant : int t
(** Epoch milliseconds, as RFC 3339 [date-time]: [2026-09-30T12:00:00.000Z],
    read with any fraction and offset -- a fraction past the millisecond is
    dropped -- and written in UTC with milliseconds, so one instant has one
    spelling. A leap second is refused. *)

val date : (int * int * int) t
(** [(year, month, day)], as RFC 3339 [full-date]: [2026-09-30]. *)

val duration : int t
(** Milliseconds, as ISO 8601: [PT1H30M]. Weeks, or days, hours, minutes and
    seconds; never years or months, which are no number of milliseconds. A day
    is 24 hours. *)

val uuid : ?version:int -> unit -> string t
(** RFC 9562, lower-cased: versions 1 to 8, with the nil and the max UUID, or
    the one version asked for. *)

val base64 : string t
(** Bytes, as RFC 4648 §4's padded base64. *)

val base64url : string t
(** Bytes, as RFC 4648 §5's unpadded base64url. *)

val uri : string t
(** RFC 3986, less what the URL standard refuses: a port past 65535, an
    [IPvFuture] host, an [http], [https], [ws], [wss] or [ftp] URI with no host
    or a host that is not a domain or an address. An [xn--] label is decoded as
    Punycode and held to UTS #46 as the URL standard applies it, with Unicode
    15.0.0's tables, the bidi rule of RFC 5893 across the host included. It is
    refused, where a browser might take it, when it holds a joiner or a code
    point that may not be in NFC, since both need a context or a normalisation
    the tables alone cannot check, or when it decodes to nothing but ASCII or to
    another [xn--]. *)

val ipv4 : string t
val ipv6 : string t

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

val value : Value.t t
(** Any JSON. It says nothing of its shape, and a schema says so. *)

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
    ?read_only:bool ->
    ?write_only:bool ->
    ?deprecated:bool ->
    ?examples:'a list ->
    string ->
    'a t ->
    ('o, 'a -> 'b) map ->
    ('o, 'b) map
  (** A member. Without [absent] it is required; with it, it may be left out and
      reads as [absent]. [omit] says which values are written by leaving it out.
      [enc] is how it is found in a value; without it the object only reads.
      [read_only] leaves it out of a request's schema and [write_only] out of an
      answer's. *)

  val opt_mem :
    ?doc:string ->
    ?enc:('o -> 'a option) ->
    ?deprecated:bool ->
    string ->
    'a t ->
    ('o, 'a option -> 'b) map ->
    ('o, 'b) map
  (** A member that may be left out: absent or [null] reads as [None], since
      clients write both for "nothing", and [None] is written by leaving it out.
  *)

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

  (** The cases of a union. *)
  module Case : sig
    type ('c, 'k, 'tag) map = ('c, 'k, 'tag) Shape.case_map

    val map : ?dec:('k -> 'c) -> 'tag -> 'k t -> ('c, 'k, 'tag) map
    (** [map tag object ~dec]: the case [tag] stands for, whose members are
        [object]'s. Without [dec] it is written and never read. Raises
        [Invalid_argument] where [object] is not one: a case is a constant
        written in source. *)

    val make : ('c, 'k, 'tag) map -> ('c, 'tag) Shape.case
    val value : ('c, 'k, 'tag) map -> 'k -> ('c, 'tag) Shape.case_value
  end
end
