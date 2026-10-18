(** Descriptions as schemas: one walk over a description, printed as JSON Schema
    2020-12 and as Zod, so the two cannot disagree. It knows no HTTP: a web
    framework's document is built from it, and so can anybody's.

    A description is walked in a {!dir}ection: what a request's body reads
    ({!Decode}) and what an answer writes ({!Encode}) differ where a member is
    read-only, write-only, or made by {!Wiretype.Object.opt_mem}, which reads
    [null] and never writes it. Each object with a [kind], but for a union's
    case, is a component named by it ({!name_of_kind}), and a request's is
    [<Name>Input] exactly where its description differs by direction, at any
    depth: a name follows from the description and the direction alone, never
    from what else was walked or in what order. A component is walked from its
    own root wherever it is reached, so kinds that hold each other in a ring of
    any length refer to each other, and a recursive description with no kind is
    expanded to the same depth inside each. Two different schemas under one
    name, docs and all, are an error. *)

type dir = Decode | Encode

type number = {
  min : float option;  (** at least *)
  max : float option;  (** at most *)
  above : float option;  (** greater than *)
  below : float option;  (** less than *)
  multiple_of : float option;
}

type string_ = {
  words : string list option;  (** an enum's, exactly *)
  min_length : int option;
  max_length : int option;
  format : Shape.format option;
}

type t =
  | Any
  | Null
  | Boolean
  | Number of number
  | Integer of number
  | String of string_
  | Array of { items : t; min_items : int option; max_items : int option }
  | Tuple of t list  (** exactly these items, in order *)
  | Dict of {
      keys : t;  (** what every name is: a string *)
      values : t;
      min_properties : int option;
      max_properties : int option;
    }  (** an object as a map *)
  | Object of obj
  | Ref of string
  | Nullable of t
  | Union of t list
  | Tagged of {
      tag : string;
      absent : Value.t option;
      cases : (Value.t * obj) list;
    }
  | Described of { doc : string; schema : t }
      (** a shape with the doc it was given, where nothing else holds it -- an
          enum's, a kind's, a map's, a value of several sorts', a union's. JSON
          Schema says it as the subschema's [description], over any the schema
          has of its own; Zod as the comment on the declaration or the member it
          stands for, an item's being JSON Schema's alone. A member's own doc
          wins over it on the member's property. *)

and obj = { about : string; props : prop list; additional : additional }

(** What an object says of a member it does not name. *)
and additional =
  | Allowed  (** anything, and it is no part of the value *)
  | Refused  (** none: {!Wiretype.Object.error_unknown} *)
  | Each of t
      (** each by this schema: beside named members, what {!Dict} cannot say;
          the walk never makes it, and a schema built by hand may *)

and prop = {
  name : string;
  schema : t;
  required : bool;
  doc : string;
  deprecated : bool;
  examples : Value.t list;
}

val no_bounds : number

val text : string_
(** A string, and nothing more said of it. *)

type ctx
(** What one document's walks share: its components, and what was found loose or
    wrong on the way. *)

val create : unit -> ctx

val walk : ctx -> dir -> at:string -> 'a Shape.t -> t
(** [at] names where the description is used, for what is reported. *)

val components : ctx -> (string * t) list
(** Each after the components it refers to, but for one it is recursive with,
    which comes later. *)

val loose : ctx -> string list
(** Every place described loosely: any JSON ({!Wiretype.Value.json}), which says
    nothing of its shape; a recursive description with no kind, where its
    expansion stops; a kind inside a hundred others, each inside the last, which
    only a description that makes a new kind at every level reaches; an
    integer's bound past 2{^ 53}, which is left out since a double holds it only
    roughly; and a value of several sorts that none of its descriptions reads.
*)

(** Why a schema could not be made right. *)
type error_code =
  | Kind_shared  (** two different descriptions under one kind's name *)
  | Kind_not_a_name  (** a kind whose name does not begin with a letter *)
  | Key_not_text  (** a map's key whose description is not text *)
  | Unwritable
      (** a case's tag, or an example, that cannot be written: an example
          outside its member's bounds among them *)

type error = { at : string; code : error_code; message : string }
(** [at] is where the description is used, as {!walk}'s [at] and the path below
    it say. *)

val error_code_to_string : error_code -> string
val error_to_string : error -> string

val errors : ctx -> error list
(** Every place a schema could not be made right, once each. *)

val name_of_kind : string -> string
(** [order line] is [OrderLine]: a component's name. *)

(** JSON Schema 2020-12. *)
module Json_schema : sig
  val of_t : ?defs:string -> t -> Value.t
  (** [defs] is where a component is referred to, ["#/$defs/"] unless given:
      OpenAPI's is ["#/components/schemas/"]. *)

  val document : (string * t) list -> t -> Value.t
  (** A whole schema: the root, with its components under [$defs]. *)
end

(** Zod, as TypeScript: the [zod/mini] module's functions, which tree-shake. *)
module Zod : sig
  val of_t : ?indent:string -> t -> string

  val components : (string * t) list -> string
  (** Each as [export const <Name>Schema] and its [type], in an order a
      reference to one written later is a getter. *)
end

(** OCaml for Melange: one self-contained module, over [Js.Json], [Js.Date] and
    [Js.Dict] alone, in which each component is a type and the functions that
    read and write it, so a browser holds the server's shapes as OCaml types and
    never runs wiretype.

    The module opens with a [Problem] -- [{ at; code; message }], [at] the path
    {!Problem.path} prints, [code] the codes {!Problem.type-code} has and
    [message] its sentences -- and the kinds' types: [Instant.t] over a
    [Js.Date.t], [Duration.t] over milliseconds, [Uuid.t] over the lower-case
    text. Then every component given, named flat by {!val-name}: for [OrderLine]
    the type [order_line], [decode_order_line] from text and
    [order_line_of_json] from a [Js.Json.t], [encode_order_line] to text and
    [order_line_to_json]. Each lists every problem, reading and writing alike,
    since the browser's writer is where a form finds what is wrong before it
    sends.

    What each schema is printed as: any JSON a [Js.Json.t]; null [unit]; a
    number a [float]; an integer an [int] where both its bounds lie within 32
    bits, which Melange's [int] holds, and otherwise a [float] that is whole,
    refused past 2{^ 53} as [z.int()] refuses it; a string a [string], its
    length counted in code points; an enum a variant, a constructor per word; a
    list an [array]; a tuple a tuple; a map an array of pairs, in the document's
    order; an object a record, one with no kind named by its place,
    [order_line_note], and one with no members [unit]; a member that may be left
    out an [option], absent and [null] both [None] where it is nullable too; a
    value of several sorts a variant by sort, [Boolean of _], [Number of _],
    [String of _], [Array of _], [Object of _]; a union by tag a variant, a
    constructor per case with the case's members as an inline record. A name is
    made as OCaml wants one: a field in lower case with each run of other
    characters as [_] and a keyword given [_] after it; a constructor with its
    first letter raised, [W] before a digit; a twin gets [_2]. An object's
    [Each] additional members are read as [Allowed]: the walk never makes one.

    The browser is the looser in four places, by design: a whole number past
    2{^ 53}; a member given twice, which [JSON.parse] reads as the last of them;
    nesting, which [JSON.parse] does not limit; and a URI, held to the browser's
    [URL] and not to the server's grammar. Text that is not JSON is a [syntax]
    problem at the document, where the server names the member it was reading.
    Everything else reads and writes as the server does, and {!Wiretype}'s own
    tests hold the two to one verdict. *)
module Melange : sig
  val name : string -> string
  (** [OrderLine] is [order_line]: the name a component's type and functions are
      made from, so a printer that writes more into the module -- a web
      framework's routes -- knows what to call each. *)

  val module_ : ?header:string -> (string * t) list -> string
  (** The whole module, [header] its first comment: what wrote it, and how to
      write it again. A schema that is no object or enum is an alias of what it
      prints as. *)
end
