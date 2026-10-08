(** Descriptions as schemas: one walk over a description, printed as JSON Schema
    2020-12 and as zod, so the two cannot disagree. It knows no HTTP: a web
    framework's document is built from it, and so can anybody's.

    A description is walked in a {!dir}ection: what a request's body reads
    ({!Decode}) and what an answer writes ({!Encode}) differ where a member is
    read-only, write-only, or made by {!Wiretype.Object.opt_mem}, which reads
    [null] and never writes it. Each object with a [kind], but for a union's
    case, is a component named by it ({!name_of_kind}), and a request's is
    [<Name>Input] exactly where its description differs by direction, at any
    depth: a name follows from the description and the direction alone, never
    from what else was walked or in what order. Two different schemas under one
    name, docs and all, are an error. *)

type dir = Decode | Encode

type string_ = {
  words : string list option;  (** an enum's, exactly *)
  min_length : int option;
  max_length : int option;
  format : Shape.format option;
}

type t =
  | Any
  | Never  (** nothing: a value no description reads *)
  | Null
  | Boolean
  | Number of Shape.number_bounds
  | Integer of Shape.number_bounds
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

val no_bounds : Shape.number_bounds

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
    expansion stops; an integer's bound past 2{^ 53}, which is left out since a
    double holds it only roughly; and, in an answer, a value of several sorts
    that none of its descriptions reads. *)

(** Why a schema could not be made right. *)
type error_code =
  | Kind_shared  (** two different descriptions under one kind's name *)
  | Kind_not_a_name  (** a kind whose name does not begin with a letter *)
  | Key_not_text  (** a map's key whose description is not text *)
  | Unwritable  (** a case's tag, or an example, that cannot be written *)

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

(** zod, as TypeScript: the [zod/mini] module's functions, which tree-shake. *)
module Zod : sig
  val of_t : ?indent:string -> t -> string

  val components : (string * t) list -> string
  (** Each as [export const <Name>Schema] and its [type], in an order a
      reference to one written later is a getter. *)
end
