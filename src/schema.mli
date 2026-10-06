(** Descriptions as schemas: one walk over a description, printed as JSON Schema
    2020-12 and as zod, so the two cannot disagree. It knows no HTTP: a web
    framework's document is built from it, and so can anybody's.

    A description is walked in a {!dir}ection: what a request's body reads
    ({!Decode}) and what an answer writes ({!Encode}) differ where a member is
    read-only, write-only, or made by {!Wiretype.Object.opt_mem}, which reads
    [null] and never writes it. Each object with a [kind] is a component named
    by it ({!name_of_kind}); two different schemas under one name are an error,
    unless they differ only by direction, when the request's is [<Name>Input].
*)

type dir = Decode | Encode

type number = {
  min : float option;
  max : float option;
  above : float option;
  below : float option;
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

and obj = { about : string; props : prop list; additional : t option }

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
(** In the order they were first met. *)

val loose : ctx -> string list
(** Every place described loosely: any JSON ({!Wiretype.value}), which says
    nothing of its shape. *)

val errors : ctx -> string list
(** Every place a schema could not be made: two descriptions under one name. *)

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
