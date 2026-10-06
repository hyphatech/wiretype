(** Any JSON value: what reads what nobody described.

    A number is a double, as I-JSON's (RFC 7493) are; an exact integer is read
    through a description ({!Wiretype.int}), which reads the digits. An object's
    members are in the order the document has them. *)

type t =
  | Null
  | Bool of bool
  | Number of float
  | String of string
  | Array of t list
  | Object of (string * t) list

val equal : t -> t -> bool
(** Structural, members in order; numbers by [Float.equal]. *)

val find : string -> t -> t option
(** The member of that name, if this is an object that has one. *)

val write : Buffer.t -> t -> unit
(** Minified, as {!Wiretype.encode} writes. *)

val to_string : t -> string
val pp : Format.formatter -> t -> unit
