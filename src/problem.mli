(** What is wrong with a document: where, a code a client branches on, and a
    sentence about that one place. Decoding reports every problem a document
    has, but for a {!Syntax} or {!Too_deep} one, after which nothing can be
    read. *)

type segment =
  | Member of string
  | Index of int
  | Name of string
      (** a member's name, where it is the name that is wrong: a map's key *)

type code =
  | Syntax  (** not JSON: nothing after it is read *)
  | Too_deep  (** nested past the limit: nothing after it is read *)
  | Required  (** a member that must be there is not *)
  | Unexpected_type  (** a value of another sort: a string for a number *)
  | Too_small  (** below its minimum, or its type's *)
  | Too_large  (** above its maximum, or its type's *)
  | Not_a_multiple  (** not a multiple of what it must be *)
  | Too_short  (** a string shorter than its minimum *)
  | Too_long  (** a string longer than its maximum *)
  | Too_few  (** a list, or a map, with fewer than its minimum *)
  | Too_many  (** a list, or a map, with more than its maximum *)
  | Unknown_word  (** not one of an enum's words, or of a union's tags *)
  | Unknown_member
      (** a member that an object which refuses unknown ones does not have *)
  | Repeated_member  (** a member given twice (RFC 7493 §2.3) *)
  | Malformed  (** a string its kind cannot read: a date, an id *)

val code_to_string : code -> string
(** [required], [unexpected_type], ...: the word a client reads. *)

type t = { at : segment list; code : code; message : string }
(** [at] is the path from the root; [message] a sentence for a person. *)

val path : ?root:string -> segment list -> string
(** [items[2].count], or [body.items[2].count] with [~root:"body"], and a name
    that is wrong as [scores.purple[name]]; the root itself is [root], or the
    empty string. *)

val to_string : t -> string
(** One line for a log: where, the sentence, the code. *)

val list_to_string : t list -> string
(** Every problem, as one line for a log. *)
