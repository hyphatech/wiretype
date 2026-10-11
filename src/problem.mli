(** What is wrong with a document: where, a code a client branches on, and a
    sentence about that one place. Decoding reports every problem a document
    has, but for a {!Syntax} or {!Too_deep} one, which is reported alone, and
    past its limit of problems, where it stops and ends the list with a
    {!Too_many_problems}. *)

type segment =
  | Member of string
  | Index of int
  | Name of string
      (** a member's name, where it is the name that is wrong: a map's key *)

type code =
  | Syntax  (** not JSON: reported alone *)
  | Too_deep
      (** nested past the limit: reported alone, at the first 16 segments of its
          path, a place the document has, where a deeper path would be a line no
          log keeps; its sentence says how many levels below *)
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
  | Malformed
      (** a value of the right sort that its description still refuses -- a
          string a kind cannot read, a date or an id, or any value a kind turns
          away -- or anything at all where the description reads nothing: an
          object, a case or a map made only to write *)
  | Too_many_problems
      (** more than the limit: the last of the list, at the document, after the
          first found; the rest of the text is not read *)

val code_to_string : code -> string
(** [required], [unexpected_type], ...: the word a client reads. *)

type t = { at : segment list; code : code; message : string }
(** [at] is the path from the root; [message] a sentence for a person. *)

val path : ?root:string -> segment list -> string
(** [items[2].count], or [body.items[2].count] with [~root:"body"], and a name
    that is wrong as [scores.purple[name]]; the root itself is [root], or the
    empty string. A member's name that is empty, or holds anything but letters,
    digits, [_], [-], [$] and non-ASCII text, is written quoted as JSON --
    [items["a.b"]] -- so a path is one line, and two are never spelt alike.
    Quoted too is a name holding what would end a line or reorder the text
    around it though it is not ASCII -- a C1 control, NEL among them, U+2028,
    U+2029, or a bidirectional control: U+061C, U+200E, U+200F, U+202A to
    U+202E, U+2066 to U+2069 -- and each of those is written as JSON's escape,
    [["a\u2028b"]], so a path reads as the name is. *)

val to_string : t -> string
(** One line for a log: where, the sentence, the code. *)

val list_to_string : t list -> string
(** Every problem, as one line for a log. *)
