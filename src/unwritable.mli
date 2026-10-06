(** What could not be written: where in the value, a code a caller branches on,
    and a sentence about that one place. Writing stops at the first, since a
    document written in part is no document. *)

type code =
  | Read_only  (** a description made only to read: no [enc] *)
  | Unspellable
      (** a value its kind cannot write: an instant past the year 9999 *)
  | Not_utf8  (** text that is not UTF-8, which no JSON reader takes *)
  | Repeated_member
      (** a member, or a map's name, written twice (RFC 7493 §2.3) *)
  | Name_not_text  (** a map's name its key's description writes as no text *)
  | Too_deep  (** nested past what {!Wiretype.decode} reads by default *)

val code_to_string : code -> string
(** [read_only], [not_utf8], ...: the word a log or a client reads. *)

type t = { at : Problem.segment list; code : code; message : string }
(** [at] is the path from the root, as {!Problem.path} prints it. *)

val to_string : t -> string
(** One line for a log: where, the sentence, the code. *)
