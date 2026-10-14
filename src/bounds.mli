(** The bounds a description states, each one a value breaks said with a code
    and a sentence: the reader and the writer hold a value to them alike, so
    what {!Encode} writes {!Decode} reads, and the two say a bound in the same
    words. *)

type broken = (Problem.code * string) list
(** Each bound broken, in the order the description states them; empty where the
    value is within them all. *)

val int : Shape.int_bounds -> int -> broken
val int64 : Shape.int64_bounds -> int64 -> broken
val number : Shape.number_bounds -> float -> broken

val length : Shape.length -> string -> broken
(** In code points, as JSON Schema counts a string's length. *)

val items : min:int option -> max:int option -> int -> broken
(** How many items a list has. *)

val members : min:int option -> max:int option -> int -> broken
(** How many members a map has. *)
