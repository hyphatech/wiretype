(** JSON's two spellings that are not obvious, written into a buffer: what a
    document is written with, and what a caller writing JSON by hand -- a log
    line -- writes with too, so the two spell alike. *)

val number : Buffer.t -> float -> unit
(** In the fewest digits that read back as the same double, and of those the
    nearest to it -- [0.1], [0.30000000000000004], [5e-324] -- laid out as [%g]
    lays one out; an integer below 2{^ 53} as one, and a non-finite number as
    [null]. *)

val string : Buffer.t -> string -> unit
(** Quoted, with what RFC 8259 §7 requires escaped, and DEL beside the control
    characters. The bytes are written as they are. *)

val quote : string -> string
(** {!string}, as a string of its own. *)
