(** Writing a value by its description, minified. *)

val max_depth : int
(** How deep a document may nest: what {!Decode.run} reads unless told
    otherwise, and so what is written. *)

type 'o packed = Packed : ('o, 'c) Shape.cases -> 'o packed

val find_cases : ('o, 'f) Shape.fields -> 'o packed option
(** The object's union, the first declared if it has several: the one that is
    read and written. *)

val run : 'a Shape.t -> 'a -> (string, Unwritable.t) result
(** [Error] is the first place that could not be written. *)
