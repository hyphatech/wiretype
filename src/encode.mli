(** Writing a value by its description, minified. *)

type 'o packed = Packed : ('o, 'c) Shape.cases -> 'o packed

val find_cases : ('o, 'f) Shape.fields -> 'o packed option
(** The object's union, the first declared if it has several: the one that is
    read and written. *)

val run : 'a Shape.t -> 'a -> (string, string) result
(** [Error] says what could not be written: a description made to read only, or
    a value its kind cannot spell. *)
