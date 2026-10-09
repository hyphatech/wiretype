(** Reading a document by its description: every problem it has, but for one
    that ends reading, which is reported alone. *)

val run : ?max_depth:int -> 'a Shape.t -> string -> ('a, Problem.t list) result
(** [max_depth] is how deep a document may nest, 512 unless given and never more
    than 10,000: [[]] is nested one deep, and a limit of 0 reads a scalar alone,
    as any below it does. *)

val written : 'a Shape.t -> 'a -> (Value.t, Unwritable.t) result
(** A value as the JSON it is written as. *)
