(** Reading a document by its description: every problem it has, but for one
    that ends reading. *)

val run : ?max_depth:int -> 'a Shape.t -> string -> ('a, Problem.t list) result
(** [max_depth] is how deep a document may nest, 512 unless given. *)
