(** A description of itself, built where it is first reached. *)

val force : 'a Lazy.t -> 'a
(** [Lazy.force], by whichever domain reaches it first, while another reaching
    it then waits for it rather than being told [Lazy.Undefined]. Within one
    domain a lazy forced from inside itself is still [Lazy.Undefined], which is
    how a description looking into itself while it is built learns it is not
    there yet. *)
