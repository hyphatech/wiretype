type point = { x : float; y : float } [@@deriving wiretype]

(** Something drawn. *)
type drawing = Dot of point | Line of { from : point; to_ : point } | Blank
[@@deriving wiretype] [@@tag "kind"]
