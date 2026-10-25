type product = {
  id : Uuidm.t; [@read_only] [@default Uuidm.nil]  (** given by the server *)
  name : string; [@min_length 1] [@max_length 80]
  price : float; [@above 0.] [@multiple_of 0.01]
  stock : int; [@default 0] [@min 0]
  tags : string list; [@max_items 10] [@default []]
  colour : string option; [@key "color"] [@examples [ "red" ]]
  code : string; [@wiretype.deprecated]  (** use [id] *)
  secret : string option; [@write_only]
}
[@@deriving wiretype]
(** A product in the catalogue. *)
