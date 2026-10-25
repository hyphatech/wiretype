type user = {
  id : int; [@read_only] [@default 0]
  email : string;
  password : string; [@write_only] [@default ""]
  nickname : string option;
}
[@@deriving wiretype]
(** A user of the shop. *)
