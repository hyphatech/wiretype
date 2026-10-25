type t = { first_name : string; last_name : string; type_ : string }
[@@deriving wiretype] [@@rename_all camel] [@@kind "person"]
