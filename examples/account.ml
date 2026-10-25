type account = {
  id : int; [@read_only] [@default 0]
  email : string; [@max_length 254]
  password : string; [@write_only] [@default ""]
  balance : float;
}
[@@deriving wiretype]
