type event = {
  at : Ptime.t;
  lasted : Mtime.Span.t;
  where : float * float;
  scores : (string * int) list; [@dict] [@max_properties 20]
  extra : Wiretype.Value.t;
  state : Status.status;
  total : float; [@with Wiretype.number_bounded ~min:0. ()]
}
[@@deriving wiretype]
