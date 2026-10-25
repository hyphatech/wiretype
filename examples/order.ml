type item = { sku : string; count : int [@min 1] } [@@deriving wiretype]

type order = {
  id : int; [@read_only] [@default 0]
  items : item list; [@min_items 1]
  note : string option;
}
[@@deriving wiretype]
