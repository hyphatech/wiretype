type size = Small | Large [@@deriving wiretype]

type order = {
  id : Uuidm.t;
  placed : Ptime.t;
  lines : (string * int) list; [@dict]
  size : size;
  gift : string option;
}
[@@deriving wiretype]

let () = Outline.print_members order_json
