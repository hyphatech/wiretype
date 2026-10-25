module J = Wiretype

let show name = function
  | Ok s -> Printf.printf "%s: %s\n" name s
  | Error (u : J.Unwritable.t) ->
      Printf.printf "%s: %s\n" name (J.Unwritable.to_string u)

let read name description text =
  match J.decode description text with
  | Ok _ -> Printf.printf "%s: read\n" name
  | Error problems ->
      Printf.printf "%s: %s\n" name (J.Problem.list_to_string problems)

let () =
  show "line"
    (J.encode Line_by_hand.line_json { sku = "A1"; quantity = 2; note = None });
  show "size" (J.encode Size.size_json Medium);
  show "tags" (J.encode Collections.tags [ "tea"; "green" ]);
  show "point" (J.encode Collections.point (1.5, -2.));
  show "scores" (J.encode Collections.scores [ ("ada", 3); ("alan", 2) ]);
  show "parent" (J.encode Collections.parent None);
  show "figure" (J.encode Figure.figure_json (Circle 2.));
  show "id" (J.encode Flexible_id.id_json 7);
  show "order number" (J.encode Order_number.order_number_json 42);
  show "comment"
    (J.encode Comment.comment_json
       { text = "First"; replies = [ { text = "Second"; replies = [] } ] });
  read "figure" Figure.figure_json {|{"side": 3, "type": "square"}|};
  read "figure" Figure.figure_json {|{"type": "hexagon"}|};
  read "id" Flexible_id.id_json {|"7"|};
  read "id" Flexible_id.id_json {|"seven"|};
  read "order number" Order_number.order_number_json {|"ord_x"|}
