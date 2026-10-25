module J = Wiretype

let read ?max_depth text =
  match J.decode ?max_depth Order.order_json text with
  | Ok _ -> print_endline "read"
  | Error problems ->
      List.iter (fun p -> print_endline (J.Problem.to_string p)) problems

let () =
  read {|{"items": [], "items": []}|};
  read {|{"items": [}|};
  read ~max_depth:2 {|{"items": [[[]]]}|}
