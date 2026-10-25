module J = Wiretype

let () =
  match
    J.decode Line.line_json {|{"sku": "A1", "quantity": 120, "note": 7}|}
  with
  | Ok _ -> ()
  | Error problems ->
      List.iter (fun p -> print_endline (J.Problem.to_string p)) problems
