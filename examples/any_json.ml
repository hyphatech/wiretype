module J = Wiretype

let () =
  match J.decode J.Value.json {|{"sku": "A1", "count": 2}|} with
  | Error problems -> print_endline (J.Problem.list_to_string problems)
  | Ok value -> (
      match J.Value.find "count" value with
      | Some (Number n) -> Printf.printf "count %g\n" n
      | Some _ | None -> print_endline "no count")
