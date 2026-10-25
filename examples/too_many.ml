module J = Wiretype

let many = "[" ^ String.concat "," (List.init 5 (fun _ -> "true")) ^ "]"

let () =
  match J.decode ~max_problems:2 (J.list J.int) many with
  | Ok _ -> ()
  | Error problems -> print_endline (J.Problem.list_to_string problems)
