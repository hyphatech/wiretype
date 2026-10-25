module J = Wiretype

let () =
  match J.nullable (J.nullable J.int) with
  | (_ : int option option J.t) -> ()
  | exception Invalid_argument message -> print_endline message
