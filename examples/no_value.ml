module J = Wiretype

let () =
  match J.int_bounded ~min:10 ~max:1 () with
  | (_ : int J.t) -> ()
  | exception Invalid_argument message -> print_endline message
