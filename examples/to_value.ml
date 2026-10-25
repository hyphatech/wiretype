module J = Wiretype

let () =
  match J.to_value (J.tuple2 J.int J.string) (1, "one") with
  | Ok value -> print_endline (J.Value.to_string value)
  | Error u -> print_endline (J.Unwritable.to_string u)
