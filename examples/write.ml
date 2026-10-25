module J = Wiretype

let () =
  match
    J.encode Account.account_json
      {
        id = 7;
        email = "ada@example.com";
        password = "hunter2";
        balance = 0.1 +. 0.2;
      }
  with
  | Ok text -> print_endline text
  | Error u -> print_endline (J.Unwritable.to_string u)
