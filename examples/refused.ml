module J = Wiretype

let print = function
  | Ok text -> print_endline text
  | Error u -> print_endline (J.Unwritable.to_string u)

let () =
  print
    (J.encode Account.account_json
       { id = 7; email = String.make 300 'a'; password = ""; balance = 0. });
  print (J.encode J.number Float.nan);
  print (J.encode J.string "\xff");
  print (J.encode (J.map ~parse:String.length J.string) 3)
