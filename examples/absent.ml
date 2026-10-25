module J = Wiretype

let item = {|{"sku": "A1", "count": 1}|}

let read text =
  match J.decode Order.order_json text with
  | Ok (o : Order.order) ->
      Printf.printf "id %d, note %s\n" o.id
        (Option.value o.note ~default:"none")
  | Error problems -> print_endline (J.Problem.list_to_string problems)

let () =
  read (Printf.sprintf {|{"items": [%s]}|} item);
  read (Printf.sprintf {|{"items": [%s], "note": null}|} item);
  read (Printf.sprintf {|{"id": 99, "items": [%s], "note": "ring"}|} item)
