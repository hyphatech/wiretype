module J = Wiretype

let () =
  match
    J.decode Order.order_json
      {|{"items": [{"sku": "A1", "count": 2}, {"count": 0}, {"sku": 7}]}|}
  with
  | Ok _ -> ()
  | Error problems ->
      List.iter
        (fun (p : J.Problem.t) ->
          Printf.printf "%s %s\n"
            (J.Problem.path ~root:"body" p.at)
            (J.Problem.code_to_string p.code))
        problems
