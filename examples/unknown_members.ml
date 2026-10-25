module J = Wiretype

let sku_json =
  J.Object.map Fun.id
  |> J.Object.mem "sku" J.string
  |> J.Object.error_unknown |> J.Object.finish

let () =
  match J.decode sku_json {|{"sku": "A1", "colour": "red"}|} with
  | Ok _ -> ()
  | Error problems -> print_endline (J.Problem.list_to_string problems)
