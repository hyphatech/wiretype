module J = Wiretype

type line = { sku : string; quantity : int; note : string option }

let line_json =
  J.Object.map ~kind:"line" ~doc:"A line of an order." (fun sku quantity note ->
      { sku; quantity; note })
  |> J.Object.mem "sku" J.string ~enc:(fun l -> l.sku)
  |> J.Object.mem "quantity" (J.int_bounded ~min:1 ~max:99 ()) ~doc:"how many"
       ~enc:(fun l -> l.quantity)
  |> J.Object.opt_mem "note" (J.string_bounded ~max_length:200 ())
       ~enc:(fun l -> l.note)
  |> J.Object.finish
