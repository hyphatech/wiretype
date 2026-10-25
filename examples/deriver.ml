module J = Wiretype

let () =
  let ctx = J.Schema.create () in
  let walk name description =
    ignore (J.Schema.walk ctx Encode ~at:name description : J.Schema.t)
  in
  walk "product" Product.product_json;
  walk "drawing" Drawing.drawing_json;
  walk "answer" Answer.answer_json;
  walk "person" Person.json;
  walk "products" Page.products;
  walk "tree" Tree.tree_json;
  walk "event" Event.event_json;
  print_string (J.Schema.Zod.components (J.Schema.components ctx))
