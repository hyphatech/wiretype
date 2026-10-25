module J = Wiretype

let ctx = J.Schema.create ()

let item member =
  J.Object.map ~kind:"item" Fun.id
  |> J.Object.mem member J.int ~enc:Fun.id
  |> J.Object.finish

let walk at description =
  ignore (J.Schema.walk ctx Encode ~at description : J.Schema.t)

let () =
  walk "first" (item "a");
  walk "second" (item "b");
  walk "big" (J.int_bounded ~max:(1 lsl 60) ());
  walk "extra" J.Value.json;
  List.iter
    (fun e -> print_endline (J.Schema.error_to_string e))
    (J.Schema.errors ctx);
  List.iter print_endline (J.Schema.loose ctx)
