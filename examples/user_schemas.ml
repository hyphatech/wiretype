module J = Wiretype

let ctx = J.Schema.create ()
let request = J.Schema.walk ctx Decode ~at:"body" User.user_json
let answer = J.Schema.walk ctx Encode ~at:"answer" User.user_json

let () =
  List.iter
    (fun (name, schema) ->
      Printf.printf "%s: %s\n" name
        (J.Value.to_string (J.Schema.Json_schema.of_t schema)))
    (J.Schema.components ctx);
  print_endline
    (J.Value.to_string
       (J.Schema.Json_schema.document (J.Schema.components ctx) request));
  print_endline
    (J.Value.to_string
       (J.Schema.Json_schema.of_t ~defs:"#/components/schemas/" answer))
