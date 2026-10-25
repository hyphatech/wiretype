module J = Wiretype

let ctx = J.Schema.create ()
let root = J.Schema.walk ctx Encode ~at:"line" Line.line_json

let () =
  match J.Schema.errors ctx with
  | [] -> ()
  | e :: _ ->
      prerr_endline (J.Schema.error_to_string e);
      exit 1

let json_schema = J.Schema.Json_schema.document (J.Schema.components ctx) root
let zod = J.Schema.Zod.components (J.Schema.components ctx)

let () =
  print_endline (J.Value.to_string json_schema);
  print_string zod
