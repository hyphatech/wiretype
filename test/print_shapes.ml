(* The printed module for the fixtures' shapes, written to stdout: a rule
   compares it with the one committed under test/melange, and `dune promote`
   rewrites that. Each object with a kind is a component of the walk, in
   both directions where they differ; anything else is printed under the
   fixture's own name. *)

module S = Wiretype.Schema

let () =
  let ctx = S.create () in
  let named =
    List.filter_map
      (fun (name, Fixtures.Shapes.P shape) ->
        let (_ : S.t) = S.walk ctx S.Decode ~at:name shape in
        match S.walk ctx S.Encode ~at:name shape with
        | S.Ref _ -> None
        | root -> Some (name, root))
      Fixtures.Shapes.all
  in
  (match S.errors ctx with
  | [] -> ()
  | errors ->
      List.iter (fun e -> prerr_endline (S.error_to_string e)) errors;
      exit 1);
  print_string
    (S.Melange.components
       ~header:
         "Printed by wiretype from test/fixtures/shapes.ml. Do not edit: run \
          dune promote."
       (S.components ctx @ named))
