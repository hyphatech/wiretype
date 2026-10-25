module J = Wiretype

type line = {
  sku : string;
  quantity : int; [@min 1] [@max 99]  (** how many *)
  note : string option; [@max_length 200]
}
[@@deriving wiretype]
(** A line of an order. *)

let () =
  let ctx = J.Schema.create () in
  ignore (J.Schema.walk ctx Encode ~at:"line" line_json : J.Schema.t);
  match J.Schema.errors ctx with
  | e :: _ ->
      prerr_endline (J.Schema.error_to_string e);
      exit 1
  | [] ->
      print_string
        (J.Schema.Melange.components
           ~header:"Written by examples/melange.ml; run it again to rewrite."
           (J.Schema.components ctx))
