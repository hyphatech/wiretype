module J = Wiretype

type point = { x : float; y : float } [@@deriving wiretype]

(** Something drawn. *)
type drawing = Dot of point | Line of { from : point; to_ : point }
[@@deriving wiretype]

type note = {
  id : int; [@read_only] [@default 0]
  text : string; [@max_length 500]
  drawing : drawing;
  hidden : bool option;
}
[@@deriving wiretype]
(** A note on a drawing. *)

let () =
  let ctx = J.Schema.create () in
  ignore (J.Schema.walk ctx Decode ~at:"body" note_json : J.Schema.t);
  ignore (J.Schema.walk ctx Encode ~at:"answer" note_json : J.Schema.t);
  match J.Schema.errors ctx with
  | e :: _ ->
      prerr_endline (J.Schema.error_to_string e);
      exit 1
  | [] ->
      print_string {|import * as z from "zod/mini";|};
      print_string "\n\n";
      print_string (J.Schema.Zod.components (J.Schema.components ctx))
