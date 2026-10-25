module J = Wiretype

let is_digit c = c >= '0' && c <= '9'

(* An order's number, written as text: ord_ and its digits. *)
let order_number_json =
  J.kind ~kind:"order number" ~doc:"An order, as ord_ and its digits."
    ~parse:(fun s ->
      match String.split_on_char '_' s with
      | [ "ord"; digits ]
        when String.length digits > 0 && String.for_all is_digit digits -> (
          match int_of_string_opt digits with
          | Some n -> Ok n
          | None -> Error "This number is too large.")
      | _ -> Error "This must be ord_ and its digits.")
    ~print:(fun n -> Printf.sprintf "ord_%d" n)
    J.string
