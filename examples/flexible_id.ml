module J = Wiretype

(* An id a client may send as a number or as text, always written as a
   number. *)
let id_json =
  J.any ~dec_number:J.int
    ~dec_string:
      (J.kind ~kind:"id"
         ~parse:(fun s ->
           match int_of_string_opt s with
           | Some n -> Ok n
           | None -> Error "This must be a whole number.")
         J.string)
    ~enc:(fun _ -> J.int)
    ()
