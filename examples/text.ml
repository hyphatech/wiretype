(* A value as one text -- a query parameter, a path segment, a header --
   read and written by the description a body's member is. *)

module J = Wiretype

let per_page = J.int_bounded ~min:1 ~max:100 ()

let show = function
  | Ok n -> string_of_int n
  | Error problems ->
      String.concat "; "
        (List.map
           (fun (p : J.Problem.t) ->
             J.Problem.code_to_string p.code ^ ": " ^ p.message)
           problems)

let () =
  List.iter
    (fun text -> print_endline (text ^ " -> " ^ show (J.of_text per_page text)))
    [ "20"; "200"; "many" ];
  match J.to_text J.string {|say "hi"|} with
  | Ok text -> print_endline text
  | Error u -> print_endline (J.Unwritable.to_string u)
