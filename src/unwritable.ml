type code =
  | Read_only
  | Unspellable
  | Not_utf8
  | Repeated_member
  | Name_not_text
  | Too_deep

let code_to_string = function
  | Read_only -> "read_only"
  | Unspellable -> "unspellable"
  | Not_utf8 -> "not_utf8"
  | Repeated_member -> "repeated_member"
  | Name_not_text -> "name_not_text"
  | Too_deep -> "too_deep"

type t = { at : Problem.segment list; code : code; message : string }

let to_string e =
  Printf.sprintf "%s: %s (%s)"
    (match Problem.path e.at with "" -> "the value" | s -> s)
    e.message (code_to_string e.code)
