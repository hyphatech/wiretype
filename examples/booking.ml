module J = Wiretype

type booking = { starts : Ptime.t; lasts : Mtime.Span.t; guest : Uuidm.t }
[@@deriving wiretype]

let () =
  match
    J.decode booking_json
      {|{"starts": "2026-09-30T12:00:00Z", "lasts": "P1W",
         "guest": "0190f6e8-7e0b-7c55-8f1a-3c1d2b4a5e6f"}|}
  with
  | Error problems -> print_endline (J.Problem.list_to_string problems)
  | Ok booking -> (
      match J.encode booking_json booking with
      | Ok text -> print_endline text
      | Error u -> print_endline (J.Unwritable.to_string u))
