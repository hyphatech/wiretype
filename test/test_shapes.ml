(* The shapes' rows on the server: each row's text read by its description,
   its problems compared by place and code, and what reads back written
   again. The same file is read in the browser's runtime by test/melange,
   through the module printed from the same descriptions, so the two sides
   are held to one verdict, but where a row says the browser is the looser.
   The rows are read through wiretype itself. *)

module J = Wiretype

type problem = { at : string; code : string }

type row = {
  shape : string;
  text : string;
  problems : problem list;
  written : string option;
}

let problem_json =
  J.Object.map (fun at code -> { at; code })
  |> J.Object.mem "at" J.string
  |> J.Object.mem "code" J.string
  |> J.Object.finish

(* A row's [melange] and [melange_written] are the browser's, where it is
   the looser, and read by test/melange alone. *)
let row_json =
  J.Object.map (fun shape text problems written _ _ ->
      { shape; text; problems; written })
  |> J.Object.mem "shape" J.string
  |> J.Object.mem "text" J.string
  |> J.Object.mem "problems" (J.list problem_json) ~absent:[]
  |> J.Object.opt_mem "written" J.string
  |> J.Object.opt_mem "melange" (J.list problem_json)
  |> J.Object.opt_mem "melange_written" J.string
  |> J.Object.error_unknown |> J.Object.finish

let rows =
  lazy
    (let text =
       In_channel.with_open_bin "fixtures/shapes.json" In_channel.input_all
     in
     match J.decode (J.list row_json) text with
     | Ok rows -> rows
     | Error problems ->
         Alcotest.failf "shapes.json does not read: %s"
           (J.Problem.list_to_string problems))

let compare_problem a b =
  match String.compare a.at b.at with
  | 0 -> String.compare a.code b.code
  | c -> c

let sorted = List.sort_uniq compare_problem

let problem =
  Alcotest.testable
    (fun fmt p -> Format.fprintf fmt "%s (%s)" p.at p.code)
    (fun a b -> compare_problem a b = 0)

let of_reported (p : J.Problem.t) =
  { at = J.Problem.path p.at; code = J.Problem.code_to_string p.code }

let run (Fixtures.Shapes.P shape) row () =
  match J.decode shape row.text with
  | Error problems ->
      Alcotest.(check (list problem))
        row.text (sorted row.problems)
        (sorted (List.map of_reported problems))
  | Ok v -> (
      Alcotest.(check (list problem)) row.text (sorted row.problems) [];
      match J.encode shape v with
      | Ok written ->
          Alcotest.(check string)
            ("written back: " ^ row.text)
            (Option.value row.written ~default:row.text)
            written
      | Error e ->
          Alcotest.failf "%s cannot be written back: %s" row.text
            (J.Unwritable.to_string e))

let () =
  let rows = Lazy.force rows in
  Alcotest.run "shapes"
    (List.map
       (fun (name, shape) ->
         ( name,
           List.mapi
             (fun i row ->
               Alcotest.test_case
                 (Printf.sprintf "%d %s" i row.text)
                 `Quick (run shape row))
             (List.filter (fun r -> String.equal r.shape name) rows) ))
       Fixtures.Shapes.all)
