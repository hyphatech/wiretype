(* The shared rows in the browser's runtime: each row's text read by the
   printed module, its problems compared by place and code with what the
   server lists, and what reads back written again, but where a row says the
   browser is the looser. Run by Bun over the two files named on its command
   line: the shapes' rows and the kinds' rows. *)

external read_file : string -> string -> string = "readFileSync"
[@@mel.module "node:fs"]

external argv : string array = "argv" [@@mel.scope "process"]
external exit : int -> unit = "exit" [@@mel.scope "process"]

let failures = ref 0

let fail text what expected found =
  incr failures;
  Js.log
    ("FAIL " ^ text ^ "\n  " ^ what ^ "\n  expected: " ^ expected
   ^ "\n  found:    " ^ found)

(* A JSON value's members, where it is an object. *)
let members j =
  match Js.Json.classify j with JSONObject o -> o | _ -> Js.Dict.empty ()

let string_member o name = Option.bind (Js.Dict.get o name) Js.Json.decodeString

let bool_member o name =
  match Js.Dict.get o name with
  | Some j -> (
      match Js.Json.classify j with
      | JSONTrue -> Some (Some true)
      | JSONFalse -> Some (Some false)
      | JSONNull -> Some None
      | _ -> None)
  | None -> None

let array_member o name = Option.bind (Js.Dict.get o name) Js.Json.decodeArray

let problems_member o name =
  Option.map
    (fun items ->
      Array.to_list
        (Array.map
           (fun j ->
             let p = members j in
             ( Option.value (string_member p "at") ~default:"",
               Option.value (string_member p "code") ~default:"" ))
           items))
    (array_member o name)

let compare_problem (a, c) (b, d) =
  match String.compare a b with 0 -> String.compare c d | n -> n

let sorted ps = List.sort_uniq compare_problem ps
let same a b = List.equal (fun p q -> compare_problem p q = 0) a b

let show ps =
  "["
  ^ String.concat ", " (List.map (fun (at, code) -> at ^ " (" ^ code ^ ")") ps)
  ^ "]"

let of_reported (p : Printed.Problem.t) =
  (p.at, Printed.Problem.code_to_string p.code)

(* Each component as the rows name it: read from text, then written back. *)
let through decode encode text =
  match decode text with Ok v -> encode v | Error ps -> Error ps

let components :
    (string * (string -> (string, Printed.Problem.t list) result)) list =
  Printed.
    [
      ("OrderLine", through decode_order_line encode_order_line);
      ("OrderLineInput", through decode_order_line_input encode_order_line_input);
      ("Settings", through decode_settings encode_settings);
      ("Shape", through decode_shape encode_shape);
      ("Tree", through decode_tree encode_tree);
      ("Box", through decode_box encode_box);
      ("Either", through decode_either encode_either);
      ("Account", through decode_account encode_account);
      ("AccountInput", through decode_account_input encode_account_input);
      ("Scores", through decode_scores encode_scores);
      ("Pair", through decode_pair encode_pair);
      ("Maybe", through decode_maybe encode_maybe);
      ("Anything", through decode_anything encode_anything);
      ("Words", through decode_words encode_words);
      ("Instant", through decode_instant encode_instant);
      ("Date", through decode_date encode_date);
      ("Duration", through decode_duration encode_duration);
      ("Uuid", through decode_uuid encode_uuid);
      ("UuidV7", through decode_uuid_v7 encode_uuid_v7);
      ("Base64", through decode_base64 encode_base64);
      ("Base64url", through decode_base64url encode_base64url);
      ("Uri", through decode_uri encode_uri);
      ("Ipv4", through decode_ipv4 encode_ipv4);
      ("Ipv6", through decode_ipv6 encode_ipv6);
    ]

let component name =
  match List.assoc_opt name components with
  | Some f -> f
  | None ->
      Js.log ("No component is named " ^ name);
      exit 2;
      fun _ -> Error []

(* A row's text is what the server reads, a request: the browser reads it
   by the component's request shape, [<Name>Input], where the two differ.
   What the server writes back, an answer, the browser reads by the
   component itself, and writes again as it is. *)
let shape_row j =
  let o = members j in
  let shape = Option.value (string_member o "shape") ~default:"" in
  let text = Option.value (string_member o "text") ~default:"" in
  let expected =
    match problems_member o "melange" with
    | Some ps -> ps
    | None -> Option.value (problems_member o "problems") ~default:[]
  in
  let answer = Option.value (string_member o "written") ~default:text in
  let written =
    Option.value (string_member o "melange_written") ~default:answer
  in
  let request =
    match List.assoc_opt (shape ^ "Input") components with
    | Some f -> f
    | None -> component shape
  in
  (match request text with
  | Error ps ->
      let found = sorted (List.map of_reported ps) in
      if not (same found (sorted expected)) then
        fail
          (shape ^ " " ^ text)
          "problems"
          (show (sorted expected))
          (show found)
  | Ok back ->
      if not (List.is_empty expected) then
        fail (shape ^ " " ^ text) "problems" (show (sorted expected)) "[]"
      else if not (String.equal back written) then
        fail (shape ^ " " ^ text) "written back" written back);
  if List.is_empty expected then
    match component shape answer with
    | Error ps ->
        fail
          (shape ^ " " ^ answer)
          "the answer read" "[]"
          (show (sorted (List.map of_reported ps)))
    | Ok once -> (
        match component shape once with
        | Ok twice when String.equal once twice -> ()
        | Ok twice -> fail (shape ^ " " ^ answer) "written twice" once twice
        | Error ps ->
            fail
              (shape ^ " " ^ once)
              "the answer read again" "[]"
              (show (sorted (List.map of_reported ps))))

(* The kinds' rows name each kind as test_wiretype does. *)
let kind_component = function
  | "instant" -> "Instant"
  | "date" -> "Date"
  | "duration" -> "Duration"
  | "uuid" -> "Uuid"
  | "uuid v7" -> "UuidV7"
  | "base64" -> "Base64"
  | "base64url" -> "Base64url"
  | "uri" -> "Uri"
  | "ipv4" -> "Ipv4"
  | "ipv6" -> "Ipv6"
  | other -> other

let kind_row kind j =
  let o = members j in
  let text = Option.value (string_member o "text") ~default:"" in
  let server = Option.value (bool_member o "server") ~default:None in
  (* The printed module decides as the server does, but where the row says
     otherwise; a row whose browser verdict is the runtime's to decide --
     [zod] null, a Punycode label -- is not held to one here either. *)
  let expected =
    match (bool_member o "melange", bool_member o "zod") with
    | Some m, _ -> m
    | None, Some None -> None
    | None, (Some (Some _) | None) -> server
  in
  match expected with
  | None -> ()
  | Some expected ->
      let verdict =
        Result.is_ok
          (component (kind_component kind)
             (Js.Json.stringify (Js.Json.string text)))
      in
      if not (Bool.equal verdict expected) then
        fail
          (kind ^ " " ^ text)
          "verdict" (string_of_bool expected) (string_of_bool verdict)

let () =
  let shapes = Js.Json.parseExn (read_file argv.(2) "utf8") in
  let kinds = Js.Json.parseExn (read_file argv.(3) "utf8") in
  let rows = Option.value (Js.Json.decodeArray shapes) ~default:[||] in
  Array.iter shape_row rows;
  Array.iter
    (fun (kind, rows) ->
      Array.iter (kind_row kind)
        (Option.value (Js.Json.decodeArray rows) ~default:[||]))
    (Js.Dict.entries (members kinds));
  Js.log
    (string_of_int (Array.length rows)
    ^ " shape rows and the kinds' rows, " ^ string_of_int !failures ^ " failing"
    );
  if !failures > 0 then exit 1
