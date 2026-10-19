(* The descriptions the shared rows are read by, on the server and in the
   browser: one for each line of what the Melange printer prints, named by
   the component each is in the printed module. The rows are in shapes.json,
   beside this file; the printed module is written from these into
   test/melange/printed.ml. *)

module J = Wiretype

type packed = P : 'a J.t -> packed
type level = Debug | Info | In_progress

let level =
  J.enum ~kind:"level" ~doc:"How much is said."
    (function
      | Debug -> "debug" | Info -> "info" | In_progress -> "in-progress")
    [ Debug; Info; In_progress ]

(* Every scalar the printer has a word for, in one object. *)
type line = {
  sku : string;
  quantity : int;
  note : string option;
  price : float;
  tags : string list;
  placed : Ptime.t;
  id : Uuidm.t;
  day : int * int * int;
  ttl : Mtime.Span.t;
}

let line =
  J.Object.map ~kind:"order line" ~doc:"A line of an order."
    (fun sku quantity note price tags placed id day ttl ->
      { sku; quantity; note; price; tags; placed; id; day; ttl })
  |> J.Object.mem "sku" J.string ~enc:(fun l -> l.sku)
  |> J.Object.mem "quantity" (J.int_bounded ~min:1 ~max:99 ()) ~doc:"how many"
       ~enc:(fun l -> l.quantity)
  |> J.Object.opt_mem "note" (J.string_bounded ~max_length:4 ()) ~enc:(fun l ->
      l.note)
  |> J.Object.mem "price" (J.number_bounded ~min:0. ()) ~enc:(fun l -> l.price)
  |> J.Object.mem "tags" (J.list ~max_items:2 J.string) ~enc:(fun l -> l.tags)
  |> J.Object.mem "placed" J.instant ~enc:(fun l -> l.placed)
  |> J.Object.mem "id" (J.uuid ()) ~enc:(fun l -> l.id)
  |> J.Object.mem "day" J.date ~enc:(fun l -> l.day)
  |> J.Object.mem "ttl" J.duration ~enc:(fun l -> l.ttl)
  |> J.Object.finish

(* Bounds of every kind, an enum, a default and a refusal of unknown
   members. *)
type settings = {
  level : level;
  ratio : float;
  step : int;
  amount : float;
  count : int;
  big : int64;
}

let settings =
  J.Object.map ~kind:"settings" (fun level ratio step amount count big ->
      { level; ratio; step; amount; count; big })
  |> J.Object.mem "level" level ~enc:(fun s -> s.level)
  |> J.Object.mem "ratio" (J.number_bounded ~above:0. ~below:1. ())
       ~enc:(fun s -> s.ratio)
  |> J.Object.mem "step" (J.int_bounded ~min:0 ~max:100 ~multiple_of:5 ())
       ~enc:(fun s -> s.step)
  |> J.Object.mem "amount" (J.number_bounded ~multiple_of:0.01 ())
       ~enc:(fun s -> s.amount)
  |> J.Object.mem "count" J.int ~absent:0 ~enc:(fun s -> s.count)
  |> J.Object.mem "big" J.int64 ~enc:(fun s -> s.big)
  |> J.Object.error_unknown |> J.Object.finish

(* A union by tag, with a case of no members that the tag may leave out. *)
type shape = Circle of float | Square of float | Dot

let shape =
  let circle =
    J.Object.Case.map "circle"
      (J.Object.map (fun r -> r)
      |> J.Object.mem "radius" J.number ~enc:Fun.id
      |> J.Object.finish)
      ~dec:(fun r -> Circle r)
  in
  let square =
    J.Object.Case.map "square"
      (J.Object.map (fun s -> s)
      |> J.Object.mem "side" J.number ~enc:Fun.id
      |> J.Object.finish)
      ~dec:(fun s -> Square s)
  in
  let dot =
    J.Object.Case.map "dot"
      (J.Object.map () |> J.Object.finish)
      ~dec:(fun () -> Dot)
  in
  J.Object.map ~kind:"shape" Fun.id
  |> J.Object.case_mem "kind" J.string ~absent:"dot" ~omit:(String.equal "dot")
       ~enc:Fun.id
       ~enc_case:(function
         | Circle r -> J.Object.Case.value circle r
         | Square s -> J.Object.Case.value square s
         | Dot -> J.Object.Case.value dot ())
       J.Object.Case.[ make circle; make square; make dot ]
  |> J.Object.finish

(* A kind that holds itself. *)
type tree = { value : int; children : tree list }

let tree =
  let rec t =
    lazy
      (J.Object.map ~kind:"tree" (fun value children -> { value; children })
      |> J.Object.mem "value" J.int ~enc:(fun t -> t.value)
      |> J.Object.mem "children" (J.list (J.rec' t)) ~enc:(fun t -> t.children)
      |> J.Object.finish)
  in
  Lazy.force t

(* An object with no kind inside one, and names OCaml must remake. *)
type corner = { x : float; y : float }
type box = { kind : string; first_name : string; at : corner }

let box =
  let corner =
    J.Object.map (fun x y -> { x; y })
    |> J.Object.mem "x" J.number ~enc:(fun c -> c.x)
    |> J.Object.mem "y" J.number ~enc:(fun c -> c.y)
    |> J.Object.finish
  in
  J.Object.map ~kind:"box" (fun kind first_name at -> { kind; first_name; at })
  |> J.Object.mem ~field:"kind" "type" J.string ~enc:(fun b -> b.kind)
  |> J.Object.mem "first-name" J.string ~enc:(fun b -> b.first_name)
  |> J.Object.mem "at" corner ~enc:(fun b -> b.at)
  |> J.Object.finish

(* A value of two sorts. *)
type either = Count of int | Word of string

let either =
  J.any ~kind:"either"
    ~dec_number:(J.map ~parse:(fun n -> Count n) J.int)
    ~dec_string:(J.map ~parse:(fun w -> Word w) J.string)
    ~enc:(function
      | Count n -> J.map ~print:(fun _ -> n) J.int
      | Word w -> J.map ~print:(fun _ -> w) J.string)
    ()

(* The server's and the client's halves of one object. *)
type account = { account_id : Uuidm.t; name : string; password : string }

let account =
  J.Object.map ~kind:"account" (fun account_id name password ->
      { account_id; name; password })
  |> J.Object.mem "id" (J.uuid ()) ~access:Read_only ~absent:Uuidm.nil
       ~enc:(fun a -> a.account_id)
  |> J.Object.mem "name" J.string ~enc:(fun a -> a.name)
  |> J.Object.mem "password" J.string ~access:Write_only ~enc:(fun a ->
      a.password)
  |> J.Object.finish

(* What is read by its name in the rows: a component's name, as the printed
   module names its functions from it. *)
let all : (string * packed) list =
  [
    ("OrderLine", P line);
    ("Settings", P settings);
    ("Shape", P shape);
    ("Tree", P tree);
    ("Box", P box);
    ("Either", P either);
    ("Account", P account);
    ("Scores", P (J.dict ~max_properties:2 ~key:level ~value:J.int ()));
    ("Pair", P (J.tuple3 J.string J.int J.bool));
    ("Maybe", P (J.nullable J.int));
    ("Anything", P J.Value.json);
    ("Words", P (J.list ~min_items:1 (J.string_bounded ~min_length:2 ())));
    ("Instant", P J.instant);
    ("Date", P J.date);
    ("Duration", P J.duration);
    ("Uuid", P (J.uuid ()));
    ("UuidV7", P (J.uuid ~version:V7 ()));
    ("Base64", P J.base64);
    ("Base64url", P J.base64url);
    ("Uri", P J.uri);
    ("Ipv4", P J.ipv4);
    ("Ipv6", P J.ipv6);
  ]

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
