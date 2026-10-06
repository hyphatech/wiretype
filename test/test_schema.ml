(* The schema printers alone: a description walked and printed as JSON
   Schema and as zod, in an executable that links nothing but the library.

   What is pinned is what somebody with a description relies on: that its
   named objects are components, that an enum's words and a bound are said
   exactly, that a case is pinned to its tag, and that what cannot be said
   exactly is reported rather than guessed at. *)

module S = Wiretype.Schema
module V = Wiretype.Value

let contains ~sub s =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s
    && (String.equal (String.sub s i n) sub || go (i + 1))
  in
  go 0

let member path j =
  List.fold_left
    (fun j k ->
      match j with
      | V.Object members -> (
          match List.assoc_opt k members with
          | Some v -> v
          | None -> Alcotest.failf "no member %s" k)
      | _ -> Alcotest.failf "not an object where %s was wanted" k)
    j path

let str = function V.String s -> s | _ -> Alcotest.fail "not a string"
let items = function V.Array l -> l | _ -> Alcotest.fail "not an array"
let strs j = List.map str (items j)

(* One walk, checked for errors, as a caller would. *)
let walked ?(dir = S.Encode) description =
  let ctx = S.create () in
  let root = S.walk ctx dir ~at:"test" description in
  match S.errors ctx with
  | [] -> (ctx, root)
  | e -> Alcotest.failf "the walk failed: %s" (String.concat "; " e)

(* ------------------------------------------------------------------ *)
(* A config file, described *)

type level = Debug | Info

let level =
  Wiretype.enum ~kind:"level"
    (function Debug -> "debug" | Info -> "info")
    [ Debug; Info ]

type config = { port : int; level : level; name : string option }

let config =
  Wiretype.Object.map ~kind:"config" ~doc:"How the service starts."
    (fun port level name -> { port; level; name })
  |> Wiretype.Object.mem "port" Wiretype.int ~doc:"Where it listens."
       ~enc:(fun c -> c.port)
  |> Wiretype.Object.mem "level" level ~enc:(fun c -> c.level)
  |> Wiretype.Object.opt_mem "name" Wiretype.string ~enc:(fun c -> c.name)
  |> Wiretype.Object.finish

let test_a_document_stands_alone () =
  let ctx, root = walked ~dir:S.Decode config in
  let doc = S.Json_schema.document (S.components ctx) root in
  Alcotest.(check string)
    "the dialect" "https://json-schema.org/draft/2020-12/schema"
    (str (member [ "$schema" ] doc));
  Alcotest.(check string)
    "the root, a component" "#/$defs/Config"
    (str (member [ "$ref" ] doc));
  let c = member [ "$defs"; "Config" ] doc in
  Alcotest.(check string)
    "its documentation" "How the service starts."
    (str (member [ "description" ] c));
  Alcotest.(check string)
    "an int is an integer" "integer"
    (str (member [ "properties"; "port"; "type" ] c));
  Alcotest.(check string)
    "a member's documentation" "Where it listens."
    (str (member [ "properties"; "port"; "description" ] c));
  Alcotest.(check (list string))
    "an enum's words, exactly" [ "debug"; "info" ]
    (strs (member [ "properties"; "level"; "enum" ] c));
  Alcotest.(check (list string))
    "what may be absent is not required" [ "port"; "level" ]
    (strs (member [ "required" ] c));
  Alcotest.(check (list string)) "and nothing loose" [] (S.loose ctx)

(* Where the components live is the caller's: an OpenAPI document keeps them
   under its own [components]. *)
let test_components_live_where_the_caller_says () =
  let _, root = walked config in
  Alcotest.(check string)
    "a reference into OpenAPI's" "#/components/schemas/Config"
    (str
       (member [ "$ref" ]
          (S.Json_schema.of_t ~defs:"#/components/schemas/" root)))

(* A recursive description is one component that refers to itself. *)
type tree = { label : string; children : tree list }

let tree =
  let rec t =
    lazy
      (Wiretype.Object.map ~kind:"tree" (fun label children ->
           { label; children })
      |> Wiretype.Object.mem "label" Wiretype.string ~enc:(fun t -> t.label)
      |> Wiretype.Object.mem "children"
           (Wiretype.list (Wiretype.rec' t))
           ~enc:(fun t -> t.children)
      |> Wiretype.Object.finish)
  in
  Lazy.force t

let test_recursion_is_a_reference () =
  let ctx, root = walked tree in
  let doc = S.Json_schema.document (S.components ctx) root in
  Alcotest.(check string)
    "its children are itself" "#/$defs/Tree"
    (str
       (member
          [ "$defs"; "Tree"; "properties"; "children"; "items"; "$ref" ]
          doc))

(* ------------------------------------------------------------------ *)
(* Cases *)

(* A value told apart by one member is one of its cases, each pinned. *)
type shape = Circle of int | Square of int

let shape_json =
  let circle =
    Wiretype.Object.map ~kind:"circle" (fun r -> r)
    |> Wiretype.Object.mem "radius" Wiretype.int ~enc:Fun.id
    |> Wiretype.Object.finish
  and square =
    Wiretype.Object.map ~kind:"square" (fun s -> s)
    |> Wiretype.Object.mem "side" Wiretype.int ~enc:Fun.id
    |> Wiretype.Object.finish
  in
  let circle =
    Wiretype.Object.Case.map "circle" circle ~dec:(fun r -> Circle r)
  in
  let square =
    Wiretype.Object.Case.map "square" square ~dec:(fun s -> Square s)
  in
  Wiretype.Object.map ~kind:"shape" Fun.id
  |> Wiretype.Object.case_mem "kind" Wiretype.string ~enc:Fun.id
       ~enc_case:(function
         | Circle r -> Wiretype.Object.Case.value circle r
         | Square s -> Wiretype.Object.Case.value square s)
       [ Wiretype.Object.Case.make circle; Wiretype.Object.Case.make square ]
  |> Wiretype.Object.finish

let test_cases_are_one_of_each () =
  let ctx, root = walked shape_json in
  let doc = S.Json_schema.document (S.components ctx) root in
  Alcotest.(check (list string))
    "each case, its tag pinned" [ "circle"; "square" ]
    (List.map
       (fun c -> str (member [ "properties"; "kind"; "const" ] c))
       (items (member [ "$defs"; "Shape"; "oneOf" ] doc)));
  let ts = S.Zod.components (S.components ctx) in
  List.iter
    (fun sub -> Alcotest.(check bool) sub true (contains ~sub ts))
    [
      {|z.discriminatedUnion("kind", [|};
      {|kind: z.literal("circle"),|};
      {|kind: z.literal("square"),|};
    ]

(* A tag that may be left out is optional in the case it stands for, and in no
   other: a move is [{"pass": true}] or a point with no [pass] at all. *)
type move = Pass | Play of (int * int)

let move_json =
  let passed =
    Wiretype.Object.Case.map true
      (Wiretype.Object.map () |> Wiretype.Object.finish)
      ~dec:(fun () -> Pass)
  and played =
    Wiretype.Object.Case.map false
      (Wiretype.Object.map (fun r c -> (r, c))
      |> Wiretype.Object.mem "row" Wiretype.int ~enc:fst
      |> Wiretype.Object.mem "col" Wiretype.int ~enc:snd
      |> Wiretype.Object.finish)
      ~dec:(fun p -> Play p)
  in
  Wiretype.Object.map ~kind:"move" Fun.id
  |> Wiretype.Object.case_mem "pass" Wiretype.bool ~absent:false ~omit:not
       ~enc:Fun.id
       ~enc_case:(function
         | Pass -> Wiretype.Object.Case.value passed ()
         | Play p -> Wiretype.Object.Case.value played p)
       Wiretype.Object.Case.[ make passed; make played ]
  |> Wiretype.Object.finish

let test_a_tag_left_out_is_optional () =
  let ctx, root = walked move_json in
  let doc = S.Json_schema.document (S.components ctx) root in
  Alcotest.(check (list (list string)))
    "the tag required where it must be written"
    [ [ "pass" ]; [ "row"; "col" ] ]
    (List.map
       (fun c -> strs (member [ "required" ] c))
       (items (member [ "$defs"; "Move"; "oneOf" ] doc)));
  let ts = S.Zod.components (S.components ctx) in
  List.iter
    (fun sub -> Alcotest.(check bool) sub true (contains ~sub ts))
    [ "pass: z.literal(true),"; "pass: z.optional(z.literal(false))," ]

(* ------------------------------------------------------------------ *)
(* What cannot be said *)

(* A description the walk cannot say exactly is reported where it is, never
   guessed at; the schema is still made. *)
let test_what_is_loose_is_reported () =
  let colour =
    Wiretype.enum ~kind:"colour"
      (fun n -> if n = 1 then "red" else "blue")
      [ 1; 2 ]
  in
  let loose =
    Wiretype.Object.map ~kind:"loose" (fun c j -> (c, j))
    |> Wiretype.Object.mem "colour" colour ~enc:fst
    |> Wiretype.Object.mem "anything" Wiretype.value ~enc:snd
    |> Wiretype.Object.finish
  in
  let ctx, _ = walked loose in
  Alcotest.(check (list string))
    "each, where it is"
    [
      "test.anything: any JSON (Wiretype.value), which says nothing of its \
       shape";
    ]
    (S.loose ctx)

(* An application's own union of all five kinds is described as one; only
   any JSON says nothing, and is reported. *)
type value =
  | Flag of bool
  | Count of float
  | Word of string
  | Words of string list

let value_json =
  let flag =
    Wiretype.map
      ~dec:(fun b -> Flag b)
      ~enc:(function Flag b -> b | _ -> false)
      Wiretype.bool
  and count =
    Wiretype.map
      ~dec:(fun n -> Count n)
      ~enc:(function Count n -> n | _ -> 0.)
      Wiretype.number
  and word =
    Wiretype.map
      ~dec:(fun w -> Word w)
      ~enc:(function Word w -> w | _ -> "")
      Wiretype.string
  and words =
    Wiretype.map
      ~dec:(fun ws -> Words ws)
      ~enc:(function Words ws -> ws | _ -> [])
      Wiretype.(list string)
  in
  let boxed =
    Wiretype.Object.map ~kind:"boxed" (fun w -> Word w)
    |> Wiretype.Object.mem "word" Wiretype.string ~enc:(function
      | Word w -> w
      | _ -> "")
    |> Wiretype.Object.finish
  in
  Wiretype.any ~kind:"value" ~dec_bool:flag ~dec_number:count ~dec_string:word
    ~dec_array:words ~dec_object:boxed
    ~enc:(function
      | Flag _ -> flag | Count _ -> count | Word _ -> word | Words _ -> words)
    ()

let test_a_union_of_five_is_described () =
  let ctx, root = walked ~dir:S.Decode value_json in
  Alcotest.(check (list string)) "nothing loose" [] (S.loose ctx);
  let doc = S.Json_schema.document (S.components ctx) root in
  Alcotest.(check int)
    "a branch each" 5
    (List.length (items (member [ "anyOf" ] doc)))

(* A kind names one component: two different descriptions under it are an
   error that names it -- unless the second is only ever decoded, when it is
   named apart. *)
let thing_a =
  Wiretype.Object.map ~kind:"thing" Fun.id
  |> Wiretype.Object.mem "a" Wiretype.int ~enc:Fun.id
  |> Wiretype.Object.finish

let thing_b =
  Wiretype.Object.map ~kind:"thing" Fun.id
  |> Wiretype.Object.mem "b" Wiretype.string ~enc:Fun.id
  |> Wiretype.Object.finish

let test_a_kind_names_one_description () =
  let ctx = S.create () in
  ignore (S.walk ctx S.Encode ~at:"one" thing_a : S.t);
  ignore (S.walk ctx S.Encode ~at:"other" thing_b : S.t);
  Alcotest.(check bool)
    "naming it" true
    (List.exists (contains ~sub:"Thing") (S.errors ctx));
  let ctx = S.create () in
  ignore (S.walk ctx S.Encode ~at:"answer" thing_a : S.t);
  Alcotest.(check bool)
    "a decoded one is named apart" true
    (match S.walk ctx S.Decode ~at:"body" thing_b with
    | S.Ref "ThingInput" -> true
    | _ -> false);
  Alcotest.(check (list string)) "and is no error" [] (S.errors ctx)

(* A bound and a kind are the schema's as they are the decoder's, so the
   browser checks what the server will refuse. *)
type slot = { at : int; minutes : int; seats : string list }

let slot_json =
  Wiretype.Object.map ~kind:"slot" (fun at minutes seats ->
      { at; minutes; seats })
  |> Wiretype.Object.mem "at" Wiretype.instant ~enc:(fun s -> s.at)
  |> Wiretype.Object.mem "minutes"
       (Wiretype.int_bounded ~min:5 ~max:180 ~multiple_of:5 ()) ~enc:(fun s ->
         s.minutes)
  |> Wiretype.Object.mem "seats"
       (Wiretype.list ~max_items:2 (Wiretype.string_bounded ~max_length:40 ()))
       ~enc:(fun s -> s.seats)
  |> Wiretype.Object.finish

let test_bounds_and_kinds_are_said () =
  let ctx, root = walked slot_json in
  let doc = S.Json_schema.document (S.components ctx) root in
  let props = member [ "$defs"; "Slot"; "properties" ] doc in
  Alcotest.(check string)
    "an instant's format" "date-time"
    (str (member [ "at"; "format" ] props));
  Alcotest.(check string)
    "a range, exactly"
    {|{"type":"integer","minimum":5,"maximum":180,"multipleOf":5}|}
    (V.to_string (member [ "minutes" ] props));
  Alcotest.(check string)
    "a count and a length"
    {|{"type":"array","items":{"type":"string","maxLength":40},"maxItems":2}|}
    (V.to_string (member [ "seats" ] props));
  let ts = S.Zod.components (S.components ctx) in
  List.iter
    (fun sub -> Alcotest.(check bool) sub true (contains ~sub ts))
    [
      "at: z.iso.datetime({ offset: true }),";
      "minutes: z.int().check(z.gte(5), z.lte(180), z.multipleOf(5)),";
      "seats: z.array(z.string().check(z.maxLength(40))).check(z.maxLength(2)),";
    ]

(* ------------------------------------------------------------------ *)
(* zod *)

let test_zod_says_the_same () =
  let ctx = S.create () in
  ignore (S.walk ctx S.Encode ~at:"config" config : S.t);
  ignore (S.walk ctx S.Encode ~at:"tree" tree : S.t);
  let ts = S.Zod.components (S.components ctx) in
  List.iter
    (fun sub -> Alcotest.(check bool) sub true (contains ~sub ts))
    [
      "/** How the service starts. */\nexport const ConfigSchema = z.object({";
      "  /** Where it listens. */\n  port: z.int(),";
      {|level: z.enum(["debug", "info"]),|};
      "name: z.optional(z.string()),";
      "export type Config = z.infer<typeof ConfigSchema>;";
      (* A reference to the schema being written is a getter, or zod reads
         a constant before its declaration. *)
      "  get children() {\n    return z.array(TreeSchema);\n  },";
    ]

let () =
  Alcotest.run "wiretype schema"
    [
      ( "JSON Schema",
        [
          Alcotest.test_case "a document stands alone" `Quick
            test_a_document_stands_alone;
          Alcotest.test_case "components live where the caller says" `Quick
            test_components_live_where_the_caller_says;
          Alcotest.test_case "recursion is a reference" `Quick
            test_recursion_is_a_reference;
        ] );
      ( "cases",
        [
          Alcotest.test_case "cases are one of each" `Quick
            test_cases_are_one_of_each;
          Alcotest.test_case "a tag left out is optional" `Quick
            test_a_tag_left_out_is_optional;
        ] );
      ( "what cannot be said",
        [
          Alcotest.test_case "a union of five is described" `Quick
            test_a_union_of_five_is_described;
          Alcotest.test_case "what is loose is reported" `Quick
            test_what_is_loose_is_reported;
          Alcotest.test_case "a kind names one description" `Quick
            test_a_kind_names_one_description;
          Alcotest.test_case "bounds and kinds are said" `Quick
            test_bounds_and_kinds_are_said;
        ] );
      ( "zod",
        [ Alcotest.test_case "zod says the same" `Quick test_zod_says_the_same ]
      );
    ]
