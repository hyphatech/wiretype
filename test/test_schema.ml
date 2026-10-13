(* The schema printers alone: a description walked and printed as JSON
   Schema and as Zod, in an executable that links nothing but the library and Ptime.

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
let error_lines ctx = List.map S.error_to_string (S.errors ctx)

(* One walk, checked for errors, as a caller would. *)
let walked ?(dir = S.Encode) description =
  let ctx = S.create () in
  let root = S.walk ctx dir ~at:"test" description in
  match error_lines ctx with
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
    "the root, a component, named as a request since it reads null where an \
     answer never writes it"
    "#/$defs/ConfigInput"
    (str (member [ "$ref" ] doc));
  let c = member [ "$defs"; "ConfigInput" ] doc in
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

(* Named objects each holding the next, the last the first: a ring of
   [n], with the member of the one at [opt] made by [opt_mem], which a
   request may give as null. *)
type link = { next : link option }

let ring ?(opt = -1) n =
  let rec links =
    lazy
      (Array.init n (fun i ->
           lazy
             (let next = Wiretype.rec' (Lazy.force links).((i + 1) mod n) in
              let link =
                Wiretype.Object.map ~kind:(Printf.sprintf "link %d" i)
                  (fun next -> { next })
              in
              (if i = opt then
                 Wiretype.Object.opt_mem "next" next ~enc:(fun l -> l.next) link
               else
                 Wiretype.Object.mem "next" (Wiretype.nullable next)
                   ~enc:(fun l -> l.next)
                   link)
              |> Wiretype.Object.finish)))
  in
  Array.map (fun l -> Wiretype.rec' l) (Lazy.force links)

(* A ring of named objects ends where it comes back to one being walked,
   however many are in it, and each is named by what it holds wherever the
   walk begins: a ring of three was walked as deep as a recursion with no
   kind is, and the third's member was any JSON. *)
let test_a_ring_of_kinds_is_references () =
  let ctx, _ = walked (ring 3).(0) in
  Alcotest.(check (list string))
    "a ring of three, each a component"
    [ "Link0"; "Link1"; "Link2" ]
    (List.sort String.compare (List.map fst (S.components ctx)));
  Alcotest.(check (list string)) "and nothing loose" [] (S.loose ctx);
  let five = ring ~opt:4 5 in
  List.iter
    (fun start ->
      let ctx, _ = walked ~dir:S.Decode five.(start) in
      Alcotest.(check (list string))
        (Printf.sprintf "a request's, from link %d, each holding the fifth's"
           start)
        [ "Link0Input"; "Link1Input"; "Link2Input"; "Link3Input"; "Link4Input" ]
        (List.sort String.compare (List.map fst (S.components ctx))))
    [ 0; 2; 4 ]

(* A kind made anew at every level is a ring that never comes back: the
   walk ends at a depth no description written by hand reaches, and says
   so, where giving each component its own depth would never end. *)
type storey = { below : storey list }

let rec storey i =
  Wiretype.Object.map ~kind:(Printf.sprintf "storey %d" i) (fun below ->
      { below })
  |> Wiretype.Object.mem "below"
       (Wiretype.list (Wiretype.rec' (lazy (storey (i + 1)))))
       ~enc:(fun l -> l.below)
  |> Wiretype.Object.finish

let test_kinds_made_as_walked_end () =
  List.iter
    (fun dir ->
      let ctx, _ = walked ~dir (storey 0) in
      Alcotest.(check int)
        "as many components as kinds may be inside each other" 100
        (List.length (S.components ctx));
      Alcotest.(check int) "and one place loose" 1 (List.length (S.loose ctx)))
    [ S.Encode; S.Decode ]

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
    |> Wiretype.Object.mem "anything" Wiretype.Value.json ~enc:snd
    |> Wiretype.Object.finish
  in
  let ctx, _ = walked loose in
  Alcotest.(check (list string))
    "each, where it is"
    [
      "test.anything: any JSON (Wiretype.Value.json), which says nothing of \
       its shape";
    ]
    (S.loose ctx);
  let rec rose =
    lazy
      (Wiretype.map
         ~dec:(fun l -> `Rose l)
         ~enc:(fun (`Rose l) -> l)
         (Wiretype.list (Wiretype.rec' rose)))
  in
  let ctx, root = walked (Lazy.force rose) in
  Alcotest.(check string)
    "a recursion with no kind, to its depth"
    "z.array(z.array(z.array(z.array(z.unknown()))))" (S.Zod.of_t root);
  Alcotest.(check (list string))
    "and reported past it"
    [
      "test[][][][]: a recursive description with no kind, any JSON from here; \
       give its object a kind";
    ]
    (S.loose ctx);
  let ctx = S.create () in
  ignore
    (S.walk ctx S.Encode ~at:"x"
       (Wiretype.Object.map Fun.id
       |> Wiretype.Object.mem "when" Wiretype.date ~enc:Fun.id
            ~examples:[ (10000, 1, 1) ]
       |> Wiretype.Object.finish)
      : S.t);
  Alcotest.(check bool)
    "an example that cannot be written is an error" true
    (List.exists
       (contains ~sub:"x.when: An example cannot be written")
       (error_lines ctx))

(* A member nobody describes is refused by the schema as by the reader, and a
   member [opt_mem] made answers as the description it was given. *)
let test_what_the_reader_refuses () =
  let strict =
    Wiretype.Object.map ~kind:"strict" Fun.id
    |> Wiretype.Object.mem "a" Wiretype.int ~enc:Fun.id
    |> Wiretype.Object.error_unknown |> Wiretype.Object.finish
  in
  let ctx, root = walked strict in
  let doc = S.Json_schema.document (S.components ctx) root in
  Alcotest.(check bool)
    "no other member" true
    (match member [ "$defs"; "Strict"; "additionalProperties" ] doc with
    | V.Bool false -> true
    | _ -> false);
  Alcotest.(check bool)
    "and zod's strict object" true
    (contains ~sub:"StrictSchema = z.strictObject({"
       (S.Zod.components (S.components ctx)));
  let maybe =
    Wiretype.Object.map ~kind:"maybe" Fun.id
    |> Wiretype.Object.opt_mem "v" Wiretype.string ~enc:Fun.id
    |> Wiretype.Object.finish
  in
  let ctx, _ = walked maybe in
  Alcotest.(check bool)
    "an answer never holds null" true
    (contains ~sub:"v: z.optional(z.string())"
       (S.Zod.components (S.components ctx)))

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

(* A kind names one component. A request's is [<Name>Input] exactly where
   its description differs by direction, so a name never depends on what else
   was walked, or in what order; two different descriptions under one name
   are an error that names it. *)
let thing_a =
  Wiretype.Object.map ~kind:"thing" Fun.id
  |> Wiretype.Object.mem "a" Wiretype.int ~enc:Fun.id
  |> Wiretype.Object.finish

let thing_b =
  Wiretype.Object.map ~kind:"thing" Fun.id
  |> Wiretype.Object.mem "b" Wiretype.string ~enc:Fun.id
  |> Wiretype.Object.finish

type account = { id : int; email : string }

let account =
  Wiretype.Object.map ~kind:"account" (fun id email -> { id; email })
  |> Wiretype.Object.mem "id" Wiretype.int ~access:`Read_only ~absent:0
       ~enc:(fun a -> a.id)
  |> Wiretype.Object.mem "email" Wiretype.string ~enc:(fun a -> a.email)
  |> Wiretype.Object.finish

let test_a_kind_names_one_description () =
  let walk order =
    let ctx = S.create () in
    let roots = List.map (fun dir -> S.walk ctx dir ~at:"a" account) order in
    (List.map fst (S.components ctx), roots, error_lines ctx)
  in
  let names, roots, errors = walk [ S.Encode; S.Decode ] in
  Alcotest.(check (list string))
    "an answer and a request"
    [ "Account"; "AccountInput" ]
    names;
  Alcotest.(check (list string)) "and no error" [] errors;
  Alcotest.(check bool)
    "each referred to by its own name" true
    (match roots with
    | [ S.Ref "Account"; S.Ref "AccountInput" ] -> true
    | _ -> false);
  let names, _, errors = walk [ S.Decode; S.Encode ] in
  Alcotest.(check (list string))
    "the same, whichever is walked first"
    [ "AccountInput"; "Account" ]
    names;
  Alcotest.(check (list string)) "and still no error" [] errors;
  List.iter
    (fun (first, second) ->
      let ctx = S.create () in
      ignore (S.walk ctx first ~at:"one" thing_a : S.t);
      ignore (S.walk ctx second ~at:"other" thing_b : S.t);
      Alcotest.(check bool)
        "two descriptions under one kind, naming it" true
        (List.exists (contains ~sub:"Thing") (error_lines ctx)))
    [ (S.Encode, S.Encode); (S.Encode, S.Decode); (S.Decode, S.Decode) ];
  let ctx = S.create () in
  ignore
    (S.walk ctx S.Encode ~at:"code"
       (Wiretype.Object.map ~kind:"2fa code" Fun.id
       |> Wiretype.Object.mem "a" Wiretype.int ~enc:Fun.id
       |> Wiretype.Object.finish)
      : S.t);
  Alcotest.(check (list string))
    "a kind that names no identifier"
    [
      "code: The kind \"2fa code\" names no component, since \"2faCode\" does \
       not begin with a letter. (kind_not_a_name)";
    ]
    (error_lines ctx)

(* A bound and a kind are the schema's as they are the decoder's, so the
   browser checks what the server will refuse. *)
type slot = { at : Ptime.t; minutes : int; seats : string list }

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
      "minutes: z.int().check(z.gte(5), z.lte(180), z.refine((n) => n % 5 === \
       0)),";
      "seats: z.array(z.string().check(z.maxLength(40))).check(z.maxLength(2)),";
    ]

(* A printer says what the reader means or reports that it cannot, and
   never says something else. *)
let test_what_the_reader_means () =
  let printed ?(dir = S.Decode) description =
    let ctx, root = walked ~dir description in
    (V.to_string (S.Json_schema.of_t root), S.Zod.of_t root, S.loose ctx)
  in
  let nothing = Wiretype.any ~enc:(fun _ -> Wiretype.int) () in
  List.iter
    (fun dir ->
      Alcotest.(check (triple string string (list string)))
        "a value nothing reads, reported"
        ( "{}",
          "z.unknown()",
          [ "test: a value of several sorts with none to read it by, any JSON" ]
        )
        (printed ~dir nothing))
    [ S.Decode; S.Encode ];
  Alcotest.(check (triple string string (list string)))
    "a bound a double holds only roughly is left out, and reported"
    ( {|{"type":"integer","maximum":5}|},
      "z.int().check(z.lte(5))",
      [ "test: a bound past 2^53, which a double holds only roughly" ] )
    (printed (Wiretype.int64_bounded ~min:(-9007199254740993L) ~max:5L ()));
  Alcotest.(check (triple string string (list string)))
    "an int's too"
    ( {|{"type":"integer"}|},
      "z.int()",
      [ "test: a bound past 2^53, which a double holds only roughly" ] )
    (printed (Wiretype.int_bounded ~max:max_int ()));
  let json, _, _ = printed (Wiretype.uuid ~version:`V7 ()) in
  Alcotest.(check string)
    "a UUID's version, which JSON Schema has no format for"
    {|{"type":"string","format":"uuid","pattern":"^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-7[0-9a-fA-F]{3}-[89abAB][0-9a-fA-F]{3}-[0-9a-fA-F]{12}$"}|}
    json;
  let _, zod, _ =
    printed
      (Wiretype.Object.map Fun.id
      |> Wiretype.Object.mem "__proto__" Wiretype.int ~enc:Fun.id
      |> Wiretype.Object.finish)
  in
  Alcotest.(check bool)
    "a member JavaScript would take for the prototype" true
    (contains ~sub:{|["__proto__"]: z.int(),|} zod)

(* What each printer says of what the rest leave out: exclusive bounds, a
   deprecated or write-only member, a map keyed by what is no text, an
   option of an option, and every kind. *)
let test_each_is_said () =
  let printed ?(dir = S.Encode) description =
    let _, root = walked ~dir description in
    (V.to_string (S.Json_schema.of_t root), S.Zod.of_t root)
  in
  Alcotest.(check (pair string string))
    "exclusive bounds"
    ( {|{"type":"number","exclusiveMinimum":0,"exclusiveMaximum":1}|},
      "z.number().check(z.gt(0), z.lt(1))" )
    (printed (Wiretype.number_bounded ~above:0. ~below:1. ()));
  Alcotest.(check (pair string string))
    "a bound every number meets, which is none"
    ({|{"type":"number","minimum":0}|}, "z.number().check(z.gte(0))")
    (printed (Wiretype.number_bounded ~min:0. ~max:Float.infinity ()));
  Alcotest.(check (pair string string))
    "an int64's bounds"
    ( {|{"type":"integer","minimum":-5,"maximum":5}|},
      "z.int().check(z.gte(-5), z.lte(5))" )
    (printed (Wiretype.int64_bounded ~min:(-5L) ~max:5L ()));
  Alcotest.(check (pair string string))
    "an option of an option, built as a shape, is one"
    ({|{"anyOf":[{"type":"integer"},{"type":"null"}]}|}, "z.nullable(z.int())")
    (printed Wiretype.Shape.(Nullable (Nullable Wiretype.int)));
  let account =
    Wiretype.Object.map (fun a b -> (a, b))
    |> Wiretype.Object.mem "old" Wiretype.int ~deprecated:true ~doc:"gone"
         ~enc:fst
    |> Wiretype.Object.mem "password" Wiretype.string ~access:`Write_only
         ~enc:snd
    |> Wiretype.Object.finish
  in
  let json, zod = printed account in
  Alcotest.(check bool)
    "deprecated, in JSON Schema" true
    (contains
       ~sub:{|"old":{"type":"integer","description":"gone","deprecated":true}|}
       json);
  Alcotest.(check bool)
    "and in zod" true
    (contains ~sub:"/** @deprecated gone */" zod);
  Alcotest.(check bool)
    "a write-only member in no answer" false
    (contains ~sub:"password" (json ^ zod));
  let json, _ = printed ~dir:S.Decode account in
  Alcotest.(check bool) "and in a request" true (contains ~sub:"password" json);
  let ctx = S.create () in
  ignore
    (S.walk ctx S.Encode ~at:"m"
       (Wiretype.dict ~key:Wiretype.int ~value:Wiretype.int ())
      : S.t);
  Alcotest.(check (list string))
    "a map keyed by what is no text"
    [
      "m: A map's names are text, and its key's description is not. \
       (key_not_text)";
    ]
    (error_lines ctx);
  let zod description = snd (printed description) in
  List.iter
    (fun (name, expected, said) -> Alcotest.(check string) name expected said)
    [
      ("an instant", "z.iso.datetime({ offset: true })", zod Wiretype.instant);
      ("a date", "z.iso.date()", zod Wiretype.date);
      ( "a duration",
        "z.iso.duration().check(z.regex(/^P(?:\\d+W|(?=\\d|T\\d)(?:\\d+D)?(?:T(?=\\d)(?:\\d+H)?(?:\\d+M)?(?:\\d+(?:[.,]\\d+)?S)?)?)$/))",
        zod Wiretype.duration );
      ("a uuid", "z.uuid()", zod (Wiretype.uuid ()));
      ( "a uuid of one version",
        {|z.uuid({ version: "v4" })|},
        zod (Wiretype.uuid ~version:`V4 ()) );
      ("base64", "z.base64()", zod Wiretype.base64);
      ("base64url", "z.base64url()", zod Wiretype.base64url);
      ("a uri", "z.url()", zod Wiretype.uri);
      ("an ipv4", "z.ipv4()", zod Wiretype.ipv4);
      ("an ipv6", "z.ipv6()", zod Wiretype.ipv6);
    ]

(* A kind of one's own is its description's schema, with the format it
   names. *)
let test_a_kind_of_ones_own_is_said () =
  let printed description =
    let _, root = walked description in
    (V.to_string (S.Json_schema.of_t root), S.Zod.of_t root)
  in
  let day ?format () =
    Wiretype.kind ~name:"day" ?format ~parse:Result.ok ~print:Fun.id
      Wiretype.string
  in
  Alcotest.(check (pair string string))
    "with a format"
    ({|{"type":"string","format":"date"}|}, "z.iso.date()")
    (printed (day ~format:Wiretype.Shape.Date ()));
  Alcotest.(check (pair string string))
    "without one"
    ({|{"type":"string"}|}, "z.string()")
    (printed (day ()))

(* Two descriptions under one kind are compared whole, docs and all, since
   one would be printed for both. *)
let test_a_kind_is_compared_whole () =
  let thing doc =
    Wiretype.Object.map ~kind:"thing" Fun.id
    |> Wiretype.Object.mem "a" Wiretype.int ~doc ~enc:Fun.id
    |> Wiretype.Object.finish
  in
  let ctx = S.create () in
  ignore
    (S.walk ctx S.Encode ~at:"x"
       (Wiretype.tuple2 (thing "first") (thing "second"))
      : S.t);
  Alcotest.(check (list string))
    "differing in a doc alone"
    [
      "x[1]: Two different descriptions share the kind that names Thing. \
       (kind_shared)";
    ]
    (error_lines ctx);
  let ctx = S.create () in
  ignore
    (S.walk ctx S.Encode ~at:"x" (Wiretype.tuple2 (thing "same") (thing "same"))
      : S.t);
  Alcotest.(check (list string)) "and alike" [] (error_lines ctx);
  let base = Wiretype.Object.map ~kind:"thing" Fun.id in
  let ctx = S.create () in
  ignore
    (S.walk ctx S.Encode ~at:"x"
       (Wiretype.tuple2
          (base
          |> Wiretype.Object.mem "a" Wiretype.int ~enc:Fun.id
          |> Wiretype.Object.finish)
          (base
          |> Wiretype.Object.mem "b" Wiretype.int ~enc:Fun.id
          |> Wiretype.Object.finish))
      : S.t);
  Alcotest.(check int)
    "two objects finished from one start" 1
    (List.length (error_lines ctx))

(* A component is walked once however often it is used: a chain of kinds,
   each using the next twice, took twice as long for each link. *)
let test_a_component_is_walked_once () =
  let rec chain n =
    if n = 0 then Wiretype.null ()
    else
      let next = chain (n - 1) in
      Wiretype.Object.map ~kind:(Printf.sprintf "link %d" n) (fun () () -> ())
      |> Wiretype.Object.mem "a" next ~enc:ignore
      |> Wiretype.Object.mem "b" next ~enc:ignore
      |> Wiretype.Object.finish
  in
  let deep = chain 24 in
  let counter = Mtime_clock.counter () in
  let ctx, _ = walked ~dir:S.Decode deep in
  Alcotest.(check int) "every link" 24 (List.length (S.components ctx));
  Alcotest.(check bool)
    "at once" true
    (Mtime.Span.is_shorter
       (Mtime_clock.count counter)
       ~than:Mtime.Span.(500 * ms))

(* ------------------------------------------------------------------ *)
(* Docs *)

(* A doc is the schema's whatever it is given to: an enum, a kind, a map, a
   value of several sorts and a union each, as an object and a member are. *)
let dot =
  Wiretype.Object.Case.map "dot"
    (Wiretype.Object.map () |> Wiretype.Object.finish)
    ~dec:Fun.id

let test_a_doc_is_said_of_every_shape () =
  let printed ?(dir = S.Encode) description =
    let _, root = walked ~dir description in
    (V.to_string (S.Json_schema.of_t root), S.Zod.of_t root)
  in
  let level =
    Wiretype.enum ~doc:"How much it says."
      (function Debug -> "debug" | Info -> "info")
      [ Debug; Info ]
  in
  Alcotest.(check (pair string string))
    "an enum's"
    ( {|{"type":"string","enum":["debug","info"],"description":"How much it says."}|},
      {|z.enum(["debug", "info"])|} )
    (printed level);
  Alcotest.(check (pair string string))
    "a kind's"
    ( {|{"type":"string","format":"date","description":"A day of the shop's."}|},
      "z.iso.date()" )
    (printed
       (Wiretype.kind ~name:"day" ~doc:"A day of the shop's."
          ~format:Wiretype.Shape.Date ~parse:Result.ok ~print:Fun.id
          Wiretype.string));
  let cents =
    Wiretype.map ~doc:"In cents." ~dec:Fun.id ~enc:Fun.id Wiretype.int
  in
  Alcotest.(check (pair string string))
    "a map's"
    ({|{"type":"integer","description":"In cents."}|}, "z.int()")
    (printed cents);
  Alcotest.(check (pair string string))
    "a value of several sorts'"
    ( {|{"anyOf":[{"type":"boolean"},{"type":"number"}],"description":"A flag or a count."}|},
      "z.union([z.boolean(), z.number()])" )
    (printed
       (Wiretype.any ~doc:"A flag or a count."
          ~dec_bool:
            (Wiretype.map ~dec:(fun _ -> 0.) ~enc:(fun _ -> true) Wiretype.bool)
          ~dec_number:Wiretype.number
          ~enc:(fun _ -> Wiretype.number)
          ()));
  Alcotest.(check (pair string string))
    "an item's, in JSON Schema alone, since no comment stands inside an \
     expression"
    ( {|{"type":"array","items":{"type":"integer","description":"In cents."}}|},
      "z.array(z.int())" )
    (printed (Wiretype.list cents));
  Alcotest.(check (pair string string))
    "a nullable's, beside its null"
    ( {|{"anyOf":[{"type":"integer"},{"type":"null"}],"description":"In cents."}|},
      "z.nullable(z.int())" )
    (printed (Wiretype.nullable cents));
  Alcotest.(check (pair string string))
    "a map's keys, still text"
    ( {|{"type":"object","additionalProperties":{"type":"integer"},"propertyNames":{"type":"string","enum":["debug","info"],"description":"How much it says."}}|},
      {|z.partialRecord(z.enum(["debug", "info"]), z.int())|} )
    (printed (Wiretype.dict ~key:level ~value:Wiretype.int ()));
  let priced =
    Wiretype.Object.map ~kind:"priced" (fun price level -> (price, level))
    |> Wiretype.Object.mem "price" cents ~enc:fst
    |> Wiretype.Object.opt_mem "level" level ~enc:snd
    |> Wiretype.Object.finish
  in
  List.iter
    (fun (dir, name) ->
      let ctx, root = walked ~dir priced in
      let doc = S.Json_schema.document (S.components ctx) root in
      Alcotest.(check (list string))
        "a member's, from its shape"
        [ "In cents."; "How much it says." ]
        (List.map
           (fun m ->
             str (member [ "$defs"; name; "properties"; m; "description" ] doc))
           [ "price"; "level" ]);
      let ts = S.Zod.components (S.components ctx) in
      List.iter
        (fun sub -> Alcotest.(check bool) sub true (contains ~sub ts))
        [
          "  /** In cents. */\n  price: z.int(),";
          "  /** How much it says. */\n  level: z.optional(";
        ])
    [ (S.Encode, "Priced"); (S.Decode, "PricedInput") ];
  let ctx, root =
    walked
      (Wiretype.Object.map ~kind:"shape" ~doc:"Something drawn." Fun.id
      |> Wiretype.Object.case_mem "kind" Wiretype.string ~enc:Fun.id
           ~enc_case:(fun () -> Wiretype.Object.Case.value dot ())
           [ Wiretype.Object.Case.make dot ]
      |> Wiretype.Object.finish)
  in
  let doc = S.Json_schema.document (S.components ctx) root in
  Alcotest.(check string)
    "a union's" "Something drawn."
    (str (member [ "$defs"; "Shape"; "description" ] doc));
  Alcotest.(check bool)
    "and in zod, on its declaration" true
    (contains ~sub:"/** Something drawn. */\nexport const ShapeSchema ="
       (S.Zod.components (S.components ctx)))

(* A member's doc is the one its property says: a shape's doc is the
   shape's, and a member's says what the shape is there, so it wins where
   both would be one [description]. A component keeps its own. *)
let test_a_members_doc_wins () =
  let level =
    Wiretype.enum ~doc:"How much it says."
      (function Debug -> "debug" | Info -> "info")
      [ Debug; Info ]
  in
  let inner =
    Wiretype.Object.map ~kind:"inner" ~doc:"What is inside." Fun.id
    |> Wiretype.Object.mem "a" Wiretype.int ~enc:Fun.id
    |> Wiretype.Object.finish
  in
  let unnamed =
    Wiretype.Object.map ~doc:"Nothing named." Fun.id
    |> Wiretype.Object.mem "b" Wiretype.int ~enc:Fun.id
    |> Wiretype.Object.finish
  in
  let outer =
    Wiretype.Object.map ~kind:"outer" (fun a b c -> (a, b, c))
    |> Wiretype.Object.mem "level" level ~doc:"How loud the log is."
         ~enc:(fun (a, _, _) -> a)
    |> Wiretype.Object.mem "inner" inner ~doc:"The one inside."
         ~enc:(fun (_, b, _) -> b)
    |> Wiretype.Object.mem "unnamed" unnamed ~doc:"The plain one."
         ~enc:(fun (_, _, c) -> c)
    |> Wiretype.Object.finish
  in
  let ctx, root = walked outer in
  let doc = S.Json_schema.document (S.components ctx) root in
  let props = member [ "$defs"; "Outer"; "properties" ] doc in
  Alcotest.(check string)
    "the member's, once"
    {|{"type":"string","enum":["debug","info"],"description":"How loud the log is."}|}
    (V.to_string (member [ "level" ] props));
  Alcotest.(check string)
    "beside a reference"
    {|{"$ref":"#/$defs/Inner","description":"The one inside."}|}
    (V.to_string (member [ "inner" ] props));
  Alcotest.(check string)
    "over an object's own, once" "The plain one."
    (str (member [ "unnamed"; "description" ] props));
  Alcotest.(check int)
    "and no other" 1
    (match member [ "unnamed" ] props with
    | V.Object fields ->
        List.length
          (List.filter (fun (k, _) -> String.equal k "description") fields)
    | _ -> 0);
  Alcotest.(check string)
    "while the component keeps its own" "What is inside."
    (str (member [ "$defs"; "Inner"; "description" ] doc));
  let ts = S.Zod.components (S.components ctx) in
  List.iter
    (fun (sub, there) -> Alcotest.(check bool) sub there (contains ~sub ts))
    [
      ("  /** How loud the log is. */\n  level:", true);
      ("How much it says.", false);
      ("/** What is inside. */\nexport const InnerSchema", true);
    ]

(* A ready-made kind's [format] says what it is, and a sentence of the
   library's would be printed into every schema that uses one; its doc is the
   one its user gives. *)
let test_a_ready_made_kind_says_no_doc () =
  let json description =
    let _, root = walked description in
    V.to_string (S.Json_schema.of_t root)
  in
  List.iter
    (fun (name, said) ->
      Alcotest.(check bool) name false (contains ~sub:"description" said))
    [
      ("an instant", json Wiretype.instant);
      ("a date", json Wiretype.date);
      ("a duration", json Wiretype.duration);
      ("a uuid", json (Wiretype.uuid ()));
      ("base64", json Wiretype.base64);
      ("base64url", json Wiretype.base64url);
      ("a uri", json Wiretype.uri);
      ("an ipv4", json Wiretype.ipv4);
      ("an ipv6", json Wiretype.ipv6);
    ];
  Alcotest.(check string)
    "and one its user gives, as any"
    {|{"type":"string","format":"date-time","description":"When it opens."}|}
    (json
       (Wiretype.map ~doc:"When it opens." ~dec:Fun.id ~enc:Fun.id
          Wiretype.instant))

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
          Alcotest.test_case "a ring of kinds is references" `Quick
            test_a_ring_of_kinds_is_references;
          Alcotest.test_case "kinds made as they are walked end" `Quick
            test_kinds_made_as_walked_end;
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
          Alcotest.test_case "what the reader refuses" `Quick
            test_what_the_reader_refuses;
          Alcotest.test_case "bounds and kinds are said" `Quick
            test_bounds_and_kinds_are_said;
          Alcotest.test_case "what the reader means" `Quick
            test_what_the_reader_means;
          Alcotest.test_case "each is said" `Quick test_each_is_said;
          Alcotest.test_case "a kind of one's own is said" `Quick
            test_a_kind_of_ones_own_is_said;
          Alcotest.test_case "a kind is compared whole" `Quick
            test_a_kind_is_compared_whole;
          Alcotest.test_case "a component is walked once" `Quick
            test_a_component_is_walked_once;
        ] );
      ( "docs",
        [
          Alcotest.test_case "a doc is said of every shape" `Quick
            test_a_doc_is_said_of_every_shape;
          Alcotest.test_case "a member's doc wins" `Quick
            test_a_members_doc_wins;
          Alcotest.test_case "a ready-made kind says no doc" `Quick
            test_a_ready_made_kind_says_no_doc;
        ] );
      ( "zod",
        [ Alcotest.test_case "zod says the same" `Quick test_zod_says_the_same ]
      );
    ]
