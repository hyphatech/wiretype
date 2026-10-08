module S = Shape

type dir = Decode | Encode

type number = {
  min : float option;
  max : float option;
  above : float option;
  below : float option;
  multiple_of : float option;
}

type string_ = {
  words : string list option;
  min_length : int option;
  max_length : int option;
  format : S.format option;
}

type t =
  | Any
  | Null
  | Boolean
  | Number of number
  | Integer of number
  | String of string_
  | Array of { items : t; min_items : int option; max_items : int option }
  | Tuple of t list
  | Dict of {
      keys : t;
      values : t;
      min_properties : int option;
      max_properties : int option;
    }
  | Object of obj
  | Ref of string
  | Nullable of t
  | Union of t list
  | Tagged of {
      tag : string;
      absent : Value.t option;
      cases : (Value.t * obj) list;
    }

and obj = { about : string; props : prop list; additional : additional }
and additional = Allowed | Refused | Each of t

and prop = {
  name : string;
  schema : t;
  required : bool;
  doc : string;
  deprecated : bool;
  examples : Value.t list;
}

let no_bounds =
  { min = None; max = None; above = None; below = None; multiple_of = None }

let text = { words = None; min_length = None; max_length = None; format = None }

(* ------------------------------------------------------------------ *)
(* The walk *)

type error_code = Kind_shared | Kind_not_a_name | Key_not_text | Unwritable
type error = { at : string; code : error_code; message : string }

let error_code_to_string = function
  | Kind_shared -> "kind_shared"
  | Kind_not_a_name -> "kind_not_a_name"
  | Key_not_text -> "key_not_text"
  | Unwritable -> "unwritable"

let error_to_string e =
  Printf.sprintf "%s: %s (%s)" e.at e.message (error_code_to_string e.code)

type ctx = {
  mutable components : (string * t) list;  (** newest first *)
  mutable walked : (S.about * dir * string) list;
      (** each object walked into a component, by its [about], which names one
          description, and the component's name *)
  mutable walking : string list;
  mutable loose : string list;
  mutable errors : error list;
}

let create () =
  { components = []; walked = []; walking = []; loose = []; errors = [] }

let format_equal a b =
  match (a, b) with
  | S.Date_time, S.Date_time
  | S.Date, S.Date
  | S.Duration, S.Duration
  | S.Base64, S.Base64
  | S.Base64url, S.Base64url
  | S.Uri, S.Uri
  | S.Ipv4, S.Ipv4
  | S.Ipv6, S.Ipv6 ->
      true
  | S.Uuid a, S.Uuid b ->
      Option.equal
        (fun a b ->
          Int.equal
            (Grammar.uuid_version_number a)
            (Grammar.uuid_version_number b))
        a b
  | ( ( S.Date_time | S.Date | S.Duration | S.Uuid _ | S.Base64 | S.Base64url
      | S.Uri | S.Ipv4 | S.Ipv6 ),
      _ ) ->
      false

let number_equal (a : number) (b : number) =
  let f = Option.equal Float.equal in
  f a.min b.min && f a.max b.max && f a.above b.above && f a.below b.below
  && f a.multiple_of b.multiple_of

(* Whether two schemas say the same, docs and all, since one is printed for
   both where they share a name. *)
let rec equal a b =
  let count = Option.equal Int.equal in
  match (a, b) with
  | Any, Any | Null, Null | Boolean, Boolean -> true
  | Number a, Number b | Integer a, Integer b -> number_equal a b
  | String a, String b ->
      Option.equal (List.equal String.equal) a.words b.words
      && count a.min_length b.min_length
      && count a.max_length b.max_length
      && Option.equal format_equal a.format b.format
  | Array a, Array b ->
      equal a.items b.items
      && count a.min_items b.min_items
      && count a.max_items b.max_items
  | Tuple a, Tuple b | Union a, Union b -> List.equal equal a b
  | Dict a, Dict b ->
      equal a.keys b.keys && equal a.values b.values
      && count a.min_properties b.min_properties
      && count a.max_properties b.max_properties
  | Object a, Object b -> obj_equal a b
  | Ref a, Ref b -> String.equal a b
  | Nullable a, Nullable b -> equal a b
  | Tagged a, Tagged b ->
      String.equal a.tag b.tag
      && Option.equal Value.equal a.absent b.absent
      && List.equal
           (fun (v, o) (w, p) -> Value.equal v w && obj_equal o p)
           a.cases b.cases
  | ( ( Any | Null | Boolean | Number _ | Integer _ | String _ | Array _
      | Tuple _ | Dict _ | Object _ | Ref _ | Nullable _ | Union _ | Tagged _ ),
      _ ) ->
      false

and obj_equal (a : obj) (b : obj) =
  String.equal a.about b.about
  && List.equal
       (fun (p : prop) (q : prop) ->
         String.equal p.name q.name && equal p.schema q.schema
         && Bool.equal p.required q.required
         && String.equal p.doc q.doc
         && Bool.equal p.deprecated q.deprecated
         && List.equal Value.equal p.examples q.examples)
       a.props b.props
  &&
  match (a.additional, b.additional) with
  | Allowed, Allowed | Refused, Refused -> true
  | Each a, Each b -> equal a b
  | (Allowed | Refused | Each _), _ -> false

let name_of_kind kind =
  String.concat ""
    (List.map String.capitalize_ascii
       (List.filter
          (fun w -> not (String.equal w ""))
          (String.split_on_char ' '
             (String.map
                (function
                  | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9') as c -> c | _ -> ' ')
                kind))))

let report ctx at what = ctx.loose <- (at ^ ": " ^ what) :: ctx.loose

let error ctx at code message =
  ctx.errors <- { at; code; message } :: ctx.errors

let register ctx at name schema =
  match List.assoc_opt name ctx.components with
  | None -> ctx.components <- (name, schema) :: ctx.components
  | Some s when equal s schema -> ()
  | Some _ ->
      error ctx at Kind_shared
        (Printf.sprintf
           "Two different descriptions share the kind that names %s." name)

(* An integer bound as a double, where the double is the bound exactly:
   past 2^53 it is the nearest, which would refuse a value the reader takes
   or take one it refuses, so it is left out and reported. *)
let exactly ctx at to_float of_float equal = function
  | None -> None
  | Some n ->
      let f = to_float n in
      if Float.abs f <= 0x1p53 && equal (of_float f) n then Some f
      else (
        report ctx at "a bound past 2^53, which a double holds only roughly";
        None)

let of_int ctx at = exactly ctx at float_of_int Float.to_int Int.equal
let of_int64 ctx at = exactly ctx at Int64.to_float Int64.of_float Int64.equal

(* A recursive description with no name is expanded only this far, and is
   any JSON past it, which is reported; one with a name is a component that
   refers to itself, and never reaches it. *)
let max_depth = 3

(* Whether a description is walked differently as a request and as an
   answer: it has a member in one alone, or one [opt_mem] made, which a
   request may give as [null] and an answer never writes. A component's
   name follows from this alone -- a request's is [<Name>Input] exactly
   where it holds -- so it never depends on what else was walked. [seen]
   are the kinds already looked into, each once: what one holds was either
   found to differ, which ended the search, or not. *)
let rec differs : type a. string list ref -> int -> a S.t -> bool =
 fun seen depth shape ->
  match shape with
  | S.Null _ | S.Bool | S.Int _ | S.Int64 _ | S.Number _ | S.String _ | S.Enum _
  | S.Value ->
      false
  | S.List l -> differs seen depth l.elt
  | S.Tuple items -> items_differ seen depth items
  | S.Dict d -> differs seen depth d.key || differs seen depth d.value
  | S.Nullable t -> differs seen depth t
  | S.Map (_, m) -> differs seen depth m.inner
  | S.Any (_, a) ->
      List.exists
        (Option.fold ~none:false ~some:(differs seen depth))
        [ a.null; a.bool; a.number; a.string; a.array; a.object_ ]
  | S.Rec l -> depth < max_depth && differs seen (depth + 1) (Lazy.force l)
  | S.Object (about, o) ->
      if String.equal about.kind "" then fields_differ seen depth o.fields
      else if List.exists (String.equal about.kind) !seen then false
      else (
        seen := about.kind :: !seen;
        fields_differ seen depth o.fields)

and items_differ : type t f. string list ref -> int -> (t, f) S.items -> bool =
 fun seen depth items ->
  match items with
  | S.Items _ -> false
  | S.Item (prev, it) ->
      items_differ seen depth prev || differs seen depth it.described

and fields_differ : type o f. string list ref -> int -> (o, f) S.fields -> bool
    =
 fun seen depth fields ->
  match fields with
  | S.Build _ | S.Unread -> false
  | S.Mem (prev, m) ->
      (match m.access with
        | `Read_write -> m.opt
        | `Read_only | `Write_only -> true)
      || differs seen depth m.shape
      || fields_differ seen depth prev
  | S.Cases (prev, S.Tagged t) ->
      differs seen depth t.shape
      || List.exists
           (fun (S.Case cm) -> fields_differ seen depth cm.obj.fields)
           t.cases
      || fields_differ seen depth prev

let rec walk_ : type a. ctx -> dir -> string -> int -> a S.t -> t =
 fun ctx dir at depth shape ->
  match shape with
  | S.Null _ -> Null
  | S.Bool -> Boolean
  | S.Int b ->
      Integer
        {
          no_bounds with
          min = of_int ctx at b.min;
          max = of_int ctx at b.max;
          multiple_of = of_int ctx at b.multiple_of;
        }
  | S.Int64 b ->
      Integer
        {
          no_bounds with
          min = of_int64 ctx at b.min;
          max = of_int64 ctx at b.max;
          multiple_of = of_int64 ctx at b.multiple_of;
        }
  | S.Number b ->
      Number
        {
          min = b.min;
          max = b.max;
          above = b.above;
          below = b.below;
          multiple_of = b.multiple_of;
        }
  | S.String l ->
      String { text with min_length = l.min_length; max_length = l.max_length }
  | S.Enum (_, e) -> String { text with words = Some (List.map fst e.words) }
  | S.List l ->
      Array
        {
          items = walk_ ctx dir (at ^ "[]") depth l.elt;
          min_items = l.min_items;
          max_items = l.max_items;
        }
  | S.Tuple items -> Tuple (tuple_items ctx dir at depth items)
  | S.Dict d ->
      let keys = walk_ ctx dir (at ^ "[name]") depth d.key in
      (match keys with
      | String _ -> ()
      | _ ->
          error ctx at Key_not_text
            "A map's names are text, and its key's description is not.");
      Dict
        {
          keys;
          values = walk_ ctx dir (at ^ ".*") depth d.value;
          min_properties = d.min_properties;
          max_properties = d.max_properties;
        }
  | S.Nullable t -> (
      (* [null] once, however many options say it. *)
      match walk_ ctx dir at depth t with
      | Nullable _ as s -> s
      | s -> Nullable s)
  | S.Object (about, o) -> object_ ctx dir at depth about o
  | S.Any (_, a) -> any ctx dir at depth a
  | S.Map (_, m) -> (
      match (walk_ ctx dir at depth m.inner, m.format) with
      | String s, Some f -> String { s with format = Some f }
      | s, (Some _ | None) -> s)
  | S.Value ->
      report ctx at
        "any JSON (Wiretype.Value.json), which says nothing of its shape";
      Any
  | S.Rec l ->
      if depth >= max_depth then (
        report ctx at
          "a recursive description with no kind, any JSON from here; give its \
           object a kind";
        Any)
      else walk_ ctx dir at (depth + 1) (Lazy.force l)

and tuple_items : type v f.
    ctx -> dir -> string -> int -> (v, f) S.items -> t list =
 fun ctx dir at depth items ->
  match items with
  | S.Items _ -> []
  | S.Item (prev, it) ->
      let before = tuple_items ctx dir at depth prev in
      before
      @ [
          walk_ ctx dir
            (Printf.sprintf "%s[%d]" at (List.length before))
            depth it.described;
        ]

and any : type a. ctx -> dir -> string -> int -> a S.any -> t =
 fun ctx dir at depth a ->
  let branches =
    List.filter_map Fun.id [ a.bool; a.number; a.string; a.array; a.object_ ]
  in
  (* Branches that are one schema are one. *)
  let distinct =
    List.fold_left
      (fun acc b ->
        let s = walk_ ctx dir at depth b in
        if List.exists (equal s) acc then acc else acc @ [ s ])
      [] branches
  in
  let nullable s = match a.null with Some _ -> Nullable s | None -> s in
  match (distinct, a.null) with
  | [], Some _ -> Null
  (* Nothing reads it: what a request may hold is nothing, and what an
     answer holds is whatever its writing picks; neither is a shape. *)
  | [], None ->
      report ctx at "a value of several sorts with none to read it by, any JSON";
      Any
  | [ s ], _ -> nullable s
  | several, _ -> nullable (Union several)

and props : type o f.
    ctx -> dir -> string -> int -> (o, f) S.fields -> prop list =
 fun ctx dir at depth fields ->
  match fields with
  | S.Build _ | S.Unread -> []
  | S.Cases (prev, _) -> props ctx dir at depth prev
  | S.Mem (prev, m) ->
      let before = props ctx dir at depth prev in
      let shown =
        match (dir, m.access) with
        | Decode, `Read_only | Encode, `Write_only -> false
        | (Decode | Encode), (`Read_write | `Read_only | `Write_only) -> true
      in
      if not shown then before
      else
        let at = at ^ "." ^ m.name in
        (* A member [opt_mem] made reads [null] as its absence and never
           writes one: its answer is the description it was given. *)
        let schema =
          match (m.opt, dir, m.shape) with
          | true, Encode, S.Nullable given -> walk_ ctx dir at depth given
          | _, (Decode | Encode), shape -> walk_ ctx dir at depth shape
        in
        let examples =
          List.filter_map
            (fun v ->
              match Decode.written m.shape v with
              | Ok v -> Some v
              | Error e ->
                  error ctx at Unwritable
                    ("An example cannot be written: " ^ Unwritable.to_string e);
                  None)
            m.examples
        in
        before
        @ [
            {
              name = m.name;
              schema;
              required = Option.is_none m.absent;
              doc = m.doc;
              deprecated = m.deprecated;
              examples;
            };
          ]

and object_ : type o. ctx -> dir -> string -> int -> S.about -> o S.obj -> t =
 fun ctx dir at depth about o ->
  let build () =
    match Encode.find_cases o.fields with
    | None ->
        Object
          {
            about = about.doc;
            props = props ctx dir at depth o.fields;
            additional =
              (match o.unknown with S.Skip -> Allowed | S.Refuse -> Refused);
          }
    | Some (Encode.Packed (S.Tagged c)) ->
        let base = props ctx dir at depth o.fields in
        let value tag =
          match Decode.written c.shape tag with
          | Ok v -> v
          | Error e ->
              error ctx at Unwritable
                ("A case's tag cannot be written: " ^ Unwritable.to_string e);
              Value.Null
        in
        Tagged
          {
            tag = c.name;
            absent = Option.map value c.absent;
            cases =
              List.map
                (fun (S.Case cm) ->
                  ( value cm.tag,
                    {
                      about = cm.case_about.doc;
                      props = base @ props ctx dir at depth cm.obj.fields;
                      additional =
                        (match (o.unknown, cm.obj.unknown) with
                        | S.Skip, S.Skip -> Allowed
                        | S.Refuse, _ | _, S.Refuse -> Refused);
                    } ))
                c.cases;
          }
  in
  if String.equal about.kind "" then build ()
  else
    let base = name_of_kind about.kind in
    if
      not
        (String.length base > 0
        && match base.[0] with 'A' .. 'Z' -> true | _ -> false)
    then
      error ctx at Kind_not_a_name
        (Printf.sprintf
           "The kind %S names no component, since %S does not begin with a \
            letter."
           about.kind base);
    let name =
      match dir with
      | Decode when differs (ref []) depth (S.Object (about, o)) ->
          base ^ "Input"
      | Decode | Encode -> base
    in
    let walked (a, d, n) =
      a == about && String.equal n name
      &&
      match (d, dir) with
      | Decode, Decode | Encode, Encode -> true
      | _ -> false
    in
    if
      List.exists (String.equal name) ctx.walking
      || List.exists walked ctx.walked
    then Ref name
    else begin
      ctx.walking <- name :: ctx.walking;
      let schema = build () in
      ctx.walking <-
        List.filter (fun n -> not (String.equal n name)) ctx.walking;
      register ctx at name schema;
      ctx.walked <- (about, dir, name) :: ctx.walked;
      Ref name
    end

let walk ctx dir ~at t = walk_ ctx dir at 0 t
let components ctx = List.rev ctx.components
let loose ctx = List.sort_uniq String.compare ctx.loose

let errors ctx =
  List.sort_uniq
    (fun a b -> String.compare (error_to_string a) (error_to_string b))
    ctx.errors

(* A case's tag is one of its members in both printers, pinned to its value,
   and optional only in the case the tag stands for when it is left out. *)
let with_tag tag absent value (o : obj) =
  let optional = Option.equal Value.equal absent (Some value) in
  {
    o with
    props =
      {
        name = tag;
        schema = Any;
        required = not optional;
        doc = "";
        deprecated = false;
        examples = [];
      }
      :: List.filter (fun p -> not (String.equal p.name tag)) o.props;
  }

(* ------------------------------------------------------------------ *)
(* JSON Schema *)

module Json_schema = struct
  let obj members = Value.Object members
  let str s = Value.String s
  let typed name = [ ("type", str name) ]
  let number f = Value.Number f

  let described doc rest =
    if String.equal doc "" then rest else ("description", str doc) :: rest

  let some key f = function None -> [] | Some x -> [ (key, f x) ]

  let bounds (b : number) =
    some "minimum" number b.min
    @ some "maximum" number b.max
    @ some "exclusiveMinimum" number b.above
    @ some "exclusiveMaximum" number b.below
    @ some "multipleOf" number b.multiple_of

  let format = function
    | None -> []
    | Some S.Date_time -> [ ("format", str "date-time") ]
    | Some S.Date -> [ ("format", str "date") ]
    | Some S.Duration ->
        [
          ("format", str "duration"); ("pattern", str Grammar.duration_pattern);
        ]
    | Some (S.Uuid None) -> [ ("format", str "uuid") ]
    | Some (S.Uuid (Some v)) ->
        [ ("format", str "uuid"); ("pattern", str (Grammar.uuid_pattern v)) ]
    | Some S.Base64 -> [ ("contentEncoding", str "base64") ]
    | Some S.Base64url -> [ ("contentEncoding", str "base64url") ]
    | Some S.Uri -> [ ("format", str "uri") ]
    | Some S.Ipv4 -> [ ("format", str "ipv4") ]
    | Some S.Ipv6 -> [ ("format", str "ipv6") ]

  let length min max =
    some "minLength" (fun n -> number (float_of_int n)) min
    @ some "maxLength" (fun n -> number (float_of_int n)) max

  let rec of_t ?(defs = "#/$defs/") = function
    | Any -> obj []
    | Null -> obj (typed "null")
    | Boolean -> obj (typed "boolean")
    | Number b -> obj (typed "number" @ bounds b)
    | Integer b -> obj (typed "integer" @ bounds b)
    | String s ->
        obj
          (typed "string"
          @ some "enum" (fun ws -> Value.Array (List.map str ws)) s.words
          @ length s.min_length s.max_length
          @ format s.format)
    | Array a ->
        obj
          (typed "array"
          @ [ ("items", of_t ~defs a.items) ]
          @ some "minItems" (fun n -> number (float_of_int n)) a.min_items
          @ some "maxItems" (fun n -> number (float_of_int n)) a.max_items)
    | Tuple ts ->
        let n = number (float_of_int (List.length ts)) in
        obj
          (typed "array"
          @ [
              ("prefixItems", Value.Array (List.map (of_t ~defs) ts));
              ("minItems", n);
              ("maxItems", n);
            ])
    | Dict d ->
        obj
          (typed "object"
          @ [ ("additionalProperties", of_t ~defs d.values) ]
          @ (match d.keys with
            (* Every name is a string already. *)
            | String
                {
                  words = None;
                  min_length = None;
                  max_length = None;
                  format = None;
                } ->
                []
            | keys -> [ ("propertyNames", of_t ~defs keys) ])
          @ some "minProperties"
              (fun n -> number (float_of_int n))
              d.min_properties
          @ some "maxProperties"
              (fun n -> number (float_of_int n))
              d.max_properties)
    | Object o -> obj (members ~defs o [])
    | Ref n -> obj [ ("$ref", str (defs ^ n)) ]
    | Nullable t ->
        obj [ ("anyOf", Value.Array [ of_t ~defs t; obj (typed "null") ]) ]
    | Union ts -> obj [ ("anyOf", Value.Array (List.map (of_t ~defs) ts)) ]
    | Tagged { tag; absent; cases } ->
        obj
          [
            ( "oneOf",
              Value.Array
                (List.map
                   (fun (value, o) ->
                     obj
                       (members ~defs
                          (with_tag tag absent value o)
                          [ (tag, obj [ ("const", value) ]) ]))
                   cases) );
          ]

  (* An object's members; [fixed] are members whose schema is written already
     -- a case's tag, pinned to its value. *)
  and members ~defs (o : obj) fixed =
    described o.about
      (typed "object"
      @ [
          ( "properties",
            obj
              (List.map
                 (fun p ->
                   match List.assoc_opt p.name fixed with
                   | Some s -> (p.name, s)
                   | None -> (
                       let extra =
                         (if String.equal p.doc "" then []
                          else [ ("description", str p.doc) ])
                         @ (if p.deprecated then
                              [ ("deprecated", Value.Bool true) ]
                            else [])
                         @
                         match p.examples with
                         | [] -> []
                         | vs -> [ ("examples", Value.Array vs) ]
                       in
                       match (of_t ~defs p.schema, extra) with
                       | s, [] -> (p.name, s)
                       | Value.Object fields, extra ->
                           (p.name, Value.Object (fields @ extra))
                       | s, _ -> (p.name, s)))
                 o.props) );
        ]
      @ (match List.filter (fun p -> p.required) o.props with
        | [] -> []
        | req ->
            [ ("required", Value.Array (List.map (fun p -> str p.name) req)) ])
      @
      match o.additional with
      | Allowed -> []
      | Refused -> [ ("additionalProperties", Value.Bool false) ]
      | Each a -> [ ("additionalProperties", of_t ~defs a) ])

  let document components root =
    let root =
      match of_t root with Value.Object members -> members | _ -> []
    in
    Value.Object
      ((("$schema", str "https://json-schema.org/draft/2020-12/schema") :: root)
      @
      match components with
      | [] -> []
      | _ :: _ ->
          [ ("$defs", obj (List.map (fun (n, s) -> (n, of_t s)) components)) ])
end

(* ------------------------------------------------------------------ *)
(* zod *)

module Zod = struct
  let quote = Text.quote

  let is_identifier s =
    String.length s > 0
    && (match s.[0] with
      | 'a' .. 'z' | 'A' .. 'Z' | '_' | '$' -> true
      | _ -> false)
    && String.for_all
         (function
           | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '$' -> true
           | _ -> false)
         s

  (* [__proto__], bare or quoted, sets an object literal's prototype, and only
     a computed key makes it a member. *)
  let key s =
    if String.equal s "__proto__" then "[" ^ quote s ^ "]"
    else if is_identifier s then s
    else quote s

  (* A comment's text may not close it. *)
  let comment indent doc =
    if String.equal doc "" then ""
    else
      let b = Buffer.create (String.length doc) in
      String.iteri
        (fun i c ->
          if Char.equal c '/' && i > 0 && Char.equal doc.[i - 1] '*' then
            Buffer.add_string b " /"
          else Buffer.add_char b c)
        doc;
      Printf.sprintf "%s/** %s */\n" indent (Buffer.contents b)

  let prop_comment (p : prop) =
    match (p.deprecated, p.doc) with
    | false, doc -> doc
    | true, "" -> "@deprecated"
    | true, doc -> "@deprecated " ^ doc

  (* Whether a schema refers to one of [names]: a component not yet written
     out, which is only ever itself or one it is recursive with. *)
  let rec refers names = function
    | Ref n -> List.mem n names
    | Array { items = t; _ } | Nullable t -> refers names t
    | Union ts | Tuple ts -> List.exists (refers names) ts
    | Dict { keys; values; _ } -> refers names keys || refers names values
    | Object o -> refers_in names o
    | Tagged { cases; _ } -> List.exists (fun (_, o) -> refers_in names o) cases
    | Any | Null | Boolean | Number _ | Integer _ | String _ -> false

  and refers_in names (o : obj) =
    List.exists (fun p -> refers names p.schema) o.props
    ||
    match o.additional with
    | Allowed | Refused -> false
    | Each a -> refers names a

  let num f = Value.to_string (Value.Number f)

  let checked base checks =
    match checks with
    | [] -> base
    | _ -> base ^ ".check(" ^ String.concat ", " checks ^ ")"

  let some f = function None -> [] | Some x -> [ f x ]

  (* zod's multipleOf allows a few rounding errors, as a number's multiple
     does, but an integer's is exact, and JavaScript's [%] is exact on every
     integer [z.int()] takes. *)
  let bounds ~multiple (b : number) =
    some (fun m -> "z.gte(" ^ num m ^ ")") b.min
    @ some (fun m -> "z.lte(" ^ num m ^ ")") b.max
    @ some (fun m -> "z.gt(" ^ num m ^ ")") b.above
    @ some (fun m -> "z.lt(" ^ num m ^ ")") b.below
    @ some multiple b.multiple_of

  let lengths min max =
    some (fun n -> Printf.sprintf "z.minLength(%d)" n) min
    @ some (fun n -> Printf.sprintf "z.maxLength(%d)" n) max

  (* zod has no bound on how many members a record has. *)
  let properties min max =
    some
      (fun n -> Printf.sprintf "z.refine((o) => Object.keys(o).length >= %d)" n)
      min
    @ some
        (fun n ->
          Printf.sprintf "z.refine((o) => Object.keys(o).length <= %d)" n)
        max

  let string_ (s : string_) =
    let base, extra =
      match (s.words, s.format) with
      | Some words, _ ->
          ("z.enum([" ^ String.concat ", " (List.map quote words) ^ "])", [])
      | None, None -> ("z.string()", [])
      | None, Some S.Date_time -> ("z.iso.datetime({ offset: true })", [])
      | None, Some S.Date -> ("z.iso.date()", [])
      | None, Some S.Duration ->
          ("z.iso.duration()", [ "z.regex(/" ^ Grammar.duration_pattern ^ "/)" ])
      | None, Some (S.Uuid None) -> ("z.uuid()", [])
      | None, Some (S.Uuid (Some v)) ->
          ( Printf.sprintf "z.uuid({ version: \"v%d\" })"
              (Grammar.uuid_version_number v),
            [] )
      | None, Some S.Base64 -> ("z.base64()", [])
      | None, Some S.Base64url -> ("z.base64url()", [])
      | None, Some S.Uri -> ("z.url()", [])
      | None, Some S.Ipv4 -> ("z.ipv4()", [])
      | None, Some S.Ipv6 -> ("z.ipv6()", [])
    in
    checked base (extra @ lengths s.min_length s.max_length)

  (* [forward] are the components not yet written out where this schema is
     printed: a member referring to one is a getter, which is how zod takes
     a reference to a schema that comes later, or to the one it is in. *)
  let rec expr ?(indent = "") ?(forward = []) = function
    | Any -> "z.unknown()"
    | Null -> "z.null()"
    | Boolean -> "z.boolean()"
    | Number b ->
        checked "z.number()"
          (bounds b ~multiple:(fun m -> "z.multipleOf(" ^ num m ^ ")"))
    | Integer b ->
        checked "z.int()"
          (bounds b ~multiple:(fun m ->
               "z.refine((n) => n % " ^ num m ^ " === 0)"))
    | String s -> string_ s
    | Array a ->
        checked
          ("z.array(" ^ expr ~indent ~forward a.items ^ ")")
          (lengths a.min_items a.max_items)
    | Tuple ts ->
        "z.tuple(["
        ^ String.concat ", " (List.map (expr ~indent ~forward) ts)
        ^ "])"
    | Dict d ->
        (* A record keyed by an enum wants every word; a map has the ones it
           has. *)
        let record =
          match d.keys with
          | String { words = Some _; _ } -> "z.partialRecord("
          | _ -> "z.record("
        in
        checked
          (record
          ^ expr ~indent ~forward d.keys
          ^ ", "
          ^ expr ~indent ~forward d.values
          ^ ")")
          (properties d.min_properties d.max_properties)
    | Object o -> obj indent forward o []
    | Ref n -> n ^ "Schema"
    | Nullable t -> "z.nullable(" ^ expr ~indent ~forward t ^ ")"
    | Union ts ->
        "z.union(["
        ^ String.concat ", " (List.map (expr ~indent ~forward) ts)
        ^ "])"
    | Tagged { tag; absent; cases } ->
        Printf.sprintf "z.discriminatedUnion(%s, [\n%s%s])" (quote tag)
          (String.concat ""
             (List.map
                (fun (value, o) ->
                  let literal = "z.literal(" ^ Value.to_string value ^ ")" in
                  let literal =
                    if Option.equal Value.equal absent (Some value) then
                      "z.optional(" ^ literal ^ ")"
                    else literal
                  in
                  indent ^ "  "
                  ^ obj (indent ^ "  ") forward
                      (with_tag tag absent value o)
                      [ (tag, literal) ]
                  ^ ",\n")
                cases))
          indent

  (* An object's members; [fixed] are members whose schema is written already
     -- a case's tag, pinned to its value. *)
  and obj indent forward (o : obj) fixed =
    let inner = indent ^ "  " in
    let members =
      List.map
        (fun p ->
          let doc = comment inner (prop_comment p) in
          match List.assoc_opt p.name fixed with
          | Some s -> doc ^ inner ^ key p.name ^ ": " ^ s ^ ",\n"
          | None ->
              let s = expr ~indent:inner ~forward p.schema in
              let s = if p.required then s else "z.optional(" ^ s ^ ")" in
              if refers forward p.schema then
                Printf.sprintf "%s%sget %s() {\n%s  return %s;\n%s},\n" doc
                  inner (key p.name) inner s inner
              else doc ^ inner ^ key p.name ^ ": " ^ s ^ ",\n")
        o.props
    in
    let object_ make =
      make ^ "({\n" ^ String.concat "" members ^ indent ^ "})"
    in
    match (o.props, o.additional) with
    | [], Each a -> "z.record(z.string(), " ^ expr ~indent ~forward a ^ ")"
    | [], Allowed -> "z.object({})"
    | [], Refused -> "z.strictObject({})"
    | _ :: _, Allowed -> object_ "z.object"
    | _ :: _, Refused -> object_ "z.strictObject"
    (* The members it names and whatever else, as JSON Schema's
       additionalProperties says beside them. *)
    | _ :: _, Each a ->
        "z.catchall(" ^ object_ "z.object" ^ ", " ^ expr ~indent ~forward a
        ^ ")"

  let of_t ?indent t = expr ?indent t

  let components cs =
    let rec go = function
      | [] -> []
      | (name, s) :: rest ->
          let forward = name :: List.map fst rest in
          ((match s with Object o -> comment "" o.about | _ -> "")
          ^ Printf.sprintf
              "export const %sSchema = %s;\n\
               export type %s = z.infer<typeof %sSchema>;\n\n"
              name (expr ~forward s) name name)
          :: go rest
    in
    String.concat "" (go cs)
end
