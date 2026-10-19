module S = Shape

(* JSON's spellings, before Text names what a schema says of a string. *)
module Json_text = Text

type dir = Decode | Encode

module Number = struct
  type t = {
    min : float option;
    max : float option;
    above : float option;
    below : float option;
    multiple_of : float option;
  }

  let any =
    { min = None; max = None; above = None; below = None; multiple_of = None }
end

module Text = struct
  type t = {
    words : string list option;
    min_length : int option;
    max_length : int option;
    format : S.format option;
  }

  let any =
    { words = None; min_length = None; max_length = None; format = None }
end

type t =
  | Any
  | Null
  | Boolean
  | Number of Number.t
  | Integer of Number.t
  | String of Text.t
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
  | Described of { doc : string; schema : t }

and obj = { about : string; props : prop list; additional : additional }
and additional = Allowed | Refused | Each of t

and prop = {
  name : string;
  field : string option;  (** the OCaml field it is in, where one was given *)
  schema : t;
  required : bool;
  doc : string;
  deprecated : bool;
  examples : Value.t list;
}

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

let number_equal (a : Number.t) (b : Number.t) =
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
  | Described a, Described b ->
      String.equal a.doc b.doc && equal a.schema b.schema
  | Tagged a, Tagged b ->
      String.equal a.tag b.tag
      && Option.equal Value.equal a.absent b.absent
      && List.equal
           (fun (v, o) (w, p) -> Value.equal v w && obj_equal o p)
           a.cases b.cases
  | ( ( Any | Null | Boolean | Number _ | Integer _ | String _ | Array _
      | Tuple _ | Dict _ | Object _ | Ref _ | Nullable _ | Union _ | Tagged _
      | Described _ ),
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

(* A shape's doc, around the schema it is the doc of. *)
let described (about : S.about) schema =
  if String.equal about.doc "" then schema
  else Described { doc = about.doc; schema }

(* What a schema is, under the doc around it. *)
let rec undescribed = function Described d -> undescribed d.schema | s -> s
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

(* A component is walked from its own root, so a recursion through kinds
   ends where one comes back, and only one that makes a new kind at every
   level -- from its depth -- would never come back: past this many kinds
   each inside the last, which no description written by hand nests, what
   is inside is any JSON, and reported. *)
let max_kinds_inside = 100

(* Whether a description is walked differently as a request and as an
   answer: it has a member in one alone, or one [opt_mem] made, which a
   request may give as [null] and an answer never writes. A component's
   name follows from this alone -- a request's is [<Name>Input] exactly
   where it holds -- so it never depends on what else was walked. [seen]
   are the kinds already looked into, each once: what one holds was either
   found to differ, which ended the search, or not. A kind is looked into
   from its own root, as it is walked, and [kinds] are those around it. *)
let rec differs : type a. string list ref -> int -> int -> a S.t -> bool =
 fun seen kinds depth shape ->
  match shape with
  | S.Null _ | S.Bool | S.Int _ | S.Int64 _ | S.Number _ | S.String _ | S.Enum _
  | S.Value ->
      false
  | S.List l -> differs seen kinds depth l.elt
  | S.Tuple items -> items_differ seen kinds depth items
  | S.Dict d ->
      differs seen kinds depth d.key || differs seen kinds depth d.value
  | S.Nullable t -> differs seen kinds depth t
  | S.Map (_, m) -> differs seen kinds depth m.inner
  | S.Any (_, a) ->
      List.exists
        (Option.fold ~none:false ~some:(differs seen kinds depth))
        [ a.null; a.bool; a.number; a.string; a.array; a.object_ ]
  | S.Rec l ->
      depth < max_depth && differs seen kinds (depth + 1) (Itself.force l)
  | S.Object (about, o) ->
      if String.equal about.kind "" then fields_differ seen kinds depth o.fields
      else if
        kinds >= max_kinds_inside || List.exists (String.equal about.kind) !seen
      then false
      else (
        seen := about.kind :: !seen;
        fields_differ seen (kinds + 1) 0 o.fields)

and items_differ : type t f.
    string list ref -> int -> int -> (t, f) S.items -> bool =
 fun seen kinds depth items ->
  match items with
  | S.Items _ -> false
  | S.Item (prev, it) ->
      items_differ seen kinds depth prev
      || differs seen kinds depth it.described

and fields_differ : type o f.
    string list ref -> int -> int -> (o, f) S.fields -> bool =
 fun seen kinds depth fields ->
  match fields with
  | S.Build _ | S.Unread -> false
  | S.Mem (prev, m) ->
      (match m.access with
        | Read_write -> m.opt
        | Read_only | Write_only -> true)
      || differs seen kinds depth m.shape
      || fields_differ seen kinds depth prev
  | S.Cases (prev, S.Tagged t) ->
      differs seen kinds depth t.shape
      || List.exists
           (fun (S.Case cm) -> fields_differ seen kinds depth cm.obj.fields)
           t.cases
      || fields_differ seen kinds depth prev

let rec walk_at_depth : type a. ctx -> dir -> string -> int -> a S.t -> t =
 fun ctx dir at depth shape ->
  match shape with
  | S.Null _ -> Null
  | S.Bool -> Boolean
  | S.Int b ->
      Integer
        {
          Number.any with
          min = of_int ctx at b.min;
          max = of_int ctx at b.max;
          multiple_of = of_int ctx at b.multiple_of;
        }
  | S.Int64 b ->
      Integer
        {
          Number.any with
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
      String
        { Text.any with min_length = l.min_length; max_length = l.max_length }
  | S.Enum (about, e) ->
      described about
        (String { Text.any with words = Some (List.map fst e.words) })
  | S.List l ->
      Array
        {
          items = walk_at_depth ctx dir (at ^ "[]") depth l.elt;
          min_items = l.min_items;
          max_items = l.max_items;
        }
  | S.Tuple items -> Tuple (tuple_items ctx dir at depth items)
  | S.Dict d ->
      let keys = walk_at_depth ctx dir (at ^ "[name]") depth d.key in
      (match undescribed keys with
      | String _ -> ()
      | _ ->
          error ctx at Key_not_text
            "A map's names are text, and its key's description is not.");
      Dict
        {
          keys;
          values = walk_at_depth ctx dir (at ^ ".*") depth d.value;
          min_properties = d.min_properties;
          max_properties = d.max_properties;
        }
  | S.Nullable t ->
      (* [null] once, though a shape built directly nests one option in
         another, and a doc around both, since it is the doc of whatever the
         value is. *)
      let rec nullable = function
        | Nullable _ as s -> s
        | Described d -> Described { d with schema = nullable d.schema }
        | s -> Nullable s
      in
      nullable (walk_at_depth ctx dir at depth t)
  | S.Object (about, o) -> object_ ctx dir at depth about o
  | S.Any (about, a) -> described about (any ctx dir at depth a)
  | S.Map (about, m) ->
      let rec formatted = function
        | Described d -> Described { d with schema = formatted d.schema }
        | String s -> (
            match m.format with
            | Some f -> String { s with format = Some f }
            | None -> String s)
        | s -> s
      in
      described about (formatted (walk_at_depth ctx dir at depth m.inner))
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
      else walk_at_depth ctx dir at (depth + 1) (Itself.force l)

and tuple_items : type v f.
    ctx -> dir -> string -> int -> (v, f) S.items -> t list =
 fun ctx dir at depth items ->
  match items with
  | S.Items _ -> []
  | S.Item (prev, it) ->
      let before = tuple_items ctx dir at depth prev in
      before
      @ [
          walk_at_depth ctx dir
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
        let s = walk_at_depth ctx dir at depth b in
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
        | Decode, S.Read_only | Encode, S.Write_only -> false
        | (Decode | Encode), (S.Read_write | S.Read_only | S.Write_only) -> true
      in
      if not shown then before
      else
        let at = at ^ "." ^ m.name in
        (* A member [opt_mem] made reads [null] as its absence and never
           writes one: its answer is the description it was given. *)
        let schema =
          match (m.opt, dir, m.shape) with
          | true, Encode, S.Nullable given ->
              walk_at_depth ctx dir at depth given
          | _, (Decode | Encode), shape -> walk_at_depth ctx dir at depth shape
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
              field = m.field;
              schema;
              required = Option.is_none m.absent;
              doc = m.doc;
              deprecated = m.deprecated;
              examples;
            };
          ]

and object_ : type o. ctx -> dir -> string -> int -> S.about -> o S.obj -> t =
 fun ctx dir at depth about o ->
  let build depth =
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
        described about
          (Tagged
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
             })
  in
  if String.equal about.kind "" then build depth
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
      | Decode when differs (ref []) 0 0 (S.Object (about, o)) -> base ^ "Input"
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
    else if List.length ctx.walking >= max_kinds_inside then (
      report ctx at
        (Printf.sprintf
           "a kind inside %d others, each inside the last, any JSON from here"
           max_kinds_inside);
      Any)
    else begin
      ctx.walking <- name :: ctx.walking;
      (* A component is walked as a document's root is, so its schema and
         its name are the same wherever it is reached. *)
      let schema = build 0 in
      ctx.walking <-
        List.filter (fun n -> not (String.equal n name)) ctx.walking;
      register ctx at name schema;
      ctx.walked <- (about, dir, name) :: ctx.walked;
      Ref name
    end

let walk ctx dir ~at t = walk_at_depth ctx dir at 0 t
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
        field = None;
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
  let undoc = List.filter (fun (k, _) -> not (String.equal k "description"))

  let bounds (b : Number.t) =
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
    (* The nearest doc to where it is used is the one said. *)
    | Described { doc; schema } -> (
        match of_t ~defs schema with
        | Value.Object fields ->
            Value.Object (undoc fields @ [ ("description", str doc) ])
        | v -> v)
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
                       (* A member's doc says what its shape is there, so it
                          wins over the shape's own. *)
                       match (of_t ~defs p.schema, extra) with
                       | s, [] -> (p.name, s)
                       | Value.Object fields, extra ->
                           let fields =
                             if String.equal p.doc "" then fields
                             else undoc fields
                           in
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
(* Zod *)

module Zod = struct
  let quote = Json_text.quote

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

  (* A member's doc, or its shape's, where a comment stands: no comment stands
     inside an expression, so an item's doc is JSON Schema's alone. *)
  let prop_comment (p : prop) =
    let doc =
      match (p.doc, p.schema) with "", Described d -> d.doc | doc, _ -> doc
    in
    match (p.deprecated, doc) with
    | false, doc -> doc
    | true, "" -> "@deprecated"
    | true, doc -> "@deprecated " ^ doc

  (* Whether a schema refers to one of [names]: a component not yet written
     out, which is only ever itself or one it is recursive with. *)
  let rec refers names = function
    | Ref n -> List.mem n names
    | Array { items = t; _ } | Nullable t | Described { schema = t; _ } ->
        refers names t
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

  (* Zod's multipleOf allows a few rounding errors, as a number's multiple
     does, but an integer's is exact, and JavaScript's [%] is exact on every
     integer [z.int()] takes. *)
  let bounds ~multiple (b : Number.t) =
    some (fun m -> "z.gte(" ^ num m ^ ")") b.min
    @ some (fun m -> "z.lte(" ^ num m ^ ")") b.max
    @ some (fun m -> "z.gt(" ^ num m ^ ")") b.above
    @ some (fun m -> "z.lt(" ^ num m ^ ")") b.below
    @ some multiple b.multiple_of

  let lengths min max =
    some (fun n -> Printf.sprintf "z.minLength(%d)" n) min
    @ some (fun n -> Printf.sprintf "z.maxLength(%d)" n) max

  (* Zod has no bound on how many members a record has. *)
  let properties min max =
    some
      (fun n -> Printf.sprintf "z.refine((o) => Object.keys(o).length >= %d)" n)
      min
    @ some
        (fun n ->
          Printf.sprintf "z.refine((o) => Object.keys(o).length <= %d)" n)
        max

  let text (s : Text.t) =
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
     printed: a member referring to one is a getter, which is how Zod takes
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
    | String s -> text s
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
          match undescribed d.keys with
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
    | Described d -> expr ~indent ~forward d.schema
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
    let zod_object make =
      make ^ "({\n" ^ String.concat "" members ^ indent ^ "})"
    in
    match (o.props, o.additional) with
    | [], Each a -> "z.record(z.string(), " ^ expr ~indent ~forward a ^ ")"
    | [], Allowed -> "z.object({})"
    | [], Refused -> "z.strictObject({})"
    | _ :: _, Allowed -> zod_object "z.object"
    | _ :: _, Refused -> zod_object "z.strictObject"
    (* The members it names and whatever else, as JSON Schema's
       additionalProperties says beside them. *)
    | _ :: _, Each a ->
        "z.catchall(" ^ zod_object "z.object" ^ ", " ^ expr ~indent ~forward a
        ^ ")"

  let of_t ?indent t = expr ?indent t

  let components cs =
    let rec go = function
      | [] -> []
      | (name, s) :: rest ->
          let forward = name :: List.map fst rest in
          ((match s with
             | Object o -> comment "" o.about
             | Described d -> comment "" d.doc
             | _ -> "")
          ^ Printf.sprintf
              "export const %sSchema = %s;\n\
               export type %s = z.infer<typeof %sSchema>;\n\n"
              name (expr ~forward s) name name)
          :: go rest
    in
    String.concat "" (go cs)
end

(* ------------------------------------------------------------------ *)
(* Melange *)

module Melange = struct
  (* ---------------------------------------------------------------- *)
  (* Names *)

  let keywords =
    [
      "and";
      "as";
      "assert";
      "asr";
      "begin";
      "class";
      "constraint";
      "do";
      "done";
      "downto";
      "else";
      "end";
      "exception";
      "external";
      "false";
      "for";
      "fun";
      "function";
      "functor";
      "if";
      "in";
      "include";
      "inherit";
      "initializer";
      "land";
      "lazy";
      "let";
      "lor";
      "lsl";
      "lsr";
      "lxor";
      "match";
      "method";
      "mod";
      "module";
      "mutable";
      "new";
      "nonrec";
      "object";
      "of";
      "open";
      "or";
      "private";
      "rec";
      "sig";
      "struct";
      "then";
      "to";
      "true";
      "try";
      "type";
      "val";
      "virtual";
      "when";
      "while";
      "with";
    ]

  let is_lower c = match c with 'a' .. 'z' -> true | _ -> false
  let is_upper c = match c with 'A' .. 'Z' -> true | _ -> false
  let is_digit c = match c with '0' .. '9' -> true | _ -> false

  (* Each run of characters an identifier cannot hold as one [_]. *)
  let cleaned s =
    let b = Buffer.create (String.length s) in
    let gap = ref false in
    String.iter
      (fun c ->
        if is_lower c || is_upper c || is_digit c || Char.equal c '\'' then (
          if !gap && Buffer.length b > 0 then Buffer.add_char b '_';
          gap := false;
          Buffer.add_char b c)
        else gap := true)
      s;
    Buffer.contents b

  (* [OrderLine] is [order_line], and [HTTPRequest] is [http_request]. *)
  let snake s =
    let b = Buffer.create (String.length s + 4) in
    let n = String.length s in
    String.iteri
      (fun i c ->
        if
          is_upper c && i > 0
          && (is_lower s.[i - 1]
             || is_digit s.[i - 1]
             || (i + 1 < n && is_lower s.[i + 1] && is_upper s.[i - 1]))
        then Buffer.add_char b '_';
        Buffer.add_char b (Char.lowercase_ascii c))
      s;
    Buffer.contents b

  let lower_name s =
    let s = snake (cleaned s) in
    let s = if String.equal s "" then "_" else s in
    let s = if is_digit s.[0] then "_" ^ s else s in
    if List.mem s keywords then s ^ "_" else s

  let name = lower_name

  let upper_name s =
    let s = cleaned s in
    if String.equal s "" then "Empty"
    else if is_lower s.[0] then String.capitalize_ascii s
    else if is_upper s.[0] then s
    else "W" ^ s

  (* A name not yet in [taken]: a twin gets [_2]. *)
  let fresh taken base =
    let rec go i =
      let s = if i = 1 then base else Printf.sprintf "%s_%d" base i in
      if List.mem s !taken then go (i + 1)
      else (
        taken := s :: !taken;
        s)
    in
    go 1

  (* ---------------------------------------------------------------- *)
  (* Literals *)

  let contains ~sub s =
    let n = String.length sub in
    let rec go i =
      i + n <= String.length s
      && (String.equal (String.sub s i n) sub || go (i + 1))
    in
    go 0

  (* A string as Melange reads one for JavaScript, by a delimiter the text
     does not hold. *)
  let lit s =
    let rec pick i =
      let d = if i = 0 then "js" else Printf.sprintf "j%d" i in
      if contains ~sub:("|" ^ d ^ "}") s then pick (i + 1) else d
    in
    let d = pick 0 in
    "{" ^ d ^ "|" ^ s ^ "|" ^ d ^ "}"

  let float_lit f =
    let s = Value.to_string (Value.Number f) in
    "(" ^ (if Float.is_integer f then s ^ "." else s) ^ ")"

  let int_lit n = Printf.sprintf "(%d)" n

  let int_arg name = function
    | None -> ""
    | Some n -> Printf.sprintf " ~%s:%s" name (int_lit n)

  let float_arg name = function
    | None -> ""
    | Some f -> Printf.sprintf " ~%s:%s" name (float_lit f)

  (* Both bounds within 32 bits, which Melange's int holds. *)
  let fits_int (b : Number.t) =
    match (b.min, b.max) with
    | Some lo, Some hi -> lo >= -2147483648. && hi <= 2147483647.
    | _ -> false

  (* A comment's text may not close it. *)
  let comment doc =
    let b = Buffer.create (String.length doc) in
    String.iteri
      (fun i c ->
        if Char.equal c ')' && i > 0 && Char.equal doc.[i - 1] '*' then
          Buffer.add_string b " )"
        else Buffer.add_char b c)
      doc;
    "(* " ^ Buffer.contents b ^ " *)"

  (* [t option] and [t array], with [t] in parentheses where it has
     structure of its own. *)
  let paren ty =
    if
      String.contains ty ' '
      && not
           (String.ends_with ~suffix:" array" ty
           || String.ends_with ~suffix:" option" ty)
    then "(" ^ ty ^ ")"
    else ty

  (* ---------------------------------------------------------------- *)
  (* The printing *)

  (* A type the module declares, in its one recursive group: a record, an
     enum's or a union's variant, or a component's alias. *)
  type decl = { tname : string; tdoc : string; body : string }

  type st = {
    mutable decls : decl list;  (** newest first *)
    types : string list ref;  (** every type name taken *)
  }

  (* What a node of a schema is printed as: its type, and the reader and the
     writer of it, each an expression. *)
  type printed = { ty : string; read : string; write : string }

  (* An object's members, printed for a record or a case. *)
  type members = {
    fields : string;  (** the fields of its type *)
    build : tag:string option -> (string -> string) -> string;
        (** the reading of every member from [o], then [make] applied to the
            record expression once each has read *)
    written : string -> string;  (** each member written from a value *)
    empty : bool;
  }

  (* The sort a union's alternative is read by. *)
  let sort_of = function
    | Boolean -> `Boolean
    | Number _ | Integer _ -> `Number
    | String _ -> `String
    | Array _ | Tuple _ -> `Array
    | Dict _ | Object _ | Ref _ | Tagged _ -> `Object
    | Null -> `Null
    | Any | Nullable _ | Union _ | Described _ -> `Any

  let sort_ctor = function
    | `Boolean -> "Boolean"
    | `Number -> "Number"
    | `String -> "String"
    | `Array -> "Array"
    | `Object -> "Object"
    | `Null -> "Null"
    | `Any -> "Any"

  let sort_words = function
    | `Boolean -> "true or false"
    | `Number -> "a number"
    | `String -> "text"
    | `Array -> "a list"
    | `Object -> "an object"
    | `Null -> "null"
    | `Any -> "anything"

  let sort_pattern = function
    | `Boolean -> "JSONTrue | JSONFalse"
    | `Number -> "JSONNumber _"
    | `String -> "JSONString _"
    | `Array -> "JSONArray _"
    | `Object -> "JSONObject _"
    | `Null -> "JSONNull"
    | `Any -> "_"

  let either = function
    | [] -> "nothing at all"
    | [ w ] -> w
    | ws ->
        let rev = List.rev ws in
        String.concat ", " (List.rev (List.tl rev)) ^ " or " ^ List.hd rev

  let declare st ~hint ~doc body =
    let tname = fresh st.types hint in
    st.decls <- { tname; tdoc = doc; body } :: st.decls;
    tname

  let rec node st ~hint ?(doc = "") t : printed =
    match t with
    | Any -> { ty = "Js.Json.t"; read = "Wire.any"; write = "Wire.w_any" }
    | Null -> { ty = "unit"; read = "Wire.null"; write = "Wire.w_null" }
    | Boolean -> { ty = "bool"; read = "Wire.bool"; write = "Wire.w_bool" }
    | Number b ->
        let args =
          float_arg "min" b.min ^ float_arg "max" b.max
          ^ float_arg "above" b.above ^ float_arg "below" b.below
          ^ float_arg "multiple_of" b.multiple_of
        in
        {
          ty = "float";
          read = "(Wire.number" ^ args ^ " ())";
          write = "(Wire.w_number" ^ args ^ " ())";
        }
    | Integer b when fits_int b ->
        let args =
          int_arg "min" (Option.map Float.to_int b.min)
          ^ int_arg "max" (Option.map Float.to_int b.max)
          ^ int_arg "multiple_of" (Option.map Float.to_int b.multiple_of)
        in
        {
          ty = "int";
          read = "(Wire.int" ^ args ^ " ())";
          write = "(Wire.w_int" ^ args ^ " ())";
        }
    | Integer b ->
        let args =
          float_arg "min" b.min ^ float_arg "max" b.max
          ^ float_arg "multiple_of" b.multiple_of
        in
        {
          ty = "float";
          read = "(Wire.whole" ^ args ^ " ())";
          write = "(Wire.w_whole" ^ args ^ " ())";
        }
    | String { words = Some words; _ } ->
        let taken = ref [] in
        let ctors = List.map (fun w -> (w, fresh taken (upper_name w))) words in
        let tname =
          declare st ~hint ~doc
            (String.concat "" (List.map (fun (_, c) -> " | " ^ c) ctors))
        in
        {
          ty = tname;
          (* A JavaScript string is no pattern in Melange, so a word is
             compared. *)
          read =
            Printf.sprintf "(Wire.enum [|%s|] (fun w -> %sNone))"
              (String.concat "; " (List.map (fun (w, _) -> lit w) ctors))
              (String.concat ""
                 (List.map
                    (fun (w, c) ->
                      Printf.sprintf
                        "if String.equal w %s then Some (%s : %s) else " (lit w)
                        c tname)
                    ctors));
          write =
            Printf.sprintf "(Wire.w_enum (fun (v : %s) -> match v with%s))"
              tname
              (String.concat ""
                 (List.map
                    (fun (w, c) -> Printf.sprintf " | %s -> %s" c (lit w))
                    ctors));
        }
    | String s -> (
        let lengths =
          int_arg "min_length" s.min_length ^ int_arg "max_length" s.max_length
        in
        let f ty r extra =
          {
            ty;
            read = Printf.sprintf "(Wire.%s%s%s ())" r lengths extra;
            write = Printf.sprintf "(Wire.w_%s%s%s ())" r lengths extra;
          }
        in
        match s.format with
        | None -> f "string" "string" ""
        | Some S.Date_time -> f "Instant.t" "instant" ""
        | Some S.Date -> f "int * int * int" "date" ""
        | Some S.Duration -> f "Duration.t" "duration" ""
        | Some (S.Uuid None) -> f "Uuid.t" "uuid" ""
        | Some (S.Uuid (Some v)) ->
            f "Uuid.t" "uuid"
              (Printf.sprintf " ~version:%d" (Grammar.uuid_version_number v))
        | Some S.Base64 -> f "string" "base64" ""
        | Some S.Base64url -> f "string" "base64url" ""
        | Some S.Uri -> f "string" "uri" ""
        | Some S.Ipv4 -> f "string" "ipv4" ""
        | Some S.Ipv6 -> f "string" "ipv6" "")
    | Array a ->
        let elt = node st ~hint a.items in
        let counts =
          int_arg "min_items" a.min_items ^ int_arg "max_items" a.max_items
        in
        {
          ty = paren elt.ty ^ " array";
          read = Printf.sprintf "(Wire.array%s %s)" counts elt.read;
          write = Printf.sprintf "(Wire.w_array%s %s)" counts elt.write;
        }
    | Tuple [] ->
        {
          ty = "unit";
          read =
            "(fun ctx path j -> Option.map (fun _ -> ()) (Wire.tuple 0 ctx \
             path j))";
          write = "(fun _ _ () -> Wire.w_tuple [||])";
        }
    | Tuple ts ->
        let items =
          List.mapi
            (fun i t -> node st ~hint:(Printf.sprintf "%s_%d" hint i) t)
            ts
        in
        let n = List.length items in
        let vars = List.init n (fun i -> Printf.sprintf "i%d" i) in
        let tuple vs =
          if n = 1 then List.hd vs else "(" ^ String.concat ", " vs ^ ")"
        in
        {
          ty =
            (if n = 1 then (List.hd items).ty
             else String.concat " * " (List.map (fun p -> paren p.ty) items));
          read =
            Printf.sprintf
              "(fun ctx path j -> match Wire.tuple %d ctx path j with | None \
               -> None | Some items -> %smatch %s with | %s -> Some %s | _ -> \
               None)"
              n
              (String.concat ""
                 (List.mapi
                    (fun i p ->
                      Printf.sprintf
                        "let i%d = %s ctx (Wire.Index %d :: path) items.(%d) \
                         in "
                        i p.read i i)
                    items))
              (tuple vars)
              (tuple (List.map (fun v -> "Some " ^ v) vars))
              (tuple vars);
          write =
            Printf.sprintf "(fun ctx path %s -> Wire.w_tuple [|%s|])"
              (tuple vars)
              (String.concat "; "
                 (List.mapi
                    (fun i p ->
                      Printf.sprintf "%s ctx (Wire.Index %d :: path) i%d"
                        p.write i i)
                    items));
        }
    | Dict d ->
        let key = node st ~hint:(hint ^ "_key") d.keys in
        let value = node st ~hint:(hint ^ "_value") d.values in
        let counts =
          int_arg "min_properties" d.min_properties
          ^ int_arg "max_properties" d.max_properties
        in
        {
          ty = Printf.sprintf "(%s * %s) array" key.ty value.ty;
          read = Printf.sprintf "(Wire.dict%s %s %s)" counts key.read value.read;
          write =
            Printf.sprintf "(Wire.w_dict%s %s %s)" counts key.write value.write;
        }
    | Object o ->
        record st ~hint ~doc:(if String.equal doc "" then o.about else doc) o
    | Ref n ->
        let n = lower_name n in
        { ty = n; read = "read_" ^ n; write = "write_" ^ n }
    | Nullable t ->
        let inner = node st ~hint t in
        {
          ty = paren inner.ty ^ " option";
          read = "(Wire.nullable " ^ inner.read ^ ")";
          write = "(Wire.w_nullable " ^ inner.write ^ ")";
        }
    | Union ts ->
        let taken = ref [] in
        let arms =
          List.map
            (fun t ->
              let sort = sort_of (undescribed t) in
              let ctor = fresh taken (sort_ctor sort) in
              ( sort,
                ctor,
                node st ~hint:(hint ^ "_" ^ String.lowercase_ascii ctor) t ))
            ts
        in
        let tname =
          declare st ~hint ~doc
            (String.concat ""
               (List.map
                  (fun (_, c, p) -> Printf.sprintf " | %s of %s" c (paren p.ty))
                  arms))
        in
        let is_any = function `Any -> true | _ -> false in
        let known = List.filter (fun (s, _, _) -> not (is_any s)) arms in
        let arm (s, c, p) =
          Printf.sprintf
            " | %s -> Option.map (fun x -> (%s x : %s)) (%s ctx path j)"
            (sort_pattern s) c tname p.read
        in
        let last =
          match List.find_opt (fun (s, _, _) -> is_any s) arms with
          | Some a -> arm a
          | None ->
              Printf.sprintf " | _ -> Wire.mismatch ctx path %s j"
                (lit (either (List.map (fun (s, _, _) -> sort_words s) known)))
        in
        {
          ty = tname;
          read =
            Printf.sprintf
              "(fun ctx path j -> match Js.Json.classify j with%s%s)"
              (String.concat "" (List.map arm known))
              last;
          write =
            Printf.sprintf "(fun ctx path (v : %s) -> match v with%s)" tname
              (String.concat ""
                 (List.map
                    (fun (_, c, p) ->
                      Printf.sprintf " | %s x -> %s ctx path x" c p.write)
                    arms));
        }
    | Tagged { tag; absent; cases } ->
        let taken = ref [] in
        let cases =
          List.map
            (fun (value, o) ->
              let word =
                match value with Value.String s -> s | v -> Value.to_string v
              in
              let ctor = fresh taken (upper_name word) in
              let text = Value.to_string value in
              let json =
                match value with
                | Value.String s -> "Js.Json.string " ^ lit s
                | Value.Number f -> "Js.Json.number " ^ float_lit f
                | Value.Bool b -> Printf.sprintf "Js.Json.boolean %b" b
                | Value.Null -> "Js.Json.null"
                | Value.Array _ | Value.Object _ ->
                    "Js.Json.parseExn " ^ lit text
              in
              ( ctor,
                text,
                json,
                members st ~hint:(hint ^ "_" ^ String.lowercase_ascii ctor) o ))
            cases
        in
        let tname =
          declare st ~hint ~doc
            (String.concat ""
               (List.map
                  (fun (ctor, _, _, m) ->
                    if m.empty then " | " ^ ctor
                    else Printf.sprintf " | %s of {%s }" ctor m.fields)
                  cases))
        in
        let absent_arg =
          match absent with
          | None -> "None"
          | Some v -> "(Some " ^ lit (Value.to_string v) ^ ")"
        in
        {
          ty = tname;
          read =
            Printf.sprintf
              "(fun ctx path j -> match Wire.members ctx path j with | None -> \
               None | Some o -> (match Wire.tag ctx path o %s ~absent:%s with \
               | None -> None | Some t -> %sWire.unknown_tag ctx path %s \
               [%s]))"
              (lit tag) absent_arg
              (String.concat ""
                 (List.map
                    (fun (ctor, text, _, m) ->
                      Printf.sprintf "if String.equal t %s then (%s) else "
                        (lit text)
                        (m.build ~tag:(Some tag) (fun built ->
                             Printf.sprintf "(%s : %s)"
                               (if m.empty then ctor else ctor ^ " " ^ built)
                               tname)))
                    cases))
              (lit tag)
              (String.concat "; "
                 (List.map (fun (_, text, _, _) -> lit text) cases));
          write =
            Printf.sprintf "(fun ctx path (v : %s) -> match v with%s)" tname
              (String.concat ""
                 (List.map
                    (fun (ctor, _, json, m) ->
                      let tag_member =
                        Printf.sprintf "Some (%s, Some (%s))" (lit tag) json
                      in
                      if m.empty then
                        Printf.sprintf " | %s -> Wire.w_object [|%s|]" ctor
                          tag_member
                      else
                        Printf.sprintf " | %s r -> Wire.w_object [|%s; %s|]"
                          ctor tag_member (m.written "r"))
                    cases));
        }
    | Described d ->
        node st ~hint ~doc:(if String.equal doc "" then d.doc else doc) d.schema

  and members st ~hint (o : obj) : members =
    let taken = ref [] in
    let members =
      List.map
        (fun (p : prop) ->
          let fname =
            fresh taken (lower_name (Option.value p.field ~default:p.name))
          in
          (* A member that may be left out and may be null reads both as
             nothing, as [opt_mem] does: one option. *)
          let nullable_optional, schema =
            match (p.required, undescribed p.schema) with
            | false, Nullable inner -> (true, inner)
            | _, _ -> (false, p.schema)
          in
          let printed = node st ~hint:(hint ^ "_" ^ fname) schema in
          let doc =
            match (p.deprecated, p.doc) with
            | false, doc -> doc
            | true, "" -> "Deprecated."
            | true, doc -> "Deprecated. " ^ doc
          in
          (p, fname, printed, nullable_optional, doc))
        o.props
    in
    let refused =
      match o.additional with Refused -> true | Allowed | Each _ -> false
    in
    let fields =
      String.concat ""
        (List.map
           (fun ((p : prop), fname, printed, _, doc) ->
             Printf.sprintf " %s : %s;%s" fname
               (if p.required then printed.ty else paren printed.ty ^ " option")
               (if String.equal doc "" then "" else " " ^ comment doc))
           members)
    in
    let build ~tag make =
      let reads =
        String.concat ""
          (List.map
             (fun ((p : prop), fname, printed, nullable_optional, _) ->
               Printf.sprintf "let m_%s = Wire.%s %s ctx path o %s in " fname
                 (if p.required then "required"
                  else if nullable_optional then "optional_or_null"
                  else "optional")
                 printed.read (lit p.name))
             members)
      in
      let check =
        if not refused then ""
        else
          let names =
            List.map (fun ((p : prop), _, _, _, _) -> lit p.name) members
          in
          Printf.sprintf "Wire.unknown ctx path o [|%s|]; "
            (String.concat "; "
               (match tag with Some t -> lit t :: names | None -> names))
      in
      match members with
      | [] -> check ^ "Some " ^ make "()"
      | _ ->
          Printf.sprintf "%s%smatch (%s) with | (%s) -> Some (%s) | _ -> None"
            reads check
            (String.concat ", "
               (List.map (fun (_, f, _, _, _) -> "m_" ^ f) members))
            (String.concat ", "
               (List.map (fun (_, f, _, _, _) -> "Some m_" ^ f) members))
            (make
               (Printf.sprintf "{%s }"
                  (String.concat ""
                     (List.map
                        (fun (_, f, _, _, _) ->
                          Printf.sprintf " %s = m_%s;" f f)
                        members))))
    in
    let written v =
      String.concat "; "
        (List.map
           (fun ((p : prop), fname, printed, _, _) ->
             if p.required then
               Printf.sprintf "Some (%s, %s ctx (Wire.Member %s :: path) %s.%s)"
                 (lit p.name) printed.write (lit p.name) v fname
             else
               Printf.sprintf
                 "(match %s.%s with None -> None | Some x -> Some (%s, %s ctx \
                  (Wire.Member %s :: path) x))"
                 v fname (lit p.name) printed.write (lit p.name))
           members)
    in
    { fields; build; written; empty = List.is_empty members }

  (* An object as a record; one with no members as [unit]. *)
  and record st ~hint ~doc (o : obj) : printed =
    let m = members st ~hint o in
    if m.empty then
      let refused =
        match o.additional with Refused -> true | Allowed | Each _ -> false
      in
      {
        ty = "unit";
        read =
          Printf.sprintf
            "(fun ctx path j -> match Wire.members ctx path j with | None -> \
             None | Some %s -> %sSome ())"
            (if refused then "o" else "_")
            (if refused then "Wire.unknown ctx path o [||]; " else "");
        write = "(fun _ _ () -> Wire.w_object [||])";
      }
    else
      let tname = declare st ~hint ~doc (Printf.sprintf " {%s }" m.fields) in
      {
        ty = tname;
        read =
          Printf.sprintf
            "(fun ctx path j -> match Wire.members ctx path j with | None -> \
             None | Some o -> %s)"
            (m.build ~tag:None (fun built ->
                 Printf.sprintf "(%s : %s)" built tname));
        write =
          Printf.sprintf "(fun ctx path (v : %s) -> Wire.w_object [|%s|])" tname
            (m.written "v");
      }

  let components
      ?(header =
        "Printed by wiretype from the server's descriptions. Do not edit.") cs =
    let st = { decls = []; types = ref [] } in
    let names = List.map (fun (n, _) -> lower_name n) cs in
    (* Every component's name is taken first, so nothing printed inside one
       takes another's; a component then declares its own under its name,
       or is an alias of what it printed as. *)
    st.types := names;
    let printed =
      List.map2
        (fun (_, s) n ->
          st.types := List.filter (fun t -> not (String.equal t n)) !(st.types);
          let p = node st ~hint:n s in
          if not (List.mem n !(st.types)) then st.types := n :: !(st.types);
          (n, p))
        cs names
    in
    let aliases =
      List.filter_map
        (fun (n, p) ->
          if List.exists (fun d -> String.equal d.tname n) st.decls then None
          else Some { tname = n; tdoc = ""; body = " " ^ p.ty })
        printed
    in
    let b = Buffer.create 65536 in
    Buffer.add_string b ("(* " ^ header ^ " *)\n\n");
    Buffer.add_string b Melange_prelude.text;
    Buffer.add_string b
      "\n\
       (* ---- The types ---- *)\n\n\
       (* One recursive group, since components hold each other. Two may share a\n\
      \   field's or a word's name, and a value is told apart by its type. *)\n\
       [@@@warning \"-30\"]\n\n";
    List.iteri
      (fun i d ->
        if not (String.equal d.tdoc "") then
          Buffer.add_string b (comment d.tdoc ^ "\n");
        Buffer.add_string b
          (Printf.sprintf "%s %s =%s\n\n"
             (if i = 0 then "type" else "and")
             d.tname d.body))
      (List.rev st.decls @ aliases);
    Buffer.add_string b "(* ---- Reading ---- *)\n\n";
    List.iteri
      (fun i (n, p) ->
        Buffer.add_string b
          (Printf.sprintf
             "%s read_%s : %s Wire.reader = fun ctx path j -> %s ctx path j\n\n"
             (if i = 0 then "let rec" else "and")
             n n p.read))
      printed;
    Buffer.add_string b "(* ---- Writing ---- *)\n\n";
    List.iteri
      (fun i (n, p) ->
        Buffer.add_string b
          (Printf.sprintf
             "%s write_%s : %s Wire.writer = fun ctx path v -> %s ctx path v\n\n"
             (if i = 0 then "let rec" else "and")
             n n p.write))
      printed;
    Buffer.add_string b "(* ---- Each component ---- *)\n\n";
    List.iter
      (fun (n, _) ->
        Buffer.add_string b
          (Printf.sprintf
             "let decode_%s s = Wire.decode read_%s s\n\
              let %s_of_json j = Wire.of_json read_%s j\n\
              let encode_%s v = Wire.encode write_%s v\n\
              let %s_to_json v = Wire.to_json write_%s v\n\n"
             n n n n n n n n))
      printed;
    Buffer.contents b
end
