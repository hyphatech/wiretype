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

and obj = { about : string; props : prop list; additional : t option }

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

(* A value the library wrote, as the JSON it is: a case's tag, an example. *)
let value_of shape v =
  match Encode.run shape v with
  | Error m -> Error m
  | Ok s -> (
      match Decode.run S.Value s with Ok v -> Ok v | Error _ -> Error s)

(* ------------------------------------------------------------------ *)
(* The walk *)

type ctx = {
  mutable components : (string * string * t) list;
      (** name, fingerprint, schema -- newest first *)
  mutable walking : string list;
  mutable loose : string list;
  mutable errors : string list;
}

let create () = { components = []; walking = []; loose = []; errors = [] }

let format_name = function
  | S.Date_time -> "date-time"
  | S.Date -> "date"
  | S.Duration -> "duration"
  | S.Uuid None -> "uuid"
  | S.Uuid (Some v) -> "uuid" ^ string_of_int v
  | S.Base64 -> "base64"
  | S.Base64url -> "base64url"
  | S.Uri -> "uri"
  | S.Ipv4 -> "ipv4"
  | S.Ipv6 -> "ipv6"

let opt f = function None -> "" | Some x -> f x
let num f = Value.to_string (Value.Number f)

let bounds_fingerprint (b : number) =
  opt (fun m -> ">=" ^ num m) b.min
  ^ opt (fun m -> "<=" ^ num m) b.max
  ^ opt (fun m -> ">" ^ num m) b.above
  ^ opt (fun m -> "<" ^ num m) b.below
  ^ opt (fun m -> "%" ^ num m) b.multiple_of

(* A schema as text, which is what two schemas are compared by: equal text,
   one schema. *)
let rec fingerprint = function
  | Any -> "any"
  | Null -> "null"
  | Boolean -> "bool"
  | Number b -> "number" ^ bounds_fingerprint b
  | Integer b -> "int" ^ bounds_fingerprint b
  | String s ->
      (match s.words with
        | None -> "string"
        | Some ws -> "enum(" ^ String.concat "," ws ^ ")")
      ^ opt (fun n -> ">=" ^ string_of_int n) s.min_length
      ^ opt (fun n -> "<=" ^ string_of_int n) s.max_length
      ^ opt (fun f -> "@" ^ format_name f) s.format
  | Array a ->
      "[" ^ fingerprint a.items ^ "]"
      ^ opt (fun n -> ">=" ^ string_of_int n) a.min_items
      ^ opt (fun n -> "<=" ^ string_of_int n) a.max_items
  | Tuple ts -> "<" ^ String.concat "," (List.map fingerprint ts) ^ ">"
  | Dict d ->
      "map{" ^ fingerprint d.keys ^ ":" ^ fingerprint d.values
      ^ opt (fun n -> ">=" ^ string_of_int n) d.min_properties
      ^ opt (fun n -> "<=" ^ string_of_int n) d.max_properties
      ^ "}"
  | Object o -> obj_fingerprint o
  | Ref n -> "&" ^ n
  | Nullable t -> "?" ^ fingerprint t
  | Union ts -> "(" ^ String.concat "|" (List.map fingerprint ts) ^ ")"
  | Tagged { tag; absent; cases } ->
      "<" ^ tag
      ^ opt (fun v -> "?" ^ Value.to_string v) absent
      ^ ":"
      ^ String.concat "|"
          (List.map
             (fun (v, o) -> Value.to_string v ^ "=" ^ obj_fingerprint o)
             cases)
      ^ ">"

and obj_fingerprint o =
  "{"
  ^ String.concat ","
      (List.map
         (fun p ->
           p.name ^ (if p.required then ":" else "?:") ^ fingerprint p.schema)
         o.props)
  ^ (match o.additional with Some a -> ",*:" ^ fingerprint a | None -> "")
  ^ "}"

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

(* A name taken by a different schema is the decoded one's to give up: it is
   named [Input], since an encoded one is what a reader looks up first. *)
let rec register ctx dir name schema =
  let f = fingerprint schema in
  match List.find_opt (fun (n, _, _) -> String.equal n name) ctx.components with
  | None ->
      ctx.components <- (name, f, schema) :: ctx.components;
      name
  | Some (_, f', _) when String.equal f f' -> name
  | Some _ -> (
      match dir with
      | Decode when not (String.ends_with ~suffix:"Input" name) ->
          register ctx dir (name ^ "Input") schema
      | Decode | Encode ->
          ctx.errors <-
            Printf.sprintf
              "two different descriptions share the kind that names %s" name
            :: ctx.errors;
          name)

let of_int = Option.map float_of_int
let of_int64 = Option.map Int64.to_float

(* A recursive description is expanded only this far before it is "any":
   past that point it is a component referring to itself, and nothing more
   is learnt. *)
let max_depth = 3

let rec walk_ : type a. ctx -> dir -> string -> int -> a S.t -> t =
 fun ctx dir at depth shape ->
  match shape with
  | S.Null _ -> Null
  | S.Bool -> Boolean
  | S.Int b ->
      Integer
        {
          no_bounds with
          min = of_int b.min;
          max = of_int b.max;
          multiple_of = of_int b.multiple_of;
        }
  | S.Int64 b ->
      Integer
        {
          no_bounds with
          min = of_int64 b.min;
          max = of_int64 b.max;
          multiple_of = of_int64 b.multiple_of;
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
          ctx.errors <-
            (at ^ ": a map's names are text, and its key is not") :: ctx.errors);
      Dict
        {
          keys;
          values = walk_ ctx dir (at ^ ".*") depth d.value;
          min_properties = d.min_properties;
          max_properties = d.max_properties;
        }
  | S.Nullable t -> (
      match walk_ ctx dir at depth t with
      | Nullable s -> Nullable s
      | s -> Nullable s)
  | S.Object (about, o) -> object_ ctx dir at depth about o
  | S.Any (_, a) -> any ctx dir at depth a
  | S.Map (_, m) -> (
      match (walk_ ctx dir at depth m.inner, m.format) with
      | String s, Some f -> String { s with format = Some f }
      | s, (Some _ | None) -> s)
  | S.Value ->
      report ctx at "any JSON (Wiretype.value), which says nothing of its shape";
      Any
  | S.Rec l ->
      if depth >= max_depth then Any
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
        if
          List.exists
            (fun x -> String.equal (fingerprint x) (fingerprint s))
            acc
        then acc
        else acc @ [ s ])
      [] branches
  in
  let nullable s = match a.null with Some _ -> Nullable s | None -> s in
  match distinct with
  | [] -> Null
  | [ s ] -> nullable s
  | several -> nullable (Union several)

and props : type o f.
    ctx -> dir -> string -> int -> (o, f) S.fields -> prop list =
 fun ctx dir at depth fields ->
  match fields with
  | S.Build _ | S.Unread -> []
  | S.Cases (prev, _) -> props ctx dir at depth prev
  | S.Mem (prev, m) ->
      let before = props ctx dir at depth prev in
      let shown =
        match dir with Decode -> not m.read_only | Encode -> not m.write_only
      in
      if not shown then before
      else
        let schema = walk_ ctx dir (at ^ "." ^ m.name) depth m.shape in
        (* A member [opt_mem] made reads [null] as its absence and never
           writes one. *)
        let schema =
          match (m.opt, dir, schema) with
          | true, Encode, Nullable s -> s
          | (true | false), (Decode | Encode), s -> s
        in
        let examples =
          List.filter_map
            (fun v -> Result.to_option (value_of m.shape v))
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
            additional = None;
          }
    | Some (Encode.Packed (S.Tagged c)) ->
        let base = props ctx dir at depth o.fields in
        let value tag =
          match value_of c.shape tag with
          | Ok v -> v
          | Error m ->
              ctx.errors <- (at ^ ": a case's tag: " ^ m) :: ctx.errors;
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
                      additional = None;
                    } ))
                c.cases;
          }
  in
  if String.equal about.kind "" then build ()
  else
    let name = name_of_kind about.kind in
    if List.exists (String.equal name) ctx.walking then Ref name
    else begin
      ctx.walking <- name :: ctx.walking;
      let schema = build () in
      ctx.walking <-
        List.filter (fun n -> not (String.equal n name)) ctx.walking;
      Ref (register ctx dir name schema)
    end

let walk ctx dir ~at t = walk_ ctx dir at 0 t
let components ctx = List.rev_map (fun (n, _, s) -> (n, s)) ctx.components
let loose ctx = List.sort_uniq String.compare ctx.loose
let errors ctx = List.sort_uniq String.compare ctx.errors

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
    | Some (S.Uuid _) -> [ ("format", str "uuid") ]
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
      | Some a -> [ ("additionalProperties", of_t ~defs a) ]
      | None -> [])

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

  let key s = if is_identifier s then s else quote s

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
    || Option.fold ~none:false ~some:(refers names) o.additional

  let num f = Value.to_string (Value.Number f)

  let checked base checks =
    match checks with
    | [] -> base
    | _ -> base ^ ".check(" ^ String.concat ", " checks ^ ")"

  let some f = function None -> [] | Some x -> [ f x ]

  let bounds (b : number) =
    some (fun m -> "z.gte(" ^ num m ^ ")") b.min
    @ some (fun m -> "z.lte(" ^ num m ^ ")") b.max
    @ some (fun m -> "z.gt(" ^ num m ^ ")") b.above
    @ some (fun m -> "z.lt(" ^ num m ^ ")") b.below
    @ some (fun m -> "z.multipleOf(" ^ num m ^ ")") b.multiple_of

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
          (Printf.sprintf "z.uuid({ version: \"v%d\" })" v, [])
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
    | Number b -> checked "z.number()" (bounds b)
    | Integer b -> checked "z.int()" (bounds b)
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
    let object_ () =
      "z.object({\n" ^ String.concat "" members ^ indent ^ "})"
    in
    match (o.props, o.additional) with
    | [], Some a -> "z.record(z.string(), " ^ expr ~indent ~forward a ^ ")"
    | [], None -> "z.object({})"
    | _ :: _, None -> object_ ()
    (* The members it names and whatever else, as JSON Schema's
       additionalProperties says beside them. *)
    | _ :: _, Some a ->
        "z.catchall(" ^ object_ () ^ ", " ^ expr ~indent ~forward a ^ ")"

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
