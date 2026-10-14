module Value = struct
  include Value

  let json = Shape.Value
end

module Text = Text
module Problem = Problem
module Unwritable = Unwritable
module Shape = Shape
module Schema = Schema
module S = Shape

type 'a t = 'a S.t

let decode ?max_depth ?max_problems t s =
  Decode.run ?max_depth ?max_problems t s

let encode = Encode.run
let to_value = Decode.written
let of_value t v = Decode.run t (Value.to_string v)

(* A description is a constant written in source, so one that can mean
   nothing is refused where it is built -- when the program starts, or in
   its first test -- and never on a request. *)
let malformed fn fmt =
  Printf.ksprintf (fun m -> invalid_arg ("Wiretype." ^ fn ^ ": " ^ m)) fmt

let first_repeated words =
  let seen = Hashtbl.create 16 in
  List.find_opt
    (fun w ->
      Hashtbl.mem seen w
      ||
      (Hashtbl.add seen w ();
       false))
    words

let about ?(kind = "") ?(doc = "") () : S.about = { kind; doc }

(* A count -- a length, a number of items or members -- is never negative,
   and its least is at most its most, or no value meets it. *)
let counted fn ~what (low, min) (high, max) =
  List.iter
    (function
      | name, Some n when n < 0 ->
          malformed fn "%s is %d, and a count is never negative" name n
      | _, (Some _ | None) -> ())
    [ (low, min); (high, max) ];
  match (min, max) with
  | Some lo, Some hi when lo > hi ->
      malformed fn "%s is %d and %s %d, which no %s meets" low lo high hi what
  | _ -> ()

(* A description of itself is named once: inside it, a description of
   itself again is "itself", which also ends the walk. *)
let name t =
  let rec name : type a. inside:bool -> a t -> string =
   fun ~inside -> function
     | S.Null _ -> "null"
     | S.Bool -> "boolean"
     | S.Int _ | S.Int64 _ -> "integer"
     | S.Number _ -> "number"
     | S.String _ -> "string"
     | S.Enum (about, _) -> or_ about "enum"
     | S.List l -> "list of " ^ name ~inside l.elt
     | S.Tuple _ -> "tuple"
     | S.Dict d -> "map of " ^ name ~inside d.value
     | S.Nullable t -> name ~inside t ^ " or null"
     | S.Object (about, _) -> or_ about "object"
     | S.Any (about, _) -> or_ about "value"
     | S.Map (about, m) ->
         if String.equal about.kind "" then name ~inside m.inner else about.kind
     | S.Value -> "JSON"
     | S.Rec l ->
         if inside then "itself" else name ~inside:true (Itself.force l)
  and or_ (about : S.about) default =
    if String.equal about.kind "" then default else about.kind
  in
  name ~inside:false t

let null v = S.Null v
let bool = S.Bool
let int = S.Int { min = None; max = None; multiple_of = None }

let int_bounded ?min ?max ?multiple_of () =
  (match multiple_of with
  | Some m when m <= 0 ->
      malformed "int_bounded" "multiple_of is %d, and must be positive" m
  | Some _ | None -> ());
  (match (min, max) with
  | Some lo, Some hi when lo > hi ->
      malformed "int_bounded" "min is %d and max %d, which no integer meets" lo
        hi
  | _ -> ());
  S.Int { min; max; multiple_of }

let int64 = S.Int64 { min = None; max = None; multiple_of = None }

let int64_bounded ?min ?max ?multiple_of () =
  (match multiple_of with
  | Some m when Int64.compare m 0L <= 0 ->
      malformed "int64_bounded" "multiple_of is %Ld, and must be positive" m
  | Some _ | None -> ());
  (match (min, max) with
  | Some lo, Some hi when Int64.compare lo hi > 0 ->
      malformed "int64_bounded" "min is %Ld and max %Ld, which no integer meets"
        lo hi
  | _ -> ());
  S.Int64 { min; max; multiple_of }

let number =
  S.Number
    { min = None; max = None; above = None; below = None; multiple_of = None }

let number_bounded ?min ?max ?above ?below ?multiple_of () =
  (* An infinity every number meets is no bound; one no number meets, or a
     bound that is no number, can mean nothing. *)
  let bound name ~none = function
    | Some b when Float.is_nan b || Float.equal b (Float.neg none) ->
        malformed "number_bounded" "%s is %g, which no number meets" name b
    | Some b when Float.equal b none -> None
    | b -> b
  in
  let min = bound "min" ~none:Float.neg_infinity min
  and max = bound "max" ~none:Float.infinity max
  and above = bound "above" ~none:Float.neg_infinity above
  and below = bound "below" ~none:Float.infinity below in
  (match multiple_of with
  | Some m when not (Float.is_finite m && Float.compare m 0. > 0) ->
      malformed "number_bounded" "multiple_of is %g, and must be positive" m
  | Some _ | None -> ());
  (* A least and a most that no number is between: an exclusive bound meets
     nothing at the other's value. *)
  List.iter
    (fun ((low, lo), (high, hi), exclusive) ->
      match (lo, hi) with
      | Some lo, Some hi
        when Float.compare lo hi > 0 || (exclusive && Float.equal lo hi) ->
          malformed "number_bounded" "%s is %g and %s %g, which no number meets"
            low lo high hi
      | _ -> ())
    [
      (("min", min), ("max", max), false);
      (("min", min), ("below", below), true);
      (("above", above), ("max", max), true);
      (("above", above), ("below", below), true);
    ];
  S.Number { min; max; above; below; multiple_of }

let string = S.String { min_length = None; max_length = None }

let string_bounded ?min_length ?max_length () =
  counted "string_bounded" ~what:"string" ("min_length", min_length)
    ("max_length", max_length);
  S.String { min_length; max_length }

let enum ?kind ?doc word values =
  (match first_repeated (List.map word values) with
  | Some w -> malformed "enum" "two values are written %S" w
  | None -> ());
  S.Enum
    ( about ?kind ?doc (),
      { words = List.map (fun v -> (word v, v)) values; word } )

let list ?min_items ?max_items elt =
  counted "list" ~what:"list" ("min_items", min_items) ("max_items", max_items);
  S.List { elt; min_items; max_items }

let dict ?min_properties ?max_properties ~key ~value () =
  counted "dict" ~what:"map"
    ("min_properties", min_properties)
    ("max_properties", max_properties);
  S.Dict { key; value; min_properties; max_properties }

module Tuple = struct
  type ('t, 'dec) map = ('t, 'dec) S.items

  let map f = S.Items f
  let item described ~enc m = S.Item (m, { described; part = enc })
  let finish m = S.Tuple m
end

let tuple2 a b =
  Tuple.(
    map (fun a b -> (a, b)) |> item a ~enc:fst |> item b ~enc:snd |> finish)

let tuple3 a b c =
  Tuple.(
    map (fun a b c -> (a, b, c))
    |> item a ~enc:(fun (a, _, _) -> a)
    |> item b ~enc:(fun (_, b, _) -> b)
    |> item c ~enc:(fun (_, _, c) -> c)
    |> finish)

let tuple4 a b c d =
  Tuple.(
    map (fun a b c d -> (a, b, c, d))
    |> item a ~enc:(fun (a, _, _, _) -> a)
    |> item b ~enc:(fun (_, b, _, _) -> b)
    |> item c ~enc:(fun (_, _, c, _) -> c)
    |> item d ~enc:(fun (_, _, _, d) -> d)
    |> finish)

(* Whether a description writes [null] for a value of its own, which an
   option around it writes for [None] too. A description of itself is
   looked into once, as [name] does: one still being made cannot be, nor
   one reached again with nothing between, and for those the writer refuses
   a value it would write [null] for Some instead. *)
let writes_null t =
  let rec writes : type a. inside:bool -> a t -> bool =
   fun ~inside -> function
     | S.Null _ | S.Nullable _ | S.Value -> true
     | S.Any (_, a) -> Option.is_some a.null
     | S.Map (_, m) -> writes ~inside m.inner
     | S.Rec l -> (
         (not inside)
         &&
         match Itself.force l with
         | t -> writes ~inside:true t
         | exception Lazy.Undefined -> false)
     | S.Bool | S.Int _ | S.Int64 _ | S.Number _ | S.String _ | S.Enum _
     | S.List _ | S.Tuple _ | S.Dict _ | S.Object _ ->
         false
  in
  writes ~inside:false t

(* What a description is called in a refusal, which may be made inside a
   description of itself that cannot yet be named. *)
let spoken t = match name t with n -> n | exception Lazy.Undefined -> "it"

let nullable t =
  if writes_null t then
    malformed "nullable"
      "%s writes null for a value of its own, so None and Some of it would \
       both be written as null; tell them apart with a variant"
      (spoken t);
  S.Nullable t

let rec' l = S.Rec l

let map ?kind ?doc ?dec ?enc inner =
  S.Map
    ( about ?kind ?doc (),
      {
        format = None;
        inner;
        dec = Option.map (fun f x -> Ok (f x)) dec;
        enc = Option.map (fun f x -> Ok (f x)) enc;
      } )

let kind ~name ?doc ?format ~parse ?print inner =
  S.Map
    ( about ~kind:name ?doc (),
      {
        format;
        inner;
        dec = Some parse;
        enc = Option.map (fun print x -> Ok (print x)) print;
      } )

let any ?kind ?doc ?dec_null ?dec_bool ?dec_number ?dec_string ?dec_array
    ?dec_object ?enc () =
  S.Any
    ( about ?kind ?doc (),
      {
        null = dec_null;
        bool = dec_bool;
        number = dec_number;
        string = dec_string;
        array = dec_array;
        object_ = dec_object;
        pick = enc;
      } )

(* A kind whose writing can fail as well as its reading. Its [format] says
   what it is, so its doc is left to its user. *)
let grammar ~name format ~parse ~print =
  S.Map
    ( about ~kind:name (),
      {
        format = Some format;
        inner = string;
        dec = Some parse;
        enc = Some print;
      } )

let instant =
  grammar ~name:"instant" S.Date_time ~parse:Grammar.instant_of_string
    ~print:(fun t -> Ok (Grammar.instant_to_string t))

let date =
  grammar ~name:"date" S.Date ~parse:Grammar.date_of_string
    ~print:Grammar.date_to_string

let duration =
  grammar ~name:"duration" S.Duration ~parse:Grammar.duration_of_string
    ~print:(fun d -> Ok (Grammar.duration_to_string d))

let uuid ?version () =
  grammar ~name:"uuid" (S.Uuid version) ~parse:(Grammar.uuid_of_string ?version)
    ~print:(fun s -> Grammar.uuid_of_string ?version s)

let base64 =
  grammar ~name:"base64" S.Base64 ~parse:(Grammar.base64_of_string ~url:false)
    ~print:(fun b -> Ok (Grammar.base64_to_string ~url:false b))

let base64url =
  grammar ~name:"base64url" S.Base64url
    ~parse:(Grammar.base64_of_string ~url:true) ~print:(fun b ->
      Ok (Grammar.base64_to_string ~url:true b))

let uri =
  grammar ~name:"uri" S.Uri ~parse:Grammar.uri_of_string
    ~print:Grammar.uri_of_string

let ipv4 =
  grammar ~name:"ipv4" S.Ipv4 ~parse:Grammar.ipv4_of_string
    ~print:Grammar.ipv4_of_string

let ipv6 =
  grammar ~name:"ipv6" S.Ipv6 ~parse:Grammar.ipv6_of_string
    ~print:Grammar.ipv6_of_string

module Object = struct
  type ('o, 'dec) map = {
    about : S.about;
    unknown : S.unknown;
    fields : ('o, 'dec) S.fields;
  }

  let map ?kind ?doc dec =
    { about = about ?kind ?doc (); unknown = S.Skip; fields = S.Build dec }

  let enc_only ?kind ?doc () =
    { about = about ?kind ?doc (); unknown = S.Skip; fields = S.Unread }

  let mem ?(doc = "") ?absent ?omit ?enc ?(access = `Read_write)
      ?(deprecated = false) ?(examples = []) name shape m =
    {
      m with
      fields =
        S.Mem
          ( m.fields,
            {
              name;
              doc;
              shape;
              absent;
              omit;
              get = enc;
              opt = false;
              access;
              deprecated;
              examples;
            } );
    }

  let opt_mem ?(doc = "") ?enc ?(access = `Read_write) ?(deprecated = false)
      ?(examples = []) name shape m =
    if writes_null shape then
      malformed "Object.opt_mem"
        "the member %S is over %s, which writes null for a value of its own, \
         so Some of it would be written as null and read back as None; \
         describe it with mem ~absent over that description"
        name (spoken shape);
    {
      m with
      fields =
        S.Mem
          ( m.fields,
            {
              name;
              doc;
              shape = S.Nullable shape;
              absent = Some None;
              omit = Some Option.is_none;
              get = enc;
              opt = true;
              access;
              deprecated;
              examples = List.map Option.some examples;
            } );
    }

  let case_mem ?(doc = "") ?absent ?omit ?enc ~enc_case name shape cases m =
    {
      m with
      fields =
        S.Cases
          ( m.fields,
            S.Tagged { name; doc; shape; absent; omit; enc; enc_case; cases } );
    }

  let error_unknown m = { m with unknown = S.Refuse }

  (* The members' names and the unions' tags, in the order declared: the
     fields hold the last declared outermost. *)
  let names fields =
    let rec go : type o f.
        string list ->
        string list ->
        (o, f) S.fields ->
        string list * string list =
     fun members tags -> function
       | S.Build _ | S.Unread -> (members, tags)
       | S.Mem (prev, m) -> go (m.name :: members) tags prev
       | S.Cases (prev, S.Tagged t) -> go members (t.name :: tags) prev
    in
    go [] [] fields

  (* Whether the object is read: built by a function, not written alone. *)
  let rec read : type o f. (o, f) S.fields -> bool = function
    | S.Build _ -> true
    | S.Unread -> false
    | S.Mem (prev, _) -> read prev
    | S.Cases (prev, _) -> read prev

  (* A member a value is written by leaving out, read where it has no
     [absent], which a reader would call required: the first, if any. *)
  let rec left_out : type o f. (o, f) S.fields -> string option = function
    | S.Build _ | S.Unread -> None
    | S.Mem (prev, m) -> (
        match left_out prev with
        | Some name -> Some name
        | None -> (
            match (m.omit, m.absent) with
            | Some _, None -> Some m.name
            | Some _, Some _ | None, _ -> None))
    | S.Cases (prev, S.Tagged t) -> (
        match left_out prev with
        | Some name -> Some name
        | None -> (
            match (t.omit, t.absent) with
            | Some _, None -> Some t.name
            | Some _, Some _ | None, _ -> None))

  let finish m =
    let members, tags = names m.fields in
    (if read m.fields then
       match left_out m.fields with
       | Some name ->
           malformed "Object.finish"
             "the member %S is left out by omit and has no absent to be read as"
             name
       | None -> ());
    (* An object has one union, since the tag that chooses a case is read
       before the members it decides, and every name is one member's: a
       document could hold it only once. *)
    (match tags with
    | first :: second :: _ ->
        malformed "Object.finish"
          "%S and %S are each a union's tag, and an object has one union" first
          second
    | [] | [ _ ] -> ());
    (match first_repeated (members @ tags) with
    | Some name ->
        malformed "Object.finish" "the member %S is described twice" name
    | None -> ());
    (match Encode.find_cases m.fields with
    | None -> ()
    | Some (Encode.Packed (S.Tagged t)) ->
        let spelled (S.Case cm) =
          match Encode.run t.shape cm.tag with
          | Ok s -> s
          | Error e ->
              malformed "Object.finish" "a tag of the union %S: %s" t.name
                e.message
        in
        (match first_repeated (List.map spelled t.cases) with
        | Some tag ->
            malformed "Object.finish" "two cases of the union %S are tagged %s"
              t.name tag
        | None -> ());
        List.iter
          (fun (S.Case cm) ->
            let own, _ = names cm.obj.fields in
            match first_repeated (members @ tags @ own) with
            | Some name ->
                malformed "Object.finish"
                  "the member %S is described twice: by the object and by its \
                   case %s"
                  name (spelled (S.Case cm))
            | None -> ())
          t.cases);
    (* A record of its own, since a schema takes one [about] for one
       description, and one start may be finished into several. *)
    S.Object
      ( { m.about with kind = m.about.kind },
        { unknown = m.unknown; fields = m.fields } )

  module Case = struct
    type ('c, 'k, 'tag) map = ('c, 'k, 'tag) S.case_map

    (* The object a description is, or what it is instead: one reached
       through [rec'] has been made by now, unless it is the union this case
       is of, which is being made, and no object. *)
    let rec object_of : type k. k S.t -> (S.about * k S.obj, string) result =
      let itself = "the union it is a case of" in
      function
      | S.Object (about, o) -> Ok (about, o)
      | S.Rec l -> (
          match Itself.force l with
          | t -> object_of t
          | exception Lazy.Undefined -> Error itself)
      | t -> (
          match name t with
          | n ->
              Error
                ("described as " ^ n
               ^ "; describe an object, as a constructor's inline record is")
          | exception Lazy.Undefined -> Error itself)

    let map (type k) ?dec tag (obj : k S.t) : (_, k, _) map =
      match object_of obj with
      | Ok (case_about, o) -> (
          match Encode.find_cases o.fields with
          | None -> { tag; case_about; obj = o; build = dec }
          | Some _ ->
              malformed "Object.Case.map"
                "a case is an object with no union of its own, since an object \
                 has one")
      | Error what ->
          malformed "Object.Case.map" "a case is an object, and this one is %s"
            what

    let make m = S.Case m
    let value m k = S.Case_value (m, k)
  end
end
