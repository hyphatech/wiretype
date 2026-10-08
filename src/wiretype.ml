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

let decode ?max_depth t s = Decode.run ?max_depth t s
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
     | S.Rec l -> if inside then "itself" else name ~inside:true (Lazy.force l)
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
  S.Int { min; max; multiple_of }

let int64 = S.Int64 { min = None; max = None; multiple_of = None }

let int64_bounded ?min ?max ?multiple_of () =
  (match multiple_of with
  | Some m when Int64.compare m 0L <= 0 ->
      malformed "int64_bounded" "multiple_of is %Ld, and must be positive" m
  | Some _ | None -> ());
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
  S.Number { min; max; above; below; multiple_of }

let string = S.String { min_length = None; max_length = None }

let string_bounded ?min_length ?max_length () =
  S.String { min_length; max_length }

let enum ?kind ?doc word values =
  (match first_repeated (List.map word values) with
  | Some w -> malformed "enum" "two values are written %S" w
  | None -> ());
  S.Enum
    ( about ?kind ?doc (),
      { words = List.map (fun v -> (word v, v)) values; word } )

let list ?min_items ?max_items elt = S.List { elt; min_items; max_items }

let dict ?min_properties ?max_properties key value =
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

let nullable t = S.Nullable t
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

(* A kind whose writing can fail as well as its reading. *)
let grammar ~name ~doc format ~parse ~print =
  S.Map
    ( about ~kind:name ~doc (),
      {
        format = Some format;
        inner = string;
        dec = Some parse;
        enc = Some print;
      } )

let instant =
  grammar ~name:"instant" ~doc:"An instant, as RFC 3339 writes one." S.Date_time
    ~parse:Grammar.instant_of_string ~print:Grammar.instant_to_string

let date =
  grammar ~name:"date" ~doc:"A date, as YYYY-MM-DD." S.Date
    ~parse:Grammar.date_of_string ~print:Grammar.date_to_string

let duration =
  grammar ~name:"duration" ~doc:"A duration, as ISO 8601 writes one." S.Duration
    ~parse:Grammar.duration_of_string ~print:Grammar.duration_to_string

let uuid ?version () =
  grammar ~name:"uuid" ~doc:"A UUID." (S.Uuid version)
    ~parse:(Grammar.uuid_of_string ?version) ~print:(fun s ->
      Grammar.uuid_of_string ?version s)

let base64 =
  grammar ~name:"base64" ~doc:"Bytes, in base64." S.Base64
    ~parse:(Grammar.base64_of_string ~url:false) ~print:(fun b ->
      Ok (Grammar.base64_to_string ~url:false b))

let base64url =
  grammar ~name:"base64url" ~doc:"Bytes, in base64url." S.Base64url
    ~parse:(Grammar.base64_of_string ~url:true) ~print:(fun b ->
      Ok (Grammar.base64_to_string ~url:true b))

let uri =
  grammar ~name:"uri" ~doc:"A URI." S.Uri ~parse:Grammar.uri_of_string
    ~print:Grammar.uri_of_string

let ipv4 =
  grammar ~name:"ipv4" ~doc:"An IPv4 address." S.Ipv4
    ~parse:Grammar.ipv4_of_string ~print:Grammar.ipv4_of_string

let ipv6 =
  grammar ~name:"ipv6" ~doc:"An IPv6 address." S.Ipv6
    ~parse:Grammar.ipv6_of_string ~print:Grammar.ipv6_of_string

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

  (* An object has one union, since the tag that chooses a case is read
     before the members it decides, and every name is one member's: a
     document could hold it only once. *)
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

    (* The object a description is: one reached through [rec'] has been
       made by now, unless it is the union this case is of, which is being
       made, and no object. *)
    let rec object_of : type k. k S.t -> (S.about * k S.obj) option = function
      | S.Object (about, o) -> Some (about, o)
      | S.Rec l -> (
          match Lazy.force l with
          | t -> object_of t
          | exception Lazy.Undefined -> None)
      | _ -> None

    let map (type k) ?dec tag (obj : k S.t) : (_, k, _) map =
      match object_of obj with
      | Some (case_about, o) -> (
          match Encode.find_cases o.fields with
          | None -> { tag; case_about; obj = o; build = dec }
          | Some _ ->
              malformed "Object.Case.map"
                "a case is an object with no union of its own, since an object \
                 has one")
      | None -> malformed "Object.Case.map" "a case is an object"

    let make m = S.Case m
    let value m k = S.Case_value (m, k)
  end
end
