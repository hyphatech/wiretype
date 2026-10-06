module Value = Value
module Text = Text
module Problem = Problem
module Shape = Shape
module Schema = Schema
module S = Shape

type 'a t = 'a S.t

let decode ?max_depth t s = Decode.run ?max_depth t s
let encode = Encode.run

let to_value t v =
  match Encode.run t v with
  | Error m -> Error m
  | Ok s -> (
      match Decode.run S.Value s with
      | Ok v -> Ok v
      | Error ps -> Error (String.concat " " (List.map Problem.to_string ps)))

let of_value t v = Decode.run t (Value.to_string v)
let about ?(kind = "") ?(doc = "") () : S.about = { kind; doc }

let rec name : type a. a t -> string = function
  | S.Null _ -> "null"
  | S.Bool -> "boolean"
  | S.Int _ | S.Int64 _ -> "integer"
  | S.Number _ -> "number"
  | S.String _ -> "string"
  | S.Enum (about, _) -> or_ about "enum"
  | S.List l -> "list of " ^ name l.elt
  | S.Tuple _ -> "tuple"
  | S.Dict d -> "map of " ^ name d.value
  | S.Nullable t -> name t ^ " or null"
  | S.Object (about, _) -> or_ about "object"
  | S.Any (about, _) -> or_ about "value"
  | S.Map (about, m) ->
      if String.equal about.kind "" then name m.inner else about.kind
  | S.Value -> "JSON"
  | S.Rec l -> name (Lazy.force l)

and or_ (about : S.about) default =
  if String.equal about.kind "" then default else about.kind

let null v = S.Null v
let bool = S.Bool
let int = S.Int { min = None; max = None; multiple_of = None }
let int_bounded ?min ?max ?multiple_of () = S.Int { min; max; multiple_of }
let int64 = S.Int64 { min = None; max = None; multiple_of = None }
let int64_bounded ?min ?max ?multiple_of () = S.Int64 { min; max; multiple_of }

let number =
  S.Number
    { min = None; max = None; above = None; below = None; multiple_of = None }

let number_bounded ?min ?max ?above ?below ?multiple_of () =
  S.Number { min; max; above; below; multiple_of }

let string = S.String { min_length = None; max_length = None }

let string_bounded ?min_length ?max_length () =
  S.String { min_length; max_length }

let enum ?kind ?doc word values =
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
let value = S.Value
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

  let mem ?(doc = "") ?absent ?omit ?enc ?(read_only = false)
      ?(write_only = false) ?(deprecated = false) ?(examples = []) name shape m
      =
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
              read_only;
              write_only;
              deprecated;
              examples;
            } );
    }

  let opt_mem ?(doc = "") ?enc ?(deprecated = false) name shape m =
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
              read_only = false;
              write_only = false;
              deprecated;
              examples = [];
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
  let finish m = S.Object (m.about, { unknown = m.unknown; fields = m.fields })

  module Case = struct
    type ('c, 'k, 'tag) map = ('c, 'k, 'tag) S.case_map

    let map (type k) ?dec tag (obj : k S.t) : (_, k, _) map =
      match obj with
      | S.Object (case_about, o) -> { tag; case_about; obj = o; build = dec }
      | _ -> invalid_arg "Wiretype.Object.Case.map: a case is an object"

    let make m = S.Case m
    let value m k = S.Case_value (m, k)
  end
end
