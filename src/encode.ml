(* Writing a value by its description, minified. A description that cannot
   write a value -- one made to read only, a kind whose value is outside what
   it can spell -- is a failure the caller is handed, never a raise past this
   module. *)

module S = Shape

exception Unwritable of string

let fail fmt = Printf.ksprintf (fun m -> raise (Unwritable m)) fmt

(* The one union an object may have, found where it was declared. *)
type 'o packed = Packed : ('o, 'c) S.cases -> 'o packed

let rec find_cases : type o f. (o, f) S.fields -> o packed option = function
  | S.Build _ | S.Unread -> None
  | S.Mem (prev, _) -> find_cases prev
  | S.Cases (prev, c) -> (
      match find_cases prev with Some p -> Some p | None -> Some (Packed c))

let rec value : type a. Buffer.t -> a S.t -> a -> unit =
 fun b shape v ->
  match shape with
  | S.Null _ -> Buffer.add_string b "null"
  | S.Bool -> Buffer.add_string b (if v then "true" else "false")
  | S.Int _ -> Buffer.add_string b (string_of_int v)
  | S.Int64 _ -> Buffer.add_string b (Int64.to_string v)
  | S.Number _ -> Text.number b v
  | S.String _ -> Text.string b v
  | S.Enum (_, e) -> Text.string b (e.word v)
  | S.List l ->
      Buffer.add_char b '[';
      List.iteri
        (fun i x ->
          if i > 0 then Buffer.add_char b ',';
          value b l.elt x)
        v;
      Buffer.add_char b ']'
  | S.Tuple items ->
      Buffer.add_char b '[';
      ignore (parts b items v true : bool);
      Buffer.add_char b ']'
  | S.Dict d ->
      (* A name written twice is a document no reader here would take. *)
      let names = Hashtbl.create 8 and name = Buffer.create 16 in
      Buffer.add_char b '{';
      List.iteri
        (fun i (k, x) ->
          Buffer.clear name;
          value name d.key k;
          let n = Buffer.contents name in
          if not (String.length n > 0 && Char.equal n.[0] '"') then
            fail "a map's name must be written as text, and %s is not" n;
          if Hashtbl.mem names n then fail "the name %s is written twice" n;
          Hashtbl.add names n ();
          if i > 0 then Buffer.add_char b ',';
          Buffer.add_string b n;
          Buffer.add_char b ':';
          value b d.value x)
        v;
      Buffer.add_char b '}'
  | S.Nullable t -> (
      match v with None -> Buffer.add_string b "null" | Some x -> value b t x)
  | S.Object (_, o) ->
      Buffer.add_char b '{';
      ignore (members b o v true : bool);
      Buffer.add_char b '}'
  | S.Any (about, a) -> (
      match a.pick with
      | Some f -> value b (f v) v
      | None -> fail "%s is read and never written" (named about "a value"))
  | S.Map (about, m) -> (
      match m.enc with
      | None -> fail "%s is read and never written" (named about "a value")
      | Some f -> (
          match f v with Ok x -> value b m.inner x | Error e -> fail "%s" e))
  | S.Value -> Value.write b v
  | S.Rec l -> value b (Lazy.force l) v

and parts : type t f. Buffer.t -> (t, f) S.items -> t -> bool -> bool =
 fun b items v first ->
  match items with
  | S.Items _ -> first
  | S.Item (prev, it) ->
      let first = parts b prev v first in
      if not first then Buffer.add_char b ',';
      value b it.described (it.part v);
      false

and named (about : S.about) default =
  if String.equal about.kind "" then default else about.kind

(* The members, after [first] says whether any has been written: a union's
   tag first, then the object's own, then the case's, as a reader expects
   the tag before what it decides. *)
and members : type o. Buffer.t -> o S.obj -> o -> bool -> bool =
 fun b o v first ->
  match find_cases o.fields with
  | None -> base b o.fields v first
  | Some (Packed (S.Tagged t)) ->
      let c =
        match t.enc with
        | Some f -> f v
        | None -> fail "the member %S is read and never written" t.name
      in
      let (S.Case_value (cm, k)) = t.enc_case c in
      let omitted = match t.omit with Some f -> f cm.tag | None -> false in
      let first =
        if omitted then first else member b first t.name t.shape cm.tag
      in
      let first = base b o.fields v first in
      members b cm.obj k first

and base : type o f. Buffer.t -> (o, f) S.fields -> o -> bool -> bool =
 fun b fields v first ->
  match fields with
  | S.Build _ | S.Unread -> first
  | S.Cases (prev, _) -> base b prev v first
  | S.Mem (prev, m) -> (
      let first = base b prev v first in
      match m.get with
      | None -> fail "the member %S is read and never written" m.name
      | Some enc ->
          let x = enc v in
          let omitted = match m.omit with Some f -> f x | None -> false in
          if omitted then first else member b first m.name m.shape x)

and member : type a. Buffer.t -> bool -> string -> a S.t -> a -> bool =
 fun b first name shape x ->
  if not first then Buffer.add_char b ',';
  Text.string b name;
  Buffer.add_char b ':';
  value b shape x;
  false

(* Small enough for the minor heap: most answers are, and a buffer on the
   major heap per encode was once most of what the collector did under
   load. *)
let run shape v =
  let b = Buffer.create 1024 in
  match value b shape v with
  | () -> Ok (Buffer.contents b)
  | exception Unwritable m -> Error m
