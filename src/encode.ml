(* Writing a value by its description, minified. A value that cannot be
   written -- by a description made to read only, by a kind it is outside, or
   as a document no reader would take: text that is not UTF-8, a member given
   twice, nesting past what the reader reads -- stops the writing at its
   place, and is handed to the caller, never raised past this module. *)

module S = Shape
module P = Problem
module U = Unwritable

let max_depth = 512

exception Stop of U.t

type path = P.segment list (* innermost first *)

let fail (path : path) code fmt =
  Printf.ksprintf
    (fun message -> raise (Stop { U.at = List.rev path; code; message }))
    fmt

(* The one union an object may have, found where it was declared. *)
type 'o packed = Packed : ('o, 'c) S.cases -> 'o packed

let rec find_cases : type o f. (o, f) S.fields -> o packed option = function
  | S.Build _ | S.Unread -> None
  | S.Mem (prev, _) -> find_cases prev
  | S.Cases (prev, c) -> (
      match find_cases prev with Some p -> Some p | None -> Some (Packed c))

(* RFC 8259 §8.1: a document is UTF-8. *)
let text b path s =
  if String.is_valid_utf_8 s then Text.string b s
  else fail path U.Not_utf8 "This text is not UTF-8."

let enter path depth =
  if depth >= max_depth then
    fail path U.Too_deep "This is nested more than %d deep." max_depth

let named (about : S.about) =
  if String.equal about.kind "" then "value" else about.kind

let rec value : type a. Buffer.t -> path -> int -> a S.t -> a -> unit =
 fun b path depth shape v ->
  match shape with
  | S.Null _ -> Buffer.add_string b "null"
  | S.Bool -> Buffer.add_string b (if v then "true" else "false")
  | S.Int _ -> Buffer.add_string b (string_of_int v)
  | S.Int64 _ -> Buffer.add_string b (Int64.to_string v)
  | S.Number _ ->
      if Float.is_finite v then Text.number b v
      else fail path U.Unspellable "%s is no JSON number." (Float.to_string v)
  | S.String _ -> text b path v
  | S.Enum (_, e) ->
      let w = e.word v in
      if List.exists (fun (w', _) -> String.equal w w') e.words then
        text b path w
      else fail path U.Unspellable "%S is not one of this enum's words." w
  | S.List l ->
      enter path depth;
      Buffer.add_char b '[';
      List.iteri
        (fun i x ->
          if i > 0 then Buffer.add_char b ',';
          value b (P.Index i :: path) (depth + 1) l.elt x)
        v;
      Buffer.add_char b ']'
  | S.Tuple items ->
      enter path depth;
      Buffer.add_char b '[';
      ignore (parts b path depth items v : int);
      Buffer.add_char b ']'
  | S.Dict d ->
      enter path depth;
      (* An entry is said by its place in the list: its name is what could
         not be written, or written twice. *)
      let names = Hashtbl.create 8 and name = Buffer.create 16 in
      Buffer.add_char b '{';
      List.iteri
        (fun i (k, x) ->
          let at = P.Index i :: path in
          Buffer.clear name;
          value name at depth d.key k;
          let n = Buffer.contents name in
          if not (String.length n > 0 && Char.equal n.[0] '"') then
            fail at U.Name_not_text
              "A map's name is written as text, and this one as %s." n;
          if Hashtbl.mem names n then
            fail at U.Repeated_member "The name %s is written twice." n;
          Hashtbl.add names n ();
          if i > 0 then Buffer.add_char b ',';
          Buffer.add_string b n;
          Buffer.add_char b ':';
          value b at (depth + 1) d.value x)
        v;
      Buffer.add_char b '}'
  | S.Nullable t -> (
      match v with
      | None -> Buffer.add_string b "null"
      | Some x -> value b path depth t x)
  | S.Object (_, o) ->
      enter path depth;
      Buffer.add_char b '{';
      ignore (members b path depth (Hashtbl.create 8) o v true : bool);
      Buffer.add_char b '}'
  | S.Any (about, a) -> (
      match a.pick with
      | Some f -> value b path depth (f v) v
      | None ->
          fail path U.Read_only "This %s is read and never written."
            (named about))
  | S.Map (about, m) -> (
      match m.enc with
      | None ->
          fail path U.Read_only "This %s is read and never written."
            (named about)
      | Some f -> (
          match f v with
          | Ok x -> value b path depth m.inner x
          | Error e -> fail path U.Unspellable "%s" e))
  | S.Value -> json b path depth v
  | S.Rec l -> value b path depth (Lazy.force l) v

(* Any JSON, held to what a reader takes as any description is. *)
and json b path depth (v : Value.t) =
  match v with
  | Null | Bool _ | Number _ -> Value.write b v
  | String s -> text b path s
  | Array items ->
      enter path depth;
      Buffer.add_char b '[';
      List.iteri
        (fun i x ->
          if i > 0 then Buffer.add_char b ',';
          json b (P.Index i :: path) (depth + 1) x)
        items;
      Buffer.add_char b ']'
  | Object members ->
      enter path depth;
      let names = Hashtbl.create 8 in
      Buffer.add_char b '{';
      List.iteri
        (fun i (name, x) ->
          let at = P.Member name :: path in
          if Hashtbl.mem names name then
            fail at U.Repeated_member "This member is given more than once.";
          Hashtbl.add names name ();
          if i > 0 then Buffer.add_char b ',';
          text b at name;
          Buffer.add_char b ':';
          json b at (depth + 1) x)
        members;
      Buffer.add_char b '}'

(* The items, each at its index; how many there are so far. *)
and parts : type t f. Buffer.t -> path -> int -> (t, f) S.items -> t -> int =
 fun b path depth items v ->
  match items with
  | S.Items _ -> 0
  | S.Item (prev, it) ->
      let i = parts b path depth prev v in
      if i > 0 then Buffer.add_char b ',';
      value b (P.Index i :: path) (depth + 1) it.described (it.part v);
      i + 1

(* The members, after [first] says whether any has been written: a union's
   tag first, then the object's own, then the case's, as a reader expects
   the tag before what it decides. [names] are those written, since a
   description built by hand may give one twice. *)
and members : type o.
    Buffer.t ->
    path ->
    int ->
    (string, unit) Hashtbl.t ->
    o S.obj ->
    o ->
    bool ->
    bool =
 fun b path depth names o v first ->
  match find_cases o.fields with
  | None -> base b path depth names o.fields v first
  | Some (Packed (S.Tagged t)) ->
      let c =
        match t.enc with
        | Some f -> f v
        | None ->
            fail (P.Member t.name :: path) U.Read_only
              "This member is read and never written."
      in
      let (S.Case_value (cm, k)) = t.enc_case c in
      let at = P.Member t.name :: path in
      let spelled tag =
        let b = Buffer.create 16 in
        value b at depth t.shape tag;
        Buffer.contents b
      in
      let tag = spelled cm.tag in
      if
        not
          (List.exists
             (fun (S.Case c) -> String.equal (spelled c.tag) tag)
             t.cases)
      then fail at U.Unspellable "%s is not one of this union's tags." tag;
      let omitted = match t.omit with Some f -> f cm.tag | None -> false in
      let first =
        if omitted then first
        else member b path depth names first t.name t.shape cm.tag
      in
      let first = base b path depth names o.fields v first in
      members b path depth names cm.obj k first

and base : type o f.
    Buffer.t ->
    path ->
    int ->
    (string, unit) Hashtbl.t ->
    (o, f) S.fields ->
    o ->
    bool ->
    bool =
 fun b path depth names fields v first ->
  match fields with
  | S.Build _ | S.Unread -> first
  | S.Cases (prev, _) -> base b path depth names prev v first
  | S.Mem (prev, m) -> (
      let first = base b path depth names prev v first in
      match m.get with
      | None ->
          fail (P.Member m.name :: path) U.Read_only
            "This member is read and never written."
      | Some enc ->
          let x = enc v in
          let omitted = match m.omit with Some f -> f x | None -> false in
          if omitted then first
          else member b path depth names first m.name m.shape x)

and member : type a.
    Buffer.t ->
    path ->
    int ->
    (string, unit) Hashtbl.t ->
    bool ->
    string ->
    a S.t ->
    a ->
    bool =
 fun b path depth names first name shape x ->
  let at = P.Member name :: path in
  if Hashtbl.mem names name then
    fail at U.Repeated_member "This member is given more than once.";
  Hashtbl.add names name ();
  if not first then Buffer.add_char b ',';
  text b at name;
  Buffer.add_char b ':';
  value b at (depth + 1) shape x;
  false

(* 1024 bytes is 128 words, under the 256 a block may have and still be
   made on the minor heap. *)
let run shape v =
  let b = Buffer.create 1024 in
  match value b [] 0 shape v with
  | () -> Ok (Buffer.contents b)
  | exception Stop e -> Error e
