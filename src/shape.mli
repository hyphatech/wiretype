(** What a description is: the representation {!Wiretype}'s combinators make,
    and everything that reads a description walks -- the decoder, the encoder,
    the schema printers, and anybody with a use of their own.

    It is the library's API and its promise: a constructor added is a new
    version, so a walk that matches every case is told by the compiler. A
    description is built with the combinators in {!Wiretype}; one built here
    directly is as good, since everything that reads one checks what it reads.
*)

type about = { kind : string; doc : string }
(** What a description is called -- a component's name, where it is an object --
    and what it is, for whoever documents it. Empty when it has neither. *)

type int_bounds = {
  min : int option;
  max : int option;
  multiple_of : int option;
}

type int64_bounds = {
  min : int64 option;
  max : int64 option;
  multiple_of : int64 option;
}

type number_bounds = {
  min : float option;  (** at least *)
  max : float option;  (** at most *)
  above : float option;  (** greater than *)
  below : float option;  (** less than *)
  multiple_of : float option;
}

type length = { min_length : int option; max_length : int option }
(** In code points, as JSON Schema counts a string's length. *)

type uuid_version = [ `V1 | `V2 | `V3 | `V4 | `V5 | `V6 | `V7 | `V8 ]
(** The versions RFC 9562 defines. *)

(** What a string is written in, where a schema has a word for it. *)
type format =
  | Date_time  (** RFC 3339 [date-time] *)
  | Date  (** RFC 3339 [full-date] *)
  | Duration  (** ISO 8601, with no years or months *)
  | Uuid of uuid_version option
      (** RFC 9562, of one version where it names one *)
  | Base64  (** RFC 4648 §4 *)
  | Base64url  (** RFC 4648 §5 *)
  | Uri  (** RFC 3986 *)
  | Ipv4
  | Ipv6

(** What an object does with a member it does not describe. *)
type unknown = Skip | Refuse

type access =
  [ `Read_write  (** in requests and answers *)
  | `Read_only  (** in answers alone: the server's to give *)
  | `Write_only  (** in requests alone: a password *) ]
(** Which of a request and an answer a member is in, as OpenAPI's [readOnly] and
    [writeOnly] say. *)

type 'a enum = {
  words : (string * 'a) list;
  word : 'a -> string;  (** the word a value is written as *)
}

type 'a t =
  | Null : 'a -> 'a t  (** JSON [null], read as the value *)
  | Bool : bool t
  | Int : int_bounds -> int t
  | Int64 : int64_bounds -> int64 t
  | Number : number_bounds -> float t
  | String : length -> string t
  | Enum : about * 'a enum -> 'a t
  | List : 'a elements -> 'a list t
  | Tuple : ('a, 'a) items -> 'a t
      (** a JSON array of exactly its items, each of its own description *)
  | Dict : ('k, 'v) dict -> ('k * 'v) list t
      (** a JSON object read as a map: its members, in the document's order *)
  | Nullable : 'a t -> 'a option t  (** the description, or [null] *)
  | Object : about * 'o obj -> 'o t
  | Any : about * 'a any -> 'a t
  | Map : about * ('a, 'b) map -> 'b t
  | Value : Value.t t  (** any JSON *)
  | Rec : 'a t Lazy.t -> 'a t  (** a description of itself *)

and 'a elements = { elt : 'a t; min_items : int option; max_items : int option }

and ('k, 'v) dict = {
  key : 'k t;
      (** a member's name, read as the JSON string it is written as, and written
          as one *)
  value : 'v t;
  min_properties : int option;
  max_properties : int option;
}

(** What a tuple is made of, in order: a function, and each item it is applied
    to, as an object is its members. *)
and ('t, 'f) items =
  | Items : 'f -> ('t, 'f) items
  | Item : ('t, 'a -> 'f) items * ('t, 'a) item -> ('t, 'f) items

and ('t, 'a) item = {
  described : 'a t;
  part : 't -> 'a;  (** the item of a value *)
}

and 'o obj = { unknown : unknown; fields : ('o, 'o) fields }
(** An object: the function that builds its value, applied to its members. *)

(** What an object is made of, in the order it was declared: a function, and
    each member it is applied to. *)
and ('o, 'f) fields =
  | Build : 'f -> ('o, 'f) fields
  | Unread : ('o, 'f) fields
      (** an object that is written and never read: an answer to somebody else's
          server *)
  | Mem : ('o, 'a -> 'f) fields * ('o, 'a) mem -> ('o, 'f) fields
  | Cases : ('o, 'c -> 'f) fields * ('o, 'c) cases -> ('o, 'f) fields
      (** the members that depend on one member's value: a union by its tag *)

and ('o, 'a) mem = {
  name : string;
  doc : string;
  shape : 'a t;
  absent : 'a option;  (** what it reads as when left out; required if none *)
  omit : ('a -> bool) option;
      (** whether a value is written by leaving it out *)
  get : ('o -> 'a) option;  (** the member of a value; none reads only *)
  opt : bool;
      (** made by {!Wiretype.Object.opt_mem}: [null] or absent is [None], and
          [None] is left out *)
  access : access;
  deprecated : bool;
  examples : 'a list;
}

and ('o, 'c) cases =
  | Tagged : {
      name : string;  (** the tag's member *)
      doc : string;
      shape : 'tag t;
      absent : 'tag option;  (** the tag a value with none is read as *)
      omit : ('tag -> bool) option;
      enc : ('o -> 'c) option;
      enc_case : 'c -> ('c, 'tag) case_value;
      cases : ('c, 'tag) case list;
    }
      -> ('o, 'c) cases

and ('c, 'tag) case = Case : ('c, 'k, 'tag) case_map -> ('c, 'tag) case

and ('c, 'k, 'tag) case_map = {
  tag : 'tag;
  case_about : about;  (** the case's object's *)
  obj : 'k obj;  (** the members this case adds *)
  build : ('k -> 'c) option;
      (** none where the case is written and never read *)
}

and ('c, 'tag) case_value =
  | Case_value : ('c, 'k, 'tag) case_map * 'k -> ('c, 'tag) case_value

and 'a any = {
  null : 'a t option;
  bool : 'a t option;
  number : 'a t option;
  string : 'a t option;
  array : 'a t option;
  object_ : 'a t option;
  pick : ('a -> 'a t) option;  (** the description a value is written by *)
}
(** A value that is one of several JSON sorts, a description per sort. *)

and ('a, 'b) map = {
  format : format option;
  inner : 'a t;
  dec : ('a -> ('b, string) result) option;
      (** [Error] is a sentence for a person: what is wrong with the value *)
  enc : ('b -> ('a, string) result) option;
}
(** A description of another type, over [inner]: a kind -- an instant, an id --
    or a plain change of type. A direction left out is one it is never used in.
*)
