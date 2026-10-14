(** The grammars behind the ready-made kinds. Each accepts no string the
    browser's Zod check for it refuses; each refusal is a sentence. *)

val instant_of_string : string -> (Ptime.t, string) result
(** RFC 3339 [date-time] as Zod's [iso.datetime({ offset: true })] reads it: a
    fraction past the millisecond is dropped, and an instant outside the years
    0000 to 9999 once in UTC, which Ptime does not hold, is refused. *)

val instant_to_string : Ptime.t -> string
(** [YYYY-MM-DDTHH:MM:SS.sssZ], a fraction past the millisecond dropped. *)

val date_of_string : string -> (int * int * int, string) result
val date_to_string : int * int * int -> (string, string) result

val duration_of_string : string -> (Mtime.Span.t, string) result
(** ISO 8601, with no years or months, to the millisecond; a day is 24 hours.
    One longer than an [Mtime.Span.t] holds, about 584 years, is refused. *)

val duration_to_string : Mtime.Span.t -> string
(** A fraction past the millisecond dropped. *)

val duration_pattern : string
val uuid_version_number : Shape.uuid_version -> int
val uuid_pattern : Shape.uuid_version -> string

val uuid_of_string :
  ?version:Shape.uuid_version -> string -> (Uuidm.t, string) result
(** In either case. *)

val base64_of_string : url:bool -> string -> (string, string) result
val base64_to_string : url:bool -> string -> string
val uri_of_string : string -> (string, string) result
val ipv4_of_string : string -> (string, string) result
val ipv6_of_string : string -> (string, string) result
