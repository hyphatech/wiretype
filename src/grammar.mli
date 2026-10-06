(** The grammars behind the ready-made kinds. Each accepts no string the
    browser's zod check for it refuses; each refusal is a sentence. *)

val instant_of_string : string -> (int, string) result
(** RFC 3339 [date-time] as zod's [iso.datetime({ offset: true })] reads it, to
    epoch milliseconds: a fraction past the millisecond is dropped. *)

val instant_to_string : int -> (string, string) result
(** [YYYY-MM-DDTHH:MM:SS.sssZ]; [Error] outside the years 0000 to 9999. *)

val date_of_string : string -> (int * int * int, string) result
val date_to_string : int * int * int -> (string, string) result

val duration_of_string : string -> (int, string) result
(** ISO 8601, with no years or months, to milliseconds; a day is 24 hours. *)

val duration_to_string : int -> (string, string) result
val duration_pattern : string

val uuid_of_string : ?version:int -> string -> (string, string) result
(** Lower-cased. *)

val base64_of_string : url:bool -> string -> (string, string) result
val base64_to_string : url:bool -> string -> string
val uri_of_string : string -> (string, string) result
val ipv4_of_string : string -> (string, string) result
val ipv6_of_string : string -> (string, string) result
