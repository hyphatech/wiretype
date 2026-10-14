(** [[@@deriving wiretype]]: a type's [Wiretype] description, as the value
    [<name>_json] -- [json] for a type named [t] -- and in a signature its
    [val].

    - A {b record} is an object named by the type (a [t] by its module), each
      field a member; one with parameters is named only by [[@@kind]]. An
      [option] field may be left out or [null] ([Object.opt_mem]). An option of
      an option, or of [unit], is refused wherever it is written -- a field, a
      list's item, a tuple's -- since [None] and [Some None] would both be
      written [null]; [[@default e]] reads a missing member as [e]; [[@key "k"]]
      names it on the wire; [[@with d]] describes it by [d] -- the inner type's,
      on an [option]. [[@min]], [[@max]], [[@multiple_of]] bound an [int],
      [int64] or [float], and [[@above]], [[@below]] a [float]; [[@min_length]],
      [[@max_length]] a [string]; [[@min_items]], [[@max_items]] a [list]; a
      least and a most written as literals with no value between them are
      refused. [[@read_only]] or [[@write_only]] say which of an answer and a
      request it is in, and it is neither read in the other nor written in it: a
      read-only field is never read, so one with no [[@default e]] that is no
      [option] is refused, since nothing would build it.
      [[@wiretype.deprecated]] and [[@examples [...]]] are what a document says
      of it, and a field's doc comment is its doc, as the type's is the
      object's, the enum's or the union's. OCaml's own [[@deprecated]] is
      OCaml's, never a document's: a field or a constructor it marks is derived
      as any other, and what is derived uses it with no alert.
    - A {b variant} of constant constructors is an enum, a word per constructor:
      its name with the first letter lowered, or [[@name "w"]].
    - A {b variant} with arguments is a union on a tag -- ["type"], or
      [[@@tag "k"]], which only a union has -- each case an inline record's
      members, the members of its one argument's object, or none.
    - A {b polymorphic variant} is either, the same way.
    - [[@@rename_all camel]], [kebab] or [snake] (the default) spells every
      member and word from its OCaml name; a trailing [_] after a keyword --
      [as_] -- is dropped. [[@@kind "k"]] names the object.
    - A type {b parameter} ['a] is an argument, its description, in order. A
      type with parameters has no kind unless [[@@kind]] gives it one, so a
      schema writes it where it is used -- ['a page] a page of users in one
      answer and of orders in another -- since a kind names one component, and
      two arguments under one are [Wiretype.Schema.Kind_shared]. A type of the
      group that refers to itself is described through [Wiretype.rec']. A
      recursive type with no parameter is built as its module loads; one with
      parameters is a function, each of whose descriptions is built a level at a
      time as it is first read, written or walked. Either may be shared between
      domains.
    - An [int], [int64], [float], [string] and [bool] are their own
      descriptions, and [unit] is [null]; a [list] and an [array] are a JSON
      array, and an [option] is its type or [null]. An [int32], a [nativeint], a
      [char] and [bytes] have no description, and are refused, saying what to
      write instead. Each of these is the same by its stdlib module's name --
      [String.t], ['a List.t], [Stdlib.Int32.t] -- and the deriver, which reads
      only the name written, reads a module of one of these names declared here
      as the stdlib's: its type is described with [[@with String.json]].
    - A [Ptime.t] is an instant ([Wiretype.instant]), an [Mtime.Span.t] or an
      [Mtime.span] a duration ([Wiretype.duration]), and a [Uuidm.t] a UUID
      ([Wiretype.uuid ()]); a module of one of these names declared here is read
      as Ptime's, Mtime's or Uuidm's.
    - A {b tuple} is a JSON array of exactly its items, each by its own
      description.
    - Any other type [M.t] is described by [M.json], and [u] by [u_json]: a
      [Wiretype.Value.t], by whatever alias it is written, is any JSON.
    - A [(key * value) list] marked [[@dict]] -- on its field, or on the type
      itself -- is a map ([Wiretype.dict]), which [[@min_properties]] and
      [[@max_properties]] bound; unmarked, it is a list of pairs, since which
      one it is cannot be read from the type.

    An attribute spelt [[@wiretype.x]] for no [x] above, or one of these where
    it is not read -- a [[@key]] on a constructor -- is refused, since it would
    say nothing; a short name that is none of these is left to another deriver.

    A function, an object type and a variant constructor with more than one
    argument have no JSON shape here, and neither has a case whose one argument
    is a scalar, a time, a UUID, a list, an array, an option or a tuple, since a
    case is an object's members. Each is refused where it is written, with what
    to write instead -- an inline record, for a case: each refusal is an error
    at its place in the derived code, so an editor shows every one in a file,
    and the rest of the file is still derived. *)
