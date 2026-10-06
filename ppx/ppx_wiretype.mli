(** [[@@deriving wiretype]]: a type's [Wiretype] description, as the value
    [<name>_json] -- [json] for a type named [t] -- and in a signature its
    [val].

    - A {b record} is an object named by the type (a [t] by its module), each
      field a member. An [option] field may be left out or [null]
      ([Object.opt_mem]); [[@default e]] reads a missing member as [e];
      [[@key "k"]] names it on the wire; [[@with d]] describes it by [d] -- the
      inner type's, on an [option]. [[@min]], [[@max]], [[@multiple_of]] bound
      an [int], [int64] or [float]; [[@min_length]], [[@max_length]] a [string];
      [[@min_items]], [[@max_items]] a [list]. [[@read_only]] or
      [[@write_only]], [[@wiretype.deprecated]] and [[@examples [...]]] are what
      a document says of it, and a field's doc comment is its doc, as the type's
      is the object's.
    - A {b variant} of constant constructors is an enum, a word per constructor:
      its name with the first letter lowered, or [[@name "w"]].
    - A {b variant} with arguments is a union on a tag -- ["type"], or
      [[@@tag "k"]] -- each case an inline record's members, the members of its
      one argument's object, or none.
    - A {b polymorphic variant} is either, the same way.
    - [[@@rename_all camel]], [kebab] or [snake] (the default) spells every
      member and word from its OCaml name; a trailing [_] after a keyword --
      [as_] -- is dropped. [[@@kind "k"]] names the object.
    - A type {b parameter} ['a] is an argument [a_json]; a type of the group
      that refers to itself is described through [Wiretype.rec'].
    - A {b tuple} is a JSON array of exactly its items, each by its own
      description.
    - Any other type [M.t] is described by [M.json], and [u] by [u_json]: a
      [Wiretype.Value.t], by whatever alias it is written, is any JSON.
    - A [(key * value) list] marked [[@dict]] -- on its field, or on the type
      itself -- is a map ([Wiretype.dict]), which [[@min_properties]] and
      [[@max_properties]] bound; unmarked, it is a list of pairs, since which
      one it is cannot be read from the type.

    A function, an object type and a variant constructor with more than one
    argument have no JSON shape here, and are refused where they are written,
    with what to write instead: each refusal is an error at its place in the
    derived code, so an editor shows every one in a file, and the rest of the
    file is still derived. *)
