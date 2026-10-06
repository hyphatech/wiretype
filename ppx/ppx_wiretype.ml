open Ppxlib
module B = Ast_builder.Default

(* ------------------------------------------------------------------ *)
(* Attributes *)

let string_payload = Ast_pattern.(single_expr_payload (estring __))
let expr_payload = Ast_pattern.(single_expr_payload __)

let label name payload =
  Attribute.declare ("wiretype." ^ name) Attribute.Context.label_declaration
    payload Fun.id

let flag name =
  Attribute.declare_flag ("wiretype." ^ name)
    Attribute.Context.label_declaration

module A = struct
  let key = label "key" string_payload
  let default = label "default" expr_payload
  let with_ = label "with" expr_payload

  let ct_with =
    Attribute.declare "wiretype.with" Attribute.Context.core_type expr_payload
      Fun.id

  let min = label "min" expr_payload
  let max = label "max" expr_payload
  let above = label "above" expr_payload
  let below = label "below" expr_payload
  let multiple_of = label "multiple_of" expr_payload
  let min_length = label "min_length" expr_payload
  let max_length = label "max_length" expr_payload
  let min_items = label "min_items" expr_payload
  let max_items = label "max_items" expr_payload
  let min_properties = label "min_properties" expr_payload
  let max_properties = label "max_properties" expr_payload
  let dict = flag "dict"

  let ct_dict =
    Attribute.declare_flag "wiretype.dict" Attribute.Context.core_type

  let examples = label "examples" expr_payload
  let read_only = flag "read_only"
  let write_only = flag "write_only"
  let deprecated = flag "deprecated"

  let cd_name =
    Attribute.declare "wiretype.name" Attribute.Context.constructor_declaration
      string_payload Fun.id

  let rtag_name =
    Attribute.declare "wiretype.name" Attribute.Context.rtag string_payload
      Fun.id

  let tag =
    Attribute.declare "wiretype.tag" Attribute.Context.type_declaration
      string_payload Fun.id

  let kind =
    Attribute.declare "wiretype.kind" Attribute.Context.type_declaration
      string_payload Fun.id

  let rename_all =
    Attribute.declare "wiretype.rename_all" Attribute.Context.type_declaration
      Ast_pattern.(single_expr_payload (pexp_ident (lident __)))
      Fun.id
end

(* A doc comment is an [ocaml.doc] attribute holding its text. *)
let doc_of attrs =
  List.find_map
    (fun a ->
      if String.equal a.attr_name.txt "ocaml.doc" then
        Ast_pattern.parse_res
          Ast_pattern.(pstr (pstr_eval (estring __) nil ^:: nil))
          a.attr_loc a.attr_payload Fun.id
        |> Result.to_option |> Option.map String.trim
      else None)
    attrs

(* ------------------------------------------------------------------ *)
(* Names *)

let keywords =
  [
    "and";
    "as";
    "assert";
    "begin";
    "class";
    "constraint";
    "do";
    "done";
    "downto";
    "else";
    "end";
    "exception";
    "external";
    "false";
    "for";
    "fun";
    "function";
    "functor";
    "if";
    "in";
    "include";
    "inherit";
    "initializer";
    "lazy";
    "let";
    "match";
    "method";
    "module";
    "mutable";
    "new";
    "nonrec";
    "object";
    "of";
    "open";
    "or";
    "private";
    "rec";
    "sig";
    "struct";
    "then";
    "to";
    "true";
    "try";
    "type";
    "val";
    "virtual";
    "when";
    "while";
    "with";
  ]

type spelling = Snake | Camel | Kebab

let spelling_of ~loc = function
  | "snake" -> Snake
  | "camel" -> Camel
  | "kebab" -> Kebab
  | other ->
      Location.raise_errorf ~loc
        "wiretype: [@@rename_all %s]: write camel, kebab or snake" other

(* An OCaml name as its word on the wire: a keyword's trailing [_] dropped,
   and the words between underscores spelt as asked. *)
let spell spelling name =
  let n = String.length name in
  let name =
    if
      n > 1
      && name.[n - 1] = '_'
      && List.mem (String.sub name 0 (n - 1)) keywords
    then String.sub name 0 (n - 1)
    else name
  in
  match spelling with
  | Snake -> name
  | Kebab -> String.concat "-" (String.split_on_char '_' name)
  | Camel -> (
      match
        List.filter
          (fun w -> not (String.equal w ""))
          (String.split_on_char '_' name)
      with
      | [] -> name
      | first :: rest ->
          first ^ String.concat "" (List.map String.capitalize_ascii rest))

let word_of_constructor spelling name =
  spell spelling (String.uncapitalize_ascii name)

let json_name type_name =
  if String.equal type_name "t" then "json" else type_name ^ "_json"

let lazy_name type_name = json_name type_name ^ "_lazy"

let rec json_lid = function
  | Lident n -> Lident (json_name n)
  | Ldot (m, n) -> Ldot (m, json_name n)
  | Lapply (a, b) -> Lapply (a, json_lid b)

let var_json v = v ^ "_json"

(* ------------------------------------------------------------------ *)
(* Core types *)

type env = {
  group : string list;  (** the types of this declaration group *)
  recursive : bool;  (** whether the group refers to itself *)
  params : bool;  (** whether the group's types take parameters *)
}

let rec desc env (ct : core_type) =
  let loc = ct.ptyp_loc in
  match Attribute.get A.ct_with ct with
  | Some e -> e
  | None when Attribute.has_flag A.ct_dict ct -> dict env ct []
  | None -> (
      match ct.ptyp_desc with
      | Ptyp_constr ({ txt = Lident "int"; _ }, []) -> [%expr Wiretype.int]
      | Ptyp_constr ({ txt = Lident "int64"; _ }, []) -> [%expr Wiretype.int64]
      | Ptyp_constr ({ txt = Lident "float"; _ }, []) -> [%expr Wiretype.number]
      | Ptyp_constr ({ txt = Lident "string"; _ }, []) ->
          [%expr Wiretype.string]
      | Ptyp_constr ({ txt = Lident "bool"; _ }, []) -> [%expr Wiretype.bool]
      | Ptyp_constr ({ txt = Lident "unit"; _ }, []) -> [%expr Wiretype.null ()]
      | Ptyp_constr ({ txt = Lident "list"; _ }, [ a ]) ->
          [%expr Wiretype.list [%e desc env a]]
      | Ptyp_constr ({ txt = Lident "array"; _ }, [ a ]) ->
          [%expr
            Wiretype.map ~dec:Array.of_list ~enc:Array.to_list
              (Wiretype.list [%e desc env a])]
      | Ptyp_constr ({ txt = Lident "option"; _ }, [ a ]) ->
          [%expr Wiretype.nullable [%e desc env a]]
      | Ptyp_constr
          ({ txt = Ldot (Ldot (Lident "Wiretype", "Value"), "t"); _ }, [])
      | Ptyp_constr ({ txt = Ldot (Lident "Value", "t"); _ }, []) ->
          [%expr Wiretype.value]
      | Ptyp_constr ({ txt = Lident n; _ }, args)
        when env.recursive && List.mem n env.group ->
          (* A type of the group is not yet defined where it is referred to:
             its description is reached through the one being made. *)
          if env.params then
            B.eapply ~loc (B.evar ~loc (json_name n)) (List.map (desc env) args)
          else [%expr Wiretype.rec' [%e B.evar ~loc (lazy_name n)]]
      | Ptyp_constr ({ txt; _ }, args) -> (
          let f = B.pexp_ident ~loc { txt = json_lid txt; loc } in
          match args with
          | [] -> f
          | _ :: _ -> B.eapply ~loc f (List.map (desc env) args))
      | Ptyp_var v -> B.evar ~loc (var_json v)
      | Ptyp_tuple cts ->
          (* A JSON array of exactly its items, each by its own description,
             built as an object is: a function, and each item it takes. *)
          let name i = Printf.sprintf "wiretype_%d" i in
          let names = List.mapi (fun i _ -> name i) cts in
          let build =
            List.fold_right
              (fun n acc -> B.pexp_fun ~loc Nolabel None (B.pvar ~loc n) acc)
              names
              (B.pexp_tuple ~loc (List.map (B.evar ~loc) names))
          in
          let item i ct =
            let pattern =
              B.ppat_tuple ~loc
                (List.mapi
                   (fun j n -> if i = j then B.pvar ~loc n else B.ppat_any ~loc)
                   names)
            in
            [%expr
              Wiretype.Tuple.item [%e desc env ct]
                ~enc:
                  [%e
                    B.pexp_fun ~loc Nolabel None pattern (B.evar ~loc (name i))]]
          in
          List.fold_left
            (fun acc e -> [%expr [%e acc] |> [%e e]])
            [%expr Wiretype.Tuple.map [%e build]]
            (List.mapi item cts @ [ [%expr Wiretype.Tuple.finish] ])
      | Ptyp_poly _ | Ptyp_arrow _ | Ptyp_object _ | Ptyp_class _ | Ptyp_alias _
      | Ptyp_variant _ | Ptyp_package _ | Ptyp_extension _ | Ptyp_any
      | Ptyp_open _ ->
          Location.raise_errorf ~loc
            "wiretype: this type has no JSON shape; describe it with [@with d]")

(* A list of pairs is a JSON list of two-item arrays unless it is asked to be
   a map, since which it is cannot be told from the type. *)
and dict env (ct : core_type) bounds =
  let loc = ct.ptyp_loc in
  match ct.ptyp_desc with
  | Ptyp_constr
      ({ txt = Lident "list"; _ }, [ { ptyp_desc = Ptyp_tuple [ k; v ]; _ } ])
    ->
      B.pexp_apply ~loc
        (B.evar ~loc "Wiretype.dict")
        (bounds @ [ (Nolabel, desc env k); (Nolabel, desc env v) ])
  | _ ->
      Location.raise_errorf ~loc
        "wiretype: [@dict] describes a (key * value) list, and this is not one"

let is_option (ct : core_type) =
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "option"; _ }, [ inner ]) -> Some inner
  | _ -> None

let base_name (ct : core_type) =
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = Lident n; _ }, _) -> Some n
  | _ -> None

(* ------------------------------------------------------------------ *)
(* A field: a record's, or an inline record's in a case *)

type field = { ocaml : string; wire : string; ld : label_declaration }

let fields spelling lds =
  List.map
    (fun (ld : label_declaration) ->
      let ocaml = ld.pld_name.txt in
      let wire =
        match Attribute.get A.key ld with
        | Some k -> k
        | None -> spell spelling ocaml
      in
      { ocaml; wire; ld })
    lds

let labelled label e = (Labelled label, e)
let some label = Option.map (labelled label)

(* A field's description, its bounds applied: to the inner type of an
   option, which is what may be left out. *)
let field_desc env ~target (ld : label_declaration) =
  let loc = ld.pld_loc in
  let get a = Attribute.get a ld in
  let numeric =
    List.filter_map Fun.id
      [
        some "min" (get A.min);
        some "max" (get A.max);
        some "multiple_of" (get A.multiple_of);
      ]
  in
  let exclusive =
    List.filter_map Fun.id
      [ some "above" (get A.above); some "below" (get A.below) ]
  in
  let length =
    List.filter_map Fun.id
      [
        some "min_length" (get A.min_length);
        some "max_length" (get A.max_length);
      ]
  in
  let items =
    List.filter_map Fun.id
      [ some "min_items" (get A.min_items); some "max_items" (get A.max_items) ]
  in
  let properties =
    List.filter_map Fun.id
      [
        some "min_properties" (get A.min_properties);
        some "max_properties" (get A.max_properties);
      ]
  in
  let unit_ = [ (Nolabel, [%expr ()]) ] in
  let bounded f args = B.pexp_apply ~loc (B.evar ~loc f) (args @ unit_) in
  let refuse attributes what =
    Location.raise_errorf ~loc
      "wiretype: %s bound %s, and this field is not one" attributes what
  in
  let none = List.is_empty in
  let as_dict = Attribute.has_flag A.dict ld in
  match Attribute.get A.with_ ld with
  | Some e ->
      if
        not
          (none numeric && none exclusive && none length && none items
         && none properties)
      then
        Location.raise_errorf ~loc
          "wiretype: a field described [@with d] is bounded by d";
      e
  | None when as_dict ->
      if not (none numeric && none exclusive && none length && none items) then
        Location.raise_errorf ~loc
          "wiretype: a map is bounded by [@min_properties] and \
           [@max_properties]";
      dict env target properties
  | None when not (none properties) ->
      refuse "[@min_properties] and [@max_properties]" "a map, [@dict]"
  | None -> (
      match (base_name target, target.ptyp_desc) with
      | _, _ when none numeric && none exclusive && none length && none items ->
          desc env target
      | Some "int", _ when none exclusive && none length && none items ->
          bounded "Wiretype.int_bounded" numeric
      | Some "int64", _ when none exclusive && none length && none items ->
          bounded "Wiretype.int64_bounded" numeric
      | Some "float", _ when none length && none items ->
          bounded "Wiretype.number_bounded" (numeric @ exclusive)
      | Some "string", _ when none numeric && none exclusive && none items ->
          bounded "Wiretype.string_bounded" length
      | Some "list", Ptyp_constr (_, [ elt ])
        when none numeric && none exclusive && none length ->
          B.pexp_apply ~loc
            (B.evar ~loc "Wiretype.list")
            (items @ [ (Nolabel, desc env elt) ])
      | _ ->
          if not (none length) then
            refuse "[@min_length] and [@max_length]" "a string"
          else if not (none items) then
            refuse "[@min_items] and [@max_items]" "a list"
          else
            refuse "[@min], [@max], [@above], [@below] and [@multiple_of]"
              "an int, an int64 or a float")

(* [|> Wiretype.Object.mem ...] for each field, [enc] being how the value
   is found from what the object is built from. *)
let member env (f : field) ~enc =
  let ld = f.ld in
  let loc = ld.pld_loc in
  let doc =
    Option.map
      (fun d -> labelled "doc" (B.estring ~loc d))
      (doc_of ld.pld_attributes)
  in
  let flag a name =
    if Attribute.has_flag a ld then Some (labelled name [%expr true]) else None
  in
  let read_only = flag A.read_only "read_only"
  and write_only = flag A.write_only "write_only" in
  let deprecated = flag A.deprecated "deprecated" in
  let enc = labelled "enc" enc in
  let name = (Nolabel, B.estring ~loc f.wire) in
  match (is_option ld.pld_type, Attribute.get A.default ld) with
  | Some inner, None ->
      let d = field_desc env ~target:inner ld in
      let args =
        List.filter_map Fun.id
          [ doc; Some enc; deprecated; read_only; write_only ]
      in
      B.pexp_apply ~loc [%expr Wiretype.Object.opt_mem]
        (args @ [ name; (Nolabel, d) ])
  | (Some _ | None), default ->
      let d = field_desc env ~target:ld.pld_type ld in
      let examples =
        Option.map (labelled "examples") (Attribute.get A.examples ld)
      in
      let absent = Option.map (labelled "absent") default in
      let args =
        List.filter_map Fun.id
          [ doc; absent; Some enc; deprecated; read_only; write_only; examples ]
      in
      B.pexp_apply ~loc [%expr Wiretype.Object.mem]
        (args @ [ name; (Nolabel, d) ])

let pipe ~loc acc e = [%expr [%e acc] |> [%e e]]

(* An object from fields: [build] makes the value from them in order, and
   [project i f] is how the i-th, [f], is found in the value. *)
let object_of env ~loc ~kind ~doc fs ~build ~project =
  let map =
    B.pexp_apply ~loc [%expr Wiretype.Object.map]
      (List.filter_map Fun.id
         [
           Option.map (fun k -> labelled "kind" (B.estring ~loc k)) kind;
           Option.map (fun d -> labelled "doc" (B.estring ~loc d)) doc;
           Some (Nolabel, build);
         ])
  in
  let members = List.mapi (fun i f -> member env f ~enc:(project i f)) fs in
  pipe ~loc
    (List.fold_left (pipe ~loc) map members)
    [%expr Wiretype.Object.finish]

let lambda ~loc names body =
  List.fold_right
    (fun n acc -> B.pexp_fun ~loc Nolabel None (B.pvar ~loc n) acc)
    names body

(* ------------------------------------------------------------------ *)
(* Declarations *)

let type_of_decl ~loc (td : type_declaration) =
  B.ptyp_constr ~loc
    { txt = Lident td.ptype_name.txt; loc }
    (List.map (fun _ -> B.ptyp_any ~loc) td.ptype_params)

let record env ~loc ~kind ~doc ~spelling td lds =
  let fs = fields spelling lds in
  let ty = type_of_decl ~loc td in
  let build =
    lambda ~loc
      (List.map (fun f -> f.ocaml) fs)
      (B.pexp_constraint ~loc
         (B.pexp_record ~loc
            (List.map
               (fun f -> ({ txt = Lident f.ocaml; loc }, B.evar ~loc f.ocaml))
               fs)
            None)
         ty)
  in
  let project _ f =
    [%expr
      fun (r : [%t ty]) ->
        [%e B.pexp_field ~loc [%expr r] { txt = Lident f.ocaml; loc }]]
  in
  object_of env ~loc ~kind ~doc fs ~build ~project

(* A case's value, from an inline record's fields: the one field, or a tuple
   of them, which never leaves the description. *)
let tuple_of ~loc names =
  match names with
  | [ n ] -> B.evar ~loc n
  | ns -> B.pexp_tuple ~loc (List.map (B.evar ~loc) ns)

let tuple_pat ~loc names =
  match names with
  | [ n ] -> B.pvar ~loc n
  | ns -> B.ppat_tuple ~loc (List.map (B.pvar ~loc) ns)

type case = {
  word : string;
  obj : expression;  (** the case's object *)
  dec : expression;  (** from its value to the variant *)
  pattern : pattern;  (** the variant's case, binding what [value] is made of *)
  value : expression;  (** the case's value, from what [pattern] bound *)
}

let variant_case env ~spelling (cd : constructor_declaration) =
  let loc = cd.pcd_loc in
  let name = cd.pcd_name.txt in
  let word =
    match Attribute.get A.cd_name cd with
    | Some w -> w
    | None -> word_of_constructor spelling name
  in
  let construct arg = B.pexp_construct ~loc { txt = Lident name; loc } arg in
  let pconstruct arg = B.ppat_construct ~loc { txt = Lident name; loc } arg in
  match cd.pcd_args with
  | Pcstr_tuple [] ->
      {
        word;
        obj = [%expr Wiretype.Object.finish (Wiretype.Object.map ())];
        dec = [%expr fun () -> [%e construct None]];
        pattern = pconstruct None;
        value = [%expr ()];
      }
  | Pcstr_tuple [ arg ] ->
      let arg = arg in
      {
        word;
        obj = desc env arg;
        dec = [%expr fun v -> [%e construct (Some [%expr v])]];
        pattern = pconstruct (Some [%pat? v]);
        value = [%expr v];
      }
  | Pcstr_tuple _ ->
      Location.raise_errorf ~loc
        "wiretype: %s has several arguments; give them names in an inline \
         record"
        name
  | Pcstr_record lds ->
      let fs = fields spelling lds in
      let names = List.map (fun f -> f.ocaml) fs in
      let n = List.length fs in
      let project i (f : field) =
        match n with
        | 1 -> [%expr fun v -> v]
        | _ ->
            B.pexp_fun ~loc Nolabel None
              (B.ppat_tuple ~loc
                 (List.mapi
                    (fun j g ->
                      if i = j then B.pvar ~loc g.ocaml else B.ppat_any ~loc)
                    fs))
              (B.evar ~loc f.ocaml)
      in
      let obj =
        object_of env ~loc ~kind:None ~doc:(doc_of cd.pcd_attributes) fs
          ~build:(lambda ~loc names (tuple_of ~loc names))
          ~project
      in
      let record =
        B.pexp_record ~loc
          (List.map
             (fun f -> ({ txt = Lident f.ocaml; loc }, B.evar ~loc f.ocaml))
             fs)
          None
      in
      let precord =
        B.ppat_record ~loc
          (List.map
             (fun f -> ({ txt = Lident f.ocaml; loc }, B.pvar ~loc f.ocaml))
             fs)
          Closed
      in
      {
        word;
        obj;
        dec =
          B.pexp_fun ~loc Nolabel None (tuple_pat ~loc names)
            (construct (Some record));
        pattern = pconstruct (Some precord);
        value = tuple_of ~loc names;
      }

let poly_case env ~spelling (rf : row_field) =
  let loc = rf.prf_loc in
  match rf.prf_desc with
  | Rinherit _ ->
      Location.raise_errorf ~loc
        "wiretype: an inherited polymorphic variant is not described; list its \
         tags"
  | Rtag ({ txt = name; _ }, _, args) -> (
      let word =
        match Attribute.get A.rtag_name rf with
        | Some w -> w
        | None -> word_of_constructor spelling name
      in
      match args with
      | [] ->
          {
            word;
            obj = [%expr Wiretype.Object.finish (Wiretype.Object.map ())];
            dec = [%expr fun () -> [%e B.pexp_variant ~loc name None]];
            pattern = B.ppat_variant ~loc name None;
            value = [%expr ()];
          }
      | [ arg ] ->
          {
            word;
            obj = desc env arg;
            dec =
              [%expr fun v -> [%e B.pexp_variant ~loc name (Some [%expr v])]];
            pattern = B.ppat_variant ~loc name (Some [%pat? v]);
            value = [%expr v];
          }
      | _ ->
          Location.raise_errorf ~loc
            "wiretype: `%s has several types; describe it with [@with d]" name)

let constant cases =
  List.for_all
    (fun c ->
      match c.value.pexp_desc with
      | Pexp_construct ({ txt = Lident "()"; _ }, None) -> true
      | _ -> false)
    cases

let cases_desc ~loc ~kind ~doc ~tag cases =
  let kind_arg =
    Option.map (fun k -> labelled "kind" (B.estring ~loc k)) kind
  in
  let doc_arg = Option.map (fun d -> labelled "doc" (B.estring ~loc d)) doc in
  if constant cases then
    let word =
      B.pexp_function_cases ~loc
        (List.map
           (fun c ->
             B.case ~lhs:c.pattern ~guard:None ~rhs:(B.estring ~loc c.word))
           cases)
    in
    let values =
      B.elist ~loc
        (List.map
           (fun c -> c.dec |> fun d -> B.eapply ~loc d [ [%expr ()] ])
           cases)
    in
    B.pexp_apply ~loc [%expr Wiretype.enum]
      (List.filter_map Fun.id
         [ kind_arg; doc_arg; Some (Nolabel, word); Some (Nolabel, values) ])
  else
    let named =
      List.mapi (fun i c -> (Printf.sprintf "wiretype_case_%d" i, c)) cases
    in
    let bindings =
      List.map
        (fun (n, c) ->
          B.value_binding ~loc ~pat:(B.pvar ~loc n)
            ~expr:
              [%expr
                Wiretype.Object.Case.map [%e B.estring ~loc c.word] [%e c.obj]
                  ~dec:[%e c.dec]])
        named
    in
    let enc_case =
      B.pexp_function_cases ~loc
        (List.map
           (fun (n, c) ->
             B.case ~lhs:c.pattern ~guard:None
               ~rhs:
                 [%expr
                   Wiretype.Object.Case.value [%e B.evar ~loc n] [%e c.value]])
           named)
    in
    let makes =
      B.elist ~loc
        (List.map
           (fun (n, _) -> [%expr Wiretype.Object.Case.make [%e B.evar ~loc n]])
           named)
    in
    let map =
      B.pexp_apply ~loc [%expr Wiretype.Object.map]
        (List.filter_map Fun.id
           [ kind_arg; doc_arg; Some (Nolabel, [%expr fun c -> c]) ])
    in
    let body =
      [%expr
        [%e map]
        |> Wiretype.Object.case_mem [%e B.estring ~loc tag] Wiretype.string
             ~enc:(fun c -> c)
             ~enc_case:[%e enc_case] [%e makes]
        |> Wiretype.Object.finish]
    in
    B.pexp_let ~loc Nonrecursive bindings body

let module_name code_path =
  match List.rev (Code_path.submodule_path code_path) with
  | m :: _ -> m
  | [] ->
      String.capitalize_ascii
        (Filename.remove_extension
           (Filename.basename (Code_path.file_path code_path)))

let body env ~code_path (td : type_declaration) =
  let loc = td.ptype_loc in
  let spelling =
    match Attribute.get A.rename_all td with
    | Some s -> spelling_of ~loc s
    | None -> Snake
  in
  let name = td.ptype_name.txt in
  let kind =
    match Attribute.get A.kind td with
    | Some k -> Some k
    | None ->
        Some (if String.equal name "t" then module_name code_path else name)
  in
  let doc = doc_of td.ptype_attributes in
  let tag = Option.value (Attribute.get A.tag td) ~default:"type" in
  match (td.ptype_kind, td.ptype_manifest) with
  | Ptype_record lds, _ -> record env ~loc ~kind ~doc ~spelling td lds
  | Ptype_variant cds, _ ->
      cases_desc ~loc ~kind ~doc ~tag
        (List.map (variant_case env ~spelling) cds)
  | Ptype_abstract, Some { ptyp_desc = Ptyp_variant (rows, _, _); _ } ->
      cases_desc ~loc ~kind ~doc ~tag (List.map (poly_case env ~spelling) rows)
  | Ptype_abstract, Some manifest -> desc env manifest
  | Ptype_abstract, None ->
      Location.raise_errorf ~loc
        "wiretype: %s is abstract; there is nothing to describe" name
  | Ptype_open, _ ->
      Location.raise_errorf ~loc
        "wiretype: an extensible type has no fixed shape"

(* Whether a core type names one of [names]. *)
let rec mentions names (ct : core_type) =
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = Lident n; _ }, args) ->
      List.mem n names || List.exists (mentions names) args
  | Ptyp_constr (_, args) | Ptyp_tuple args -> List.exists (mentions names) args
  | Ptyp_variant (rows, _, _) ->
      List.exists
        (fun rf ->
          match rf.prf_desc with
          | Rtag (_, _, cts) -> List.exists (mentions names) cts
          | Rinherit ct -> mentions names ct)
        rows
  | Ptyp_alias (ct, _) | Ptyp_poly (_, ct) -> mentions names ct
  | Ptyp_arrow (_, a, b) -> mentions names a || mentions names b
  | _ -> false

let decl_mentions names (td : type_declaration) =
  let lds lds =
    List.exists (fun (ld : label_declaration) -> mentions names ld.pld_type) lds
  in
  (match td.ptype_kind with
    | Ptype_record l -> lds l
    | Ptype_variant cds ->
        List.exists
          (fun (cd : constructor_declaration) ->
            match cd.pcd_args with
            | Pcstr_tuple cts -> List.exists (mentions names) cts
            | Pcstr_record l -> lds l)
          cds
    | Ptype_abstract | Ptype_open -> false)
  || match td.ptype_manifest with Some m -> mentions names m | None -> false

let param_names (td : type_declaration) =
  List.map
    (fun (ct, _) ->
      match ct.ptyp_desc with
      | Ptyp_var v -> v
      | _ ->
          Location.raise_errorf ~loc:ct.ptyp_loc
            "wiretype: a type parameter is named, as 'a")
    td.ptype_params

let str_type_decl ~ctxt (rec_flag, tds) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let code_path = Expansion_context.Deriver.code_path ctxt in
  let group = List.map (fun td -> td.ptype_name.txt) tds in
  let recursive =
    (match rec_flag with Recursive -> true | Nonrecursive -> false)
    && List.exists (decl_mentions group) tds
  in
  let params =
    List.exists (fun td -> not (List.is_empty td.ptype_params)) tds
  in
  let env = { group; recursive; params } in
  let with_params td e = lambda ~loc (List.map var_json (param_names td)) e in
  match (recursive, params) with
  | false, _ ->
      List.map
        (fun td ->
          [%stri
            let [%p B.pvar ~loc (json_name td.ptype_name.txt)] =
              [%e with_params td (body env ~code_path td)]])
        tds
  | true, true ->
      [
        B.pstr_value ~loc Recursive
          (List.map
             (fun td ->
               B.value_binding ~loc
                 ~pat:(B.pvar ~loc (json_name td.ptype_name.txt))
                 ~expr:
                   (with_params td
                      [%expr Wiretype.rec' (lazy [%e body env ~code_path td])]))
             tds);
      ]
  | true, false ->
      [
        B.pstr_value ~loc Recursive
          (List.map
             (fun td ->
               B.value_binding ~loc
                 ~pat:(B.pvar ~loc (lazy_name td.ptype_name.txt))
                 ~expr:[%expr lazy [%e body env ~code_path td]])
             tds);
        B.pstr_value ~loc Nonrecursive
          (List.map
             (fun td ->
               B.value_binding ~loc
                 ~pat:(B.pvar ~loc (json_name td.ptype_name.txt))
                 ~expr:
                   [%expr
                     Wiretype.rec'
                       [%e B.evar ~loc (lazy_name td.ptype_name.txt)]])
             tds);
      ]

let sig_type_decl ~ctxt (_rec_flag, tds) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map
    (fun td ->
      let vars = param_names td in
      let ty =
        B.ptyp_constr ~loc
          { txt = Lident td.ptype_name.txt; loc }
          (List.map (B.ptyp_var ~loc) vars)
      in
      let described t = [%type: [%t t] Wiretype.t] in
      let full =
        List.fold_right
          (fun v acc -> [%type: [%t described (B.ptyp_var ~loc v)] -> [%t acc]])
          vars (described ty)
      in
      B.psig_value ~loc
        (B.value_description ~loc
           ~name:{ txt = json_name td.ptype_name.txt; loc }
           ~type_:full ~prim:[]))
    tds

let () =
  let attributes =
    Attribute.
      [
        T A.key;
        T A.default;
        T A.with_;
        T A.min;
        T A.max;
        T A.above;
        T A.below;
        T A.multiple_of;
        T A.min_length;
        T A.max_length;
        T A.min_items;
        T A.max_items;
        T A.min_properties;
        T A.max_properties;
        T A.dict;
        T A.ct_dict;
        T A.examples;
        T A.read_only;
        T A.write_only;
        T A.deprecated;
        T A.cd_name;
        T A.rtag_name;
        T A.tag;
        T A.kind;
        T A.rename_all;
        T A.ct_with;
      ]
  in
  ignore
    (Deriving.add "wiretype"
       ~str_type_decl:
         (Deriving.Generator.V2.make_noarg ~attributes str_type_decl)
       ~sig_type_decl:(Deriving.Generator.V2.make_noarg sig_type_decl))
