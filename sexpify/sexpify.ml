open! Core

let mknoloc = Location.mknoloc
let loc = Location.none

open Ppxlib

module Longident = struct
  module T = struct
    type t = longident =
      | Lident of string
      | Ldot of t * string
      | Lapply of t * t
    [@@deriving sexp]

    include (Ppxlib.Longident : module type of struct include Ppxlib.Longident end with type t := t)
  end
  include T
  include Comparable.Make(T)

  let rec of_list_rev = function
    | [] -> assert false
    | [ x ] -> Lident x
    | x :: xs -> Ldot (of_list_rev xs, x)

  let of_list l = of_list_rev (List.rev l)

  let dot (a : t) (b : t) =
    (flatten_exn a) @ (flatten_exn b)
    |> of_list
end

let cleanup = 
  let replace =
    Longident.Map.of_alist_exn
      [ Longident.parse "Digest.t", Longident.parse "Caml_digest.t"
      ; Longident.parse "Type_immediacy.t", Longident.parse "Caml_type_immediacy.t"
      ; Longident.parse "Type_immediacy.Violation.t", Longident.parse "Caml_type_immediacy.Violation.t"
      ]
  in
  let filter_attr attrs =
    List.filter attrs
      ~f:(fun attr ->
          match attr.attr_name.txt with
          | "ocaml.doc" -> false
          | _ -> true
        )
  in
  object
    inherit Ast_traverse.map as super

    method! longident lident =
      Map.find replace lident
      |> Option.value ~default:lident

    method! attributes a =
      let result = super#attributes a in
      filter_attr result
  end

type t =
  { types : core_type list String.Map.t
  ; modules : Longident.Set.t
  ; has_values : bool
  }

let empty =
  { types = String.Map.empty
  ; modules = Longident.Set.empty
  ; has_values = false
  }

let typename : type_declaration -> _ = function
  | { ptype_name = { txt; _}; ptype_params; _} -> 
    let params = List.map ptype_params ~f:fst in
    txt, params

let rec map_last ~f = function
  | [] -> []
  | [ x ] -> [ f x ]
  | x :: xs -> x :: (map_last ~f xs)

let create_ocamlcommon_ident =
  let global_modules = Longident.Set.of_list [ ] in
  fun ~module_name ~typename ->
    if Set.mem global_modules module_name
    then
      mknoloc
        (Longident.Ldot (module_name, typename))
    else
      mknoloc
        (Longident.Ldot ((Longident.dot (Lident "Compiler_without_sexp") module_name), typename))

let add_manifest ~module_name (td : type_declaration) =
  match td.ptype_kind, td.ptype_manifest with
  | (Ptype_variant _ | Ptype_record _), _ ->
    let params = List.map td.ptype_params ~f:fst in
    let ptype_manifest =
      Ast_helper.Typ.constr
        (create_ocamlcommon_ident ~module_name ~typename:td.ptype_name.txt)
        params
      |> Some
    in
    { td with
      ptype_manifest
    ; ptype_attributes = []
    }
  | Ptype_abstract, None ->
    let params = List.map td.ptype_params ~f:fst in
    let ptype_manifest =
      Ast_helper.Typ.constr
        ~attrs:[Ast_helper.Attr.mk (mknoloc "sexp.opaque") (PStr [])]
        (create_ocamlcommon_ident ~module_name ~typename:td.ptype_name.txt)
        params
      |> Some
    in
    { td with
      ptype_manifest
    ; ptype_attributes = []
    }
  | Ptype_abstract, _ ->
    { td with
      ptype_attributes = []
    }
  | Ptype_open, _ -> 
    { td with
      ptype_attributes = []
    }

let add_deriving_sexp (td : type_declaration) =
  let attribute =
    Ast_helper.Attr.mk (mknoloc "deriving") (PStr [%str sexp_of]);
  in
  { td with ptype_attributes = [ attribute ] }

let typesubst ~module_name name params : with_constraint =
  let manifest =
    Ast_helper.Typ.constr 
      (create_ocamlcommon_ident ~module_name ~typename:name)
      params
  in
  Pwith_typesubst 
    (mknoloc (Lident name), Ast_helper.Type.mk 
       ~params:(List.map params ~f:(fun p -> p, Invariant))
       ~manifest (mknoloc "x")) 

let modsubst ~module_name:_ subst_module_name : with_constraint =
  Pwith_modsubst
    ( (mknoloc (Lident (Longident.last_exn subst_module_name)))
    , (mknoloc subst_module_name)
    )

let mk_inc ~module_name types modules =
  let open Ast_helper in
  let modname = mknoloc (Longident.dot (Lident "Compiler_without_sexp") module_name) in
  Str.include_
    (Incl.mk
       (Mod.constraint_
          (Mod.ident modname)
          (Mty.with_
             (Mty.typeof_ 
                (Mod.structure [ Str.include_ (Incl.mk (Mod.ident modname)) ]))
             ((Map.fold ~init:[] ~f:(fun ~key ~data acc -> typesubst ~module_name key data :: acc) types |> List.rev)
              @ (List.map ~f:(modsubst ~module_name) (Set.to_list modules))
             )
          )
       )
    )

let rec convert_sig ~module_name signature : structure_item list =
  let {types; modules; has_values} , as_struct =
    List.fold_map signature ~init:empty ~f:(traverse_sig ~module_name)
  in
  let as_struct =
    List.concat as_struct
    |> cleanup#structure
  in
  if not has_values
  then as_struct
  else begin
    as_struct
    @ [ mk_inc ~module_name types modules ]
  end
and traverse_sig ~module_name ({ types; modules; has_values = _ } as acc) sig_element =
  let { psig_desc; _ } = sig_element in
  match psig_desc with
  | Psig_type (rec_flag,sig_types) -> 
    let typenames = List.map sig_types ~f:typename in
    let types = List.fold typenames ~init:types ~f:(fun map (key, data) -> Map.add_exn map ~key ~data) in
    let str_types =
      List.map sig_types ~f:(add_manifest ~module_name)
      |> map_last ~f:add_deriving_sexp
      |> Ast_helper.Str.type_ rec_flag
      |> List.return
    in
    { acc with types}, str_types
  | Psig_module {pmd_name = { txt = pmd_name; _}; pmd_type = { pmty_desc = Pmty_alias alias; _} ; _ }-> 
    let converted =
      let open Ast_helper in
      Str.module_ (Mb.mk (mknoloc pmd_name) (Mod.ident alias))
    in
    let modules =
      let module_name = Longident.Ldot (module_name, pmd_name) in
      let modname = Longident.dot (Lident "Compiler_without_sexp") module_name in
      Set.add modules modname
    in 
    { acc with modules }, [ converted ]
  | Psig_module {pmd_name = { txt = pmd_name; _}; pmd_type = { pmty_desc = Pmty_signature msig ; _} ; _ }-> 
    let module_name = Longident.Ldot (module_name, pmd_name) in
    let converted =
      convert_sig ~module_name msig 
    in
    let converted =
      let open Ast_helper in
      Str.module_ (Mb.mk (mknoloc pmd_name) (Mod.structure converted))
    in
    let modules =
      let modname = Longident.dot (Lident "Compiler_without_sexp") module_name in
      Set.add modules modname
    in 
    { acc with modules}, [ converted ]
  | Psig_module psm ->
    module_special_case ~module_name ~acc psm
  | Psig_open { popen_expr = { loc = _; txt = Lident mod_ }
              ; popen_override = _
              ; popen_loc = _
              ; popen_attributes = _
              } -> 
    let result = 
      let open Ast_helper in
      Str.open_ (Opn.mk ~override:Override (Mod.ident (mknoloc (Lident mod_))))
    in
    acc, [ result ]
  | Psig_open _
  |Psig_typesubst _|Psig_typext _|Psig_exception _
  |Psig_modsubst _|Psig_recmodule _|Psig_modtype _
  |Psig_include _|Psig_class _|Psig_class_type _|Psig_attribute _
  |Psig_extension (_, _) ->
    acc, []
  | Psig_value _ -> 
    { acc with has_values = true }, []
and module_special_case ~module_name ~acc psm =
  let p = 
    let open Ast_pattern in
    pmty_with
      (pmty_ident __)
      (pwith_type 
         (lident __)
         (type_declaration
            ~name:__
            ~params:nil
            ~cstrs:nil
            ~kind:ptype_abstract
            ~private_:public
            ~manifest:(some __)
         )
       ^:: nil)
  in
  match psm with
  | {pmd_name = { txt = pmd_name; _}; pmd_type; pmd_loc; _ }-> 
    Ast_pattern.parse p pmd_loc pmd_type
      ~on_error:(fun () ->
          (acc, []))
      (fun left right _ keytype ->
         match left, right with
         | Ldot (Lident ("Map"), "S"), _ ->
           let open Ast_helper in
           let modname =
             Longident.dot (Lident "Compiler_without_sexp") (Ldot (module_name, pmd_name))
           in
           let expr = 
             Str.module_
               (Mb.mk
                  (mknoloc pmd_name)
                  (Mod.structure
                     ([ Str.include_ (Ast_helper.Incl.mk (Mod.ident (mknoloc modname)))
                      ] 
                      @ [%str
                        let sexp_of_t sexp_of_a t =
                          fold (fun key data acc ->
                              (key,data) :: acc) t [] 
                          |> [%sexp_of: ([%t keytype] * a) list]
                      ]
                     )
                  )
               )
           in
           let modules = Set.add acc.modules modname in 
           { acc with modules}
         , [ expr ]
         | Ldot (Lident ("Set"), "S"), _ ->
           let open Ast_helper in
           let modname =
             Longident.dot (Lident "Compiler_without_sexp") (Ldot (module_name, pmd_name))
           in
           let expr = 
             Str.module_
               (Mb.mk
                  (mknoloc pmd_name)
                  (Mod.structure
                     ([ Str.include_ (Ast_helper.Incl.mk (Mod.ident (mknoloc modname)))
                      ] 
                      @ [%str
                        let sexp_of_t t =
                          fold (fun key acc ->
                              key :: acc) t [] 
                          |> [%sexp_of: [%t keytype] list]
                      ]
                     )
                  )
               )
           in
           let modules = Set.add acc.modules modname in 
           { acc with modules}
         , [ expr ]
         | _ ->
           acc, [])


let sexpify_file filename =
  let module_name =
    (String.chop_suffix_exn ~suffix:".mli" (Filename.basename filename)
     |> String.capitalize
     |> Longident.Lident
    )
  in
  let interface =
    In_channel.with_file filename
      ~f:(fun channel ->
          let lexbuf = Lexing.from_channel channel in
          Parse.interface lexbuf
        )
  in
  let result =
    [%str open! Core ]
    @ (convert_sig ~module_name interface)
  in
  Format.fprintf
    Format.str_formatter "(* This file is generated from %s using sexpify *)\n" filename;
  result
  |> Pprintast.structure Format.str_formatter;
  Format.flush_str_formatter ()

let ocamlformat string =
  let process = Unix.create_process ~prog:"ocamlformat" ~args:[ "-p"; "janestreet"; "--impl"; "-" ] in
  let c = Unix.out_channel_of_descr process.stdin in
  Out_channel.output_string c string;
  Out_channel.close c;
  In_channel.input_all (Unix.in_channel_of_descr process.stdout)

let command : Command.t =
  let open Command.Let_syntax in
  Command.basic
    ~summary:""
    [%map_open
      let filename = anon ("FILENAME" %: string)
      in
      fun () ->
        sexpify_file filename
        |> ocamlformat
        |> print_endline
    ]

let () =
  Command.run command
