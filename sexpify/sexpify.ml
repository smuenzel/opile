open! Core

let mknoloc = Location.mknoloc
let loc = Location.none

open Ppxlib


type t = unit

let add_with_sexp = object
  inherit Ast_traverse.map

  method! longident = function
    | Longident.Ldot (Lident module_name, txt) ->
      let module_name = module_name ^ "_with_sexp" in
      Ldot (Lident module_name,txt)
    | a -> a

end

let add_manifest ~module_name (td : type_declaration) =
  match td.ptype_kind with
  | Ptype_variant _ | Ptype_record _ ->
    let params = List.map td.ptype_params ~f:fst in
    let ptype_manifest =
      Ast_helper.Typ.constr
        (mknoloc (Longident.Ldot (Longident.Lident module_name ,td.ptype_name.txt)))
        params
      |> Some
    in
    { td with
      ptype_manifest
    ; ptype_attributes = []
    }
  | Ptype_abstract | Ptype_open -> 
    { td with
      ptype_attributes = []
    }

let add_deriving_sexp (td : type_declaration) =
  let attribute =
    Ast_helper.Attr.mk (mknoloc "deriving") (PStr [%str sexp]);
  in
  { td with ptype_attributes = [ attribute ] }

let rec map_last ~f = function
  | [] -> []
  | [ x ] -> [ f x ]
  | x :: xs -> x :: (map_last ~f xs)


let psi ~module_name ({psig_desc; _} : signature_item) : structure_item option =
  match psig_desc with
  | Psig_type (rec_flag,types) -> 
    let types = List.map ~f:(add_manifest ~module_name) types in
    let types = map_last ~f:add_deriving_sexp types in
    Ast_helper.Str.type_ rec_flag types
    |> Some
  | Psig_value _|Psig_typesubst _|Psig_typext _|Psig_exception _
  |Psig_module _|Psig_modsubst _|Psig_recmodule _|Psig_modtype _|Psig_open _
  |Psig_include _|Psig_class _|Psig_class_type _|Psig_attribute _
  |Psig_extension (_, _) -> None

let typename : structure_item -> _ list = function
  | { pstr_desc = Pstr_type (_,types); _} -> 
    List.map types 
      ~f:(fun { ptype_name = { txt; _}; ptype_params; _} -> 
          let params = List.map ptype_params ~f:fst in
          txt, params
        )
  | _ -> []


let typesubst ~module_name (name,params) : with_constraint =
  let manifest =
    Ast_helper.Typ.constr (mknoloc (Ldot ((Lident module_name), name))) params
  in
  Pwith_typesubst 
    (mknoloc (Lident name), Ast_helper.Type.mk 
       ~params:(List.map params ~f:(fun p -> p, Invariant))
       ~manifest (mknoloc "x")) 

let mk_inc ~module_name types =
  let open Ast_helper in
  Str.include_
    (Incl.mk
       (Mod.constraint_
          (Mod.ident (mknoloc (Lident module_name)))
          (Mty.with_
             (Mty.typeof_ 
                (Mod.structure [ Str.include_ (Incl.mk (Mod.ident (mknoloc (Lident module_name)))) ]))
             (List.map ~f:(typesubst ~module_name) types)
          )
       )

    )

let sexpify_file filename =
  let module_name =
    (String.chop_suffix_exn ~suffix:".mli" (Filename.basename filename)
     |> String.capitalize
    )
  in
  let interface =
    In_channel.with_file filename
      ~f:(fun channel ->
          let lexbuf = Lexing.from_channel channel in
          Parse.interface lexbuf
        )
  in
  let has_val = List.exists interface ~f:(function | {psig_desc = Psig_value _; _ } -> true | _ -> false) in
  let from_file =
    interface
    |> add_with_sexp#signature
    |> List.filter_map ~f:(psi ~module_name)
  in
  let all_types =
    List.concat_map ~f:typename from_file 
  in
  let result =
    [%str open Core ]
    @ from_file
    @ (if has_val then [ mk_inc ~module_name all_types] else [])
  in
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
