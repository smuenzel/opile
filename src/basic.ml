open! Core
open! Compiler_with_sexp


let rec clean_sexp : Sexp.t -> Sexp.t = function
  | List [ Atom x; _ ] when is_loc x -> List [ ]
  | List [ Atom "txt"; txt ] -> clean_sexp txt
  | List (List [ Atom "loc_start"; _ ] :: _) -> List [ ]
  | List list ->
    List (List.map ~f:clean_sexp list)
    |> omit_nil
    |> filter_list
  | Atom _ as a -> a
and is_loc = String.is_suffix ~suffix:"loc"
and omit_nil : Sexp.t -> Sexp.t = function
  | List [ Atom _; List [ ] ] -> List [ ]
  | other -> other
and filter_list : Sexp.t -> Sexp.t = function
  | List x -> List (List.filter x ~f:(function | List [ ] -> false | _ -> true))
  | other -> other

let ld_library_path_contents () =
  match Sys.getenv_exn "CAML_LD_LIBRARY_PATH" with
  | s ->
      Misc.split_path_contents s

let f2 str =
  let ttstr, _, _, _ =
    Cmt_format.clear ();
    Typecore.reset_delayed_checks ();
    Env.reset_required_globals ();
    let env =
      Typemod.initial_env
        ~loc:Location.none
        ~safe_string:true
        ~initially_opened_module:(Some "Stdlib")
        ~open_implicit_modules:[]
    in
    Typemod.type_structure env str Location.none
  in
  ttstr
  |> [%sexp_of: Typedtree.structure]

let empty_formatter : Format.formatter =
  Format.make_formatter (fun _ _ _ -> ()) (fun () -> ())

module Backend = struct
  (* See backend_intf.mli. *)

  let symbol_for_global' = Compilenv.symbol_for_global'
  let closure_symbol = Compilenv.closure_symbol

  let really_import_approx = Import_approx.really_import_approx
  let import_symbol = Import_approx.import_symbol

  let size_int = Arch.size_int
  let big_endian = Arch.big_endian

  let max_sensible_number_of_arguments =
    (* The "-1" is to allow for a potential closure environment parameter. *)
    Proc.max_arguments_for_tailcalls - 1
end
let backend = (module Backend : Backend_intf.S)

let f str =
  let parsetree = Parse.implementation (Lexing.from_string str) in
  let cleaned_parsetree =
    parsetree
    |> [%sexp_of: Parsetree.structure]
    |> clean_sexp
  in
  let typedtree = 
    let ttstr, _, _, _ =
      try
        Load_path.init
          (ld_library_path_contents ())
        ;
        Cmt_format.clear ();
        Typecore.reset_delayed_checks ();
        Env.reset_required_globals ();
        let env =
          Typemod.initial_env
            ~loc:Location.none
            ~safe_string:true
            ~initially_opened_module:(Some "Stdlib")
            ~open_implicit_modules:[]
        in
        Typemod.type_structure env parsetree Location.none
      with
      | Typetexp.Error (_,_,error) ->
        raise_s [%message "error typing" (error : Typetexp.error)]
    in
    ttstr
  in
  let cleaned_typedtree =
    typedtree
    |> [%sexp_of: Typedtree.structure]
    |> clean_sexp
  in
  let lambda =
    Translmod.transl_implementation_flambda "Test" (typedtree,Tcoerce_none)
  in
  let cleaned_lambda =
    lambda
    |> [%sexp_of: Lambda.program]
    |> clean_sexp
  in
  let simplif_lambda =
    Simplif.simplify_lambda "Test" lambda.code
  in
  let cleaned_simplif_lambda =
    simplif_lambda
    |> [%sexp_of: Lambda.lambda]
    |> clean_sexp
  in
  let flambda : Flambda.program =
    Middle_end.middle_end
      ~ppf_dump:empty_formatter
      ~prefixname:""
      ~backend
      ~size:lambda.main_module_block_size
      ~filename:"test.ml"
      ~module_ident:lambda.module_ident
      ~module_initializer:simplif_lambda
  in
  let cleaned_flambda =
    flambda
    |> [%sexp_of: Flambda.program]
  in
  print_s cleaned_parsetree;
  print_s cleaned_typedtree;
  print_s cleaned_lambda;
  print_s cleaned_simplif_lambda

let%expect_test "hello" =
  f {|
    let f z () =
      let x, y =
        match z with
        | 0 -> 0, 1
        | 1 -> 1, 0
        | _ -> assert false
      in
      x + y
    |};
  [%expect {|
    (((pstr_desc
       (Pstr_value Nonrecursive
        (((pvb_expr
           ((pexp_desc
             (Pexp_fun Nolabel ((ppat_desc (Ppat_construct ((Lident "()")))))
              ((pexp_desc
                (Pexp_let Nonrecursive
                 (((pvb_expr
                    ((pexp_desc
                      (Pexp_match ((pexp_desc (Pexp_ident ((Lident z)))))
                       (((pc_lhs
                          ((ppat_desc (Ppat_constant (Pconst_integer 0)))))
                         (pc_rhs
                          ((pexp_desc
                            (Pexp_tuple
                             (((pexp_desc (Pexp_constant (Pconst_integer 0))))
                              ((pexp_desc (Pexp_constant (Pconst_integer 1))))))))))
                        ((pc_lhs
                          ((ppat_desc (Ppat_constant (Pconst_integer 1)))))
                         (pc_rhs
                          ((pexp_desc
                            (Pexp_tuple
                             (((pexp_desc (Pexp_constant (Pconst_integer 1))))
                              ((pexp_desc (Pexp_constant (Pconst_integer 0))))))))))
                        ((pc_lhs ((ppat_desc Ppat_any)))
                         (pc_rhs
                          ((pexp_desc
                            (Pexp_assert
                             ((pexp_desc (Pexp_construct ((Lident false)))))))))))))))))
                 ((pexp_desc
                   (Pexp_apply ((pexp_desc (Pexp_ident ((Lident +)))))
                    ((Nolabel ((pexp_desc (Pexp_ident ((Lident x))))))
                     (Nolabel ((pexp_desc (Pexp_ident ((Lident y))))))))))))))))))))))) |}]
