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
  Load_path.init
    (ld_library_path_contents ())
  ;
  Cmt_format.clear ();
  Typecore.reset_delayed_checks ();
  Env.reset_required_globals ();
  Compilenv.reset "Test";
  let typedtree = 
    let ttstr, _, _, _ =
      try
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
  let lambda =
    Translmod.transl_implementation_flambda "Test" (typedtree,Tcoerce_none)
  in
  let simplif_lambda =
    Simplif.simplify_lambda "Test" lambda.code
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
  let export_info =
    Build_export_info.build_transient ~backend flambda
  in
  let clambda_convert = 
    Flambda_to_clambda.convert (flambda,export_info)
  in
  let un_anf_clambda =
    Un_anf.apply
      ~ppf_dump:empty_formatter
      clambda_convert.expr
      ~what:""
  in
  let rec do_un_anf_ustructured_constant = function
    | Clambda.Uconst_closure (ufl, str, uconst) ->
      Clambda.Uconst_closure
        ( List.map ufl ~f:do_un_anf_ufunction
        , str
        , List.map uconst ~f:do_un_anf_uconstant
        )
    | other -> other
  and do_un_anf_ufunction (uf : Clambda.ufunction) =
    { uf with body = do_un_anf_ulambda uf.body }
  and do_un_anf_uconstant = function
    | Clambda.Uconst_ref (str, Some usc) ->
      Clambda.Uconst_ref (str, Some (do_un_anf_ustructured_constant usc))
    | other -> other
  and do_un_anf_ulambda ul =
    Un_anf.apply
      ~ppf_dump:empty_formatter
      ul
      ~what:""
  in
  let un_anf_functions =
    clambda_convert.structured_constants
    |> Symbol.Map.map
         do_un_anf_ustructured_constant
  in
  let cmm : Cmm.phrase list =
    let constants =
      List.map ~f:(fun (symbol, definition) ->
          { Clambda.symbol = Linkage_name.to_string (Symbol.label symbol);
            exported = true;
            definition;
            provenance = None;
          })
        (Symbol.Map.bindings un_anf_functions)
    in
    Cmmgen.compunit
      ~ppf_dump:empty_formatter
      (un_anf_clambda, clambda_convert.preallocated_blocks, constants)
  in
  let asm =
    Emitaux.reset ();
    let pread, pwrite = Unix.pipe () in
    let cread = Unix.in_channel_of_descr pread in
    let cwrite = Unix.out_channel_of_descr pwrite in
    Emitaux.output_channel := cwrite;
    Emit.begin_assembly ();
    List.iter cmm
      ~f:(Asmgen.compile_phrase ~ppf_dump:empty_formatter)
    ;
    Emit.end_assembly ();
    Out_channel.close cwrite;
    In_channel.input_all cread
  in
  let print_with_name name f =
    print_endline name;
    print_endline "------";
    f ();
    print_endline "";
  in
  let p (type a) name (sexp_of : a -> Sexp.t) thing =
    let sexp = 
      sexp_of thing
      |> clean_sexp
    in
    print_with_name name (fun () -> print_s sexp);
  in
  p "parsetree" [%sexp_of: Parsetree.structure] parsetree;
  p "typedtree" [%sexp_of: Typedtree.structure] typedtree;
  p "lambda" [%sexp_of: Lambda.program] lambda;
  p "simplif_lambda" [%sexp_of: Lambda.lambda] simplif_lambda;
  p "flambda" [%sexp_of: Flambda.program] flambda;
  p "clambda_convert" [%sexp_of: Flambda_to_clambda.result] clambda_convert;
  p "un_anf_clambda" [%sexp_of: Clambda.ustructured_constant Symbol.Map.t] un_anf_functions;
  p "cmm" [%sexp_of: Cmm.phrase list] cmm;
  print_with_name "asm" (fun () -> print_endline asm);
;;

let%expect_test "hello" =
  f {|
    let f z () =
      let x, y =
        match z with
        | 0 -> 0, (z - 1)
        | 1 -> 122, 0
        | _ -> assert false
      in
      Some (x + y)
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
