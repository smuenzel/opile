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
    parsetree
    ------
    (((pstr_desc
       (Pstr_value Nonrecursive
        (((pvb_expr
           ((pexp_desc
             (Pexp_fun Nolabel
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
                                 ((pexp_desc
                                   (Pexp_apply
                                    ((pexp_desc (Pexp_ident ((Lident -)))))
                                    ((Nolabel
                                      ((pexp_desc (Pexp_ident ((Lident z))))))
                                     (Nolabel
                                      ((pexp_desc
                                        (Pexp_constant (Pconst_integer 1)))))))))))))))
                           ((pc_lhs
                             ((ppat_desc (Ppat_constant (Pconst_integer 1)))))
                            (pc_rhs
                             ((pexp_desc
                               (Pexp_tuple
                                (((pexp_desc
                                   (Pexp_constant (Pconst_integer 122))))
                                 ((pexp_desc (Pexp_constant (Pconst_integer 0))))))))))
                           ((pc_lhs ((ppat_desc Ppat_any)))
                            (pc_rhs
                             ((pexp_desc
                               (Pexp_assert
                                ((pexp_desc (Pexp_construct ((Lident false)))))))))))))))))
                    ((pexp_desc
                      (Pexp_construct ((Lident Some))
                       (((pexp_desc
                          (Pexp_apply ((pexp_desc (Pexp_ident ((Lident +)))))
                           ((Nolabel ((pexp_desc (Pexp_ident ((Lident x))))))
                            (Nolabel ((pexp_desc (Pexp_ident ((Lident y))))))))))))))))))))))))))))))

    typedtree
    ------
    ((str_items
      (((str_desc
         (Tstr_value Nonrecursive
          (((vb_pat
             ((pat_desc (Tpat_var f_80))
              (pat_type
               ((desc
                 (Tlink
                  ((desc
                    (Tarrow Nolabel
                     ((desc
                       (Tlink
                        ((desc (Tconstr (Pident int) Mnil)) (level 100000000)
                         (scope 0) (id 808))))
                      (level 1) (scope 0) (id 780))
                     ((desc
                       (Tlink
                        ((desc
                          (Tarrow Nolabel
                           ((desc
                             (Tlink
                              ((desc (Tconstr (Pident unit) Mnil))
                               (level 100000000) (scope 0) (id 788))))
                            (level 1) (scope 0) (id 785))
                           ((desc
                             (Tlink
                              ((desc
                                (Tconstr (Pident option)
                                 (((desc
                                    (Tlink
                                     ((desc (Tconstr (Pident int) Mnil))
                                      (level 100000000) (scope 0) (id 830))))
                                   (level 1) (scope 0) (id 826)))
                                 Mnil))
                               (level 100000000) (scope 0) (id 825))))
                            (level 1) (scope 0) (id 786))
                           Cok))
                         (level 100000000) (scope 0) (id 787))))
                      (level 1) (scope 0) (id 781))
                     Cok))
                   (level 100000000) (scope 0) (id 782))))
                (level 1) (scope 0) (id 779)))
              (pat_env <opaque>)))
            (vb_expr
             ((exp_desc
               (Texp_function (arg_label Nolabel) (param z_81)
                (cases
                 (((c_lhs
                    ((pat_desc (Tpat_var z_81))
                     (pat_type
                      ((desc
                        (Tlink
                         ((desc (Tconstr (Pident int) Mnil)) (level 100000000)
                          (scope 0) (id 808))))
                       (level 1) (scope 0) (id 780)))
                     (pat_env <opaque>)))
                   (c_rhs
                    ((exp_desc
                      (Texp_function (arg_label Nolabel) (param param_84)
                       (cases
                        (((c_lhs
                           ((pat_desc
                             (Tpat_construct ((Lident "()"))
                              ((cstr_name "()")
                               (cstr_res
                                ((desc (Tconstr (Pident unit) Mnil))
                                 (level 100000000) (scope 0) (id 19)))
                               (cstr_arity 0) (cstr_tag (Cstr_constant 0))
                               (cstr_consts 1) (cstr_nonconsts 0) (cstr_normal 1)
                               (cstr_generalized false) (cstr_private Public))))
                            (pat_type
                             ((desc (Tconstr (Pident unit) Mnil))
                              (level 100000000) (scope 0) (id 788)))
                            (pat_env <opaque>)))
                          (c_rhs
                           ((exp_desc
                             (Texp_let Nonrecursive
                              (((vb_pat
                                 ((pat_desc
                                   (Tpat_tuple
                                    (((pat_desc (Tpat_var x_82))
                                      (pat_type
                                       ((desc
                                         (Tlink
                                          ((desc (Tconstr (Pident int) Mnil))
                                           (level 100000000) (scope 0) (id 802))))
                                        (level 3) (scope 0) (id 791)))
                                      (pat_env <opaque>))
                                     ((pat_desc (Tpat_var y_83))
                                      (pat_type
                                       ((desc
                                         (Tlink
                                          ((desc (Tconstr (Pident int) Mnil))
                                           (level 100000000) (scope 0) (id 806))))
                                        (level 3) (scope 0) (id 792)))
                                      (pat_env <opaque>)))))
                                  (pat_type
                                   ((desc
                                     (Ttuple
                                      (((desc
                                         (Tlink
                                          ((desc (Tconstr (Pident int) Mnil))
                                           (level 100000000) (scope 0) (id 802))))
                                        (level 3) (scope 0) (id 791))
                                       ((desc
                                         (Tlink
                                          ((desc (Tconstr (Pident int) Mnil))
                                           (level 100000000) (scope 0) (id 806))))
                                        (level 3) (scope 0) (id 792)))))
                                    (level 100000000) (scope 0) (id 794)))
                                  (pat_env <opaque>)))
                                (vb_expr
                                 ((exp_desc
                                   (Texp_match
                                    ((exp_desc
                                      (Texp_ident (Pident z_81) ((Lident z))
                                       ((val_type
                                         ((desc
                                           (Tlink
                                            ((desc (Tconstr (Pident int) Mnil))
                                             (level 100000000) (scope 0)
                                             (id 808))))
                                          (level 1) (scope 0) (id 780)))
                                        (val_kind Val_reg))))
                                     (exp_type
                                      ((desc
                                        (Tlink
                                         ((desc
                                           (Tlink
                                            ((desc (Tconstr (Pident int) Mnil))
                                             (level 100000000) (scope 0)
                                             (id 808))))
                                          (level 1) (scope 0) (id 796))))
                                       (level 1) (scope 0) (id 783)))
                                     (exp_env <opaque>))
                                    (((c_lhs
                                       ((pat_desc (Tpat_constant (Const_int 0)))
                                        (pat_type
                                         ((desc
                                           (Tlink
                                            ((desc (Tconstr (Pident int) Mnil))
                                             (level 100000000) (scope 0)
                                             (id 808))))
                                          (level 1) (scope 0) (id 796)))
                                        (pat_env <opaque>)))
                                      (c_rhs
                                       ((exp_desc
                                         (Texp_tuple
                                          (((exp_desc
                                             (Texp_constant (Const_int 0)))
                                            (exp_type
                                             ((desc (Tconstr (Pident int) Mnil))
                                              (level 100000000) (scope 0)
                                              (id 802)))
                                            (exp_env <opaque>))
                                           ((exp_desc
                                             (Texp_apply
                                              ((exp_desc
                                                (Texp_ident
                                                 (Pdot (Pident Stdlib_0) -)
                                                 ((Lident -))
                                                 ((val_type
                                                   ((desc
                                                     (Tarrow Nolabel
                                                      ((desc
                                                        (Tconstr (Pident int)
                                                         Mnil))
                                                       (level 100000000)
                                                       (scope 0) (id 201))
                                                      ((desc
                                                        (Tarrow Nolabel
                                                         ((desc
                                                           (Tconstr (Pident int)
                                                            Mnil))
                                                          (level 100000000)
                                                          (scope 0) (id 200))
                                                         ((desc
                                                           (Tconstr (Pident int)
                                                            Mnil))
                                                          (level 100000000)
                                                          (scope 0) (id 199))
                                                         Cok))
                                                       (level 100000000)
                                                       (scope 0) (id 198))
                                                      Cok))
                                                    (level 100000000) (scope 0)
                                                    (id 197)))
                                                  (val_kind
                                                   (Val_prim
                                                    ((prim_name %subint)
                                                     (prim_arity 2)
                                                     (prim_native_name "")
                                                     (prim_native_repr_args
                                                      (Same_as_ocaml_repr
                                                       Same_as_ocaml_repr))
                                                     (prim_native_repr_res
                                                      Same_as_ocaml_repr)))))))
                                               (exp_type
                                                ((desc
                                                  (Tarrow Nolabel
                                                   ((desc
                                                     (Tconstr (Pident int) Mnil))
                                                    (level 100000000) (scope 0)
                                                    (id 808))
                                                   ((desc
                                                     (Tarrow Nolabel
                                                      ((desc
                                                        (Tconstr (Pident int)
                                                         Mnil))
                                                       (level 3) (scope 0)
                                                       (id 807))
                                                      ((desc
                                                        (Tconstr (Pident int)
                                                         Mnil))
                                                       (level 100000000)
                                                       (scope 0) (id 806))
                                                      Cok))
                                                    (level 3) (scope 0) (id 805))
                                                   Cok))
                                                 (level 3) (scope 0) (id 804)))
                                               (exp_env <opaque>))
                                              ((Nolabel
                                                (((exp_desc
                                                   (Texp_ident (Pident z_81)
                                                    ((Lident z))
                                                    ((val_type
                                                      ((desc
                                                        (Tlink
                                                         ((desc
                                                           (Tconstr (Pident int)
                                                            Mnil))
                                                          (level 100000000)
                                                          (scope 0) (id 808))))
                                                       (level 1) (scope 0)
                                                       (id 780)))
                                                     (val_kind Val_reg))))
                                                  (exp_type
                                                   ((desc
                                                     (Tlink
                                                      ((desc
                                                        (Tconstr (Pident int)
                                                         Mnil))
                                                       (level 100000000)
                                                       (scope 0) (id 808))))
                                                    (level 1) (scope 0) (id 796)))
                                                  (exp_env <opaque>))))
                                               (Nolabel
                                                (((exp_desc
                                                   (Texp_constant (Const_int 1)))
                                                  (exp_type
                                                   ((desc
                                                     (Tlink
                                                      ((desc
                                                        (Tconstr (Pident int)
                                                         Mnil))
                                                       (level 3) (scope 0)
                                                       (id 807))))
                                                    (level 4) (scope 0) (id 811)))
                                                  (exp_env <opaque>)))))))
                                            (exp_type
                                             ((desc (Tconstr (Pident int) Mnil))
                                              (level 100000000) (scope 0)
                                              (id 806)))
                                            (exp_env <opaque>)))))
                                        (exp_type
                                         ((desc
                                           (Ttuple
                                            (((desc
                                               (Tlink
                                                ((desc
                                                  (Tconstr (Pident int) Mnil))
                                                 (level 100000000) (scope 0)
                                                 (id 802))))
                                              (level 3) (scope 0) (id 791))
                                             ((desc
                                               (Tlink
                                                ((desc
                                                  (Tconstr (Pident int) Mnil))
                                                 (level 100000000) (scope 0)
                                                 (id 806))))
                                              (level 3) (scope 0) (id 792)))))
                                          (level 100000000) (scope 0) (id 794)))
                                        (exp_env <opaque>))))
                                     ((c_lhs
                                       ((pat_desc (Tpat_constant (Const_int 1)))
                                        (pat_type
                                         ((desc
                                           (Tlink
                                            ((desc
                                              (Tlink
                                               ((desc
                                                 (Tconstr (Pident int) Mnil))
                                                (level 100000000) (scope 0)
                                                (id 808))))
                                             (level 1) (scope 0) (id 796))))
                                          (level 4) (scope 0) (id 797)))
                                        (pat_env <opaque>)))
                                      (c_rhs
                                       ((exp_desc
                                         (Texp_tuple
                                          (((exp_desc
                                             (Texp_constant (Const_int 122)))
                                            (exp_type
                                             ((desc
                                               (Tlink
                                                ((desc
                                                  (Tconstr (Pident int) Mnil))
                                                 (level 100000000) (scope 0)
                                                 (id 802))))
                                              (level 3) (scope 0) (id 817)))
                                            (exp_env <opaque>))
                                           ((exp_desc
                                             (Texp_constant (Const_int 0)))
                                            (exp_type
                                             ((desc
                                               (Tlink
                                                ((desc
                                                  (Tconstr (Pident int) Mnil))
                                                 (level 100000000) (scope 0)
                                                 (id 806))))
                                              (level 3) (scope 0) (id 818)))
                                            (exp_env <opaque>)))))
                                        (exp_type
                                         ((desc
                                           (Ttuple
                                            (((desc
                                               (Tlink
                                                ((desc
                                                  (Tconstr (Pident int) Mnil))
                                                 (level 100000000) (scope 0)
                                                 (id 802))))
                                              (level 3) (scope 0) (id 791))
                                             ((desc
                                               (Tlink
                                                ((desc
                                                  (Tconstr (Pident int) Mnil))
                                                 (level 100000000) (scope 0)
                                                 (id 806))))
                                              (level 3) (scope 0) (id 792)))))
                                          (level 100000000) (scope 0) (id 794)))
                                        (exp_env <opaque>))))
                                     ((c_lhs
                                       ((pat_desc Tpat_any)
                                        (pat_type
                                         ((desc
                                           (Tlink
                                            ((desc (Tconstr (Pident int) Mnil))
                                             (level 100000000) (scope 0)
                                             (id 808))))
                                          (level 1) (scope 0) (id 796)))
                                        (pat_env <opaque>)))
                                      (c_rhs
                                       ((exp_desc
                                         (Texp_assert
                                          ((exp_desc
                                            (Texp_construct ((Lident false))
                                             ((cstr_name false)
                                              (cstr_res
                                               ((desc
                                                 (Tconstr (Pident bool) Mnil))
                                                (level 100000000) (scope 0)
                                                (id 18)))
                                              (cstr_arity 0)
                                              (cstr_tag (Cstr_constant 0))
                                              (cstr_consts 2) (cstr_nonconsts 0)
                                              (cstr_normal 2)
                                              (cstr_generalized false)
                                              (cstr_private Public))))
                                           (exp_type
                                            ((desc
                                              (Tlink
                                               ((desc
                                                 (Tconstr (Pident bool) Mnil))
                                                (level 3) (scope 0) (id 821))))
                                             (level 3) (scope 0) (id 820)))
                                           (exp_env <opaque>))))
                                        (exp_type
                                         ((desc
                                           (Ttuple
                                            (((desc
                                               (Tlink
                                                ((desc
                                                  (Tconstr (Pident int) Mnil))
                                                 (level 100000000) (scope 0)
                                                 (id 802))))
                                              (level 3) (scope 0) (id 791))
                                             ((desc
                                               (Tlink
                                                ((desc
                                                  (Tconstr (Pident int) Mnil))
                                                 (level 100000000) (scope 0)
                                                 (id 806))))
                                              (level 3) (scope 0) (id 792)))))
                                          (level 100000000) (scope 0) (id 794)))
                                        (exp_env <opaque>)))))
                                    Total))
                                  (exp_type
                                   ((desc
                                     (Ttuple
                                      (((desc
                                         (Tlink
                                          ((desc (Tconstr (Pident int) Mnil))
                                           (level 100000000) (scope 0) (id 802))))
                                        (level 3) (scope 0) (id 791))
                                       ((desc
                                         (Tlink
                                          ((desc (Tconstr (Pident int) Mnil))
                                           (level 100000000) (scope 0) (id 806))))
                                        (level 3) (scope 0) (id 792)))))
                                    (level 100000000) (scope 0) (id 794)))
                                  (exp_env <opaque>)))))
                              ((exp_desc
                                (Texp_construct ((Lident Some))
                                 ((cstr_name Some)
                                  (cstr_res
                                   ((desc
                                     (Tconstr (Pident option)
                                      (((level 100000000) (scope 0) (id 16)))
                                      Mnil))
                                    (level 100000000) (scope 0) (id 21)))
                                  (cstr_args
                                   (((level 100000000) (scope 0) (id 16))))
                                  (cstr_arity 1) (cstr_tag (Cstr_block 0))
                                  (cstr_consts 1) (cstr_nonconsts 1)
                                  (cstr_normal 2) (cstr_generalized false)
                                  (cstr_private Public))
                                 (((exp_desc
                                    (Texp_apply
                                     ((exp_desc
                                       (Texp_ident (Pdot (Pident Stdlib_0) +)
                                        ((Lident +))
                                        ((val_type
                                          ((desc
                                            (Tarrow Nolabel
                                             ((desc (Tconstr (Pident int) Mnil))
                                              (level 100000000) (scope 0)
                                              (id 196))
                                             ((desc
                                               (Tarrow Nolabel
                                                ((desc
                                                  (Tconstr (Pident int) Mnil))
                                                 (level 100000000) (scope 0)
                                                 (id 195))
                                                ((desc
                                                  (Tconstr (Pident int) Mnil))
                                                 (level 100000000) (scope 0)
                                                 (id 194))
                                                Cok))
                                              (level 100000000) (scope 0)
                                              (id 193))
                                             Cok))
                                           (level 100000000) (scope 0) (id 192)))
                                         (val_kind
                                          (Val_prim
                                           ((prim_name %addint) (prim_arity 2)
                                            (prim_native_name "")
                                            (prim_native_repr_args
                                             (Same_as_ocaml_repr
                                              Same_as_ocaml_repr))
                                            (prim_native_repr_res
                                             Same_as_ocaml_repr)))))))
                                      (exp_type
                                       ((desc
                                         (Tarrow Nolabel
                                          ((desc (Tconstr (Pident int) Mnil))
                                           (level 2) (scope 0) (id 832))
                                          ((desc
                                            (Tarrow Nolabel
                                             ((desc (Tconstr (Pident int) Mnil))
                                              (level 2) (scope 0) (id 831))
                                             ((desc (Tconstr (Pident int) Mnil))
                                              (level 100000000) (scope 0)
                                              (id 830))
                                             Cok))
                                           (level 2) (scope 0) (id 829))
                                          Cok))
                                        (level 2) (scope 0) (id 828)))
                                      (exp_env <opaque>))
                                     ((Nolabel
                                       (((exp_desc
                                          (Texp_ident (Pident x_82) ((Lident x))
                                           ((val_type
                                             ((desc
                                               (Tlink
                                                ((desc
                                                  (Tconstr (Pident int) Mnil))
                                                 (level 100000000) (scope 0)
                                                 (id 802))))
                                              (level 3) (scope 0) (id 791)))
                                            (val_kind Val_reg))))
                                         (exp_type
                                          ((desc
                                            (Tlink
                                             ((desc (Tconstr (Pident int) Mnil))
                                              (level 2) (scope 0) (id 832))))
                                           (level 3) (scope 0) (id 835)))
                                         (exp_env <opaque>))))
                                      (Nolabel
                                       (((exp_desc
                                          (Texp_ident (Pident y_83) ((Lident y))
                                           ((val_type
                                             ((desc
                                               (Tlink
                                                ((desc
                                                  (Tconstr (Pident int) Mnil))
                                                 (level 100000000) (scope 0)
                                                 (id 806))))
                                              (level 3) (scope 0) (id 792)))
                                            (val_kind Val_reg))))
                                         (exp_type
                                          ((desc
                                            (Tlink
                                             ((desc (Tconstr (Pident int) Mnil))
                                              (level 2) (scope 0) (id 831))))
                                           (level 3) (scope 0) (id 836)))
                                         (exp_env <opaque>)))))))
                                   (exp_type
                                    ((desc (Tconstr (Pident int) Mnil))
                                     (level 100000000) (scope 0) (id 830)))
                                   (exp_env <opaque>)))))
                               (exp_type
                                ((desc
                                  (Tconstr (Pident option)
                                   (((desc
                                      (Tlink
                                       ((desc (Tconstr (Pident int) Mnil))
                                        (level 100000000) (scope 0) (id 830))))
                                     (level 1) (scope 0) (id 826)))
                                   Mnil))
                                 (level 100000000) (scope 0) (id 825)))
                               (exp_env <opaque>))))
                            (exp_type
                             ((desc
                               (Tconstr (Pident option)
                                (((desc
                                   (Tlink
                                    ((desc (Tconstr (Pident int) Mnil))
                                     (level 100000000) (scope 0) (id 830))))
                                  (level 1) (scope 0) (id 826)))
                                Mnil))
                              (level 100000000) (scope 0) (id 825)))
                            (exp_env <opaque>))))))
                       (partial Total)))
                     (exp_type
                      ((desc
                        (Tarrow Nolabel
                         ((desc
                           (Tlink
                            ((desc (Tconstr (Pident unit) Mnil))
                             (level 100000000) (scope 0) (id 788))))
                          (level 1) (scope 0) (id 785))
                         ((desc
                           (Tlink
                            ((desc
                              (Tconstr (Pident option)
                               (((desc
                                  (Tlink
                                   ((desc (Tconstr (Pident int) Mnil))
                                    (level 100000000) (scope 0) (id 830))))
                                 (level 1) (scope 0) (id 826)))
                               Mnil))
                             (level 100000000) (scope 0) (id 825))))
                          (level 1) (scope 0) (id 786))
                         Cok))
                       (level 100000000) (scope 0) (id 787)))
                     (exp_env <opaque>))))))
                (partial Total)))
              (exp_type
               ((desc
                 (Tarrow Nolabel
                  ((desc (Tconstr (Pident int) Mnil)) (level 100000000) (scope 0)
                   (id 808))
                  ((desc
                    (Tarrow Nolabel
                     ((desc
                       (Tlink
                        ((desc (Tconstr (Pident unit) Mnil)) (level 100000000)
                         (scope 0) (id 788))))
                      (level 1) (scope 0) (id 785))
                     ((desc
                       (Tlink
                        ((desc
                          (Tconstr (Pident option)
                           (((desc
                              (Tlink
                               ((desc (Tconstr (Pident int) Mnil))
                                (level 100000000) (scope 0) (id 830))))
                             (level 1) (scope 0) (id 826)))
                           Mnil))
                         (level 100000000) (scope 0) (id 825))))
                      (level 1) (scope 0) (id 786))
                     Cok))
                   (level 100000000) (scope 0) (id 787))
                  Cok))
                (level 100000000) (scope 0) (id 842)))
              (exp_env <opaque>)))))))
        (str_env <opaque>))))
     (str_type
      ((Sig_value f_80
        ((val_type
          ((desc
            (Tlink
             ((desc
               (Tarrow Nolabel
                ((desc
                  (Tlink
                   ((desc (Tconstr (Pident int) Mnil)) (level 100000000)
                    (scope 0) (id 808))))
                 (level 1) (scope 0) (id 780))
                ((desc
                  (Tlink
                   ((desc
                     (Tarrow Nolabel
                      ((desc
                        (Tlink
                         ((desc (Tconstr (Pident unit) Mnil)) (level 100000000)
                          (scope 0) (id 788))))
                       (level 1) (scope 0) (id 785))
                      ((desc
                        (Tlink
                         ((desc
                           (Tconstr (Pident option)
                            (((desc
                               (Tlink
                                ((desc (Tconstr (Pident int) Mnil))
                                 (level 100000000) (scope 0) (id 830))))
                              (level 1) (scope 0) (id 826)))
                            Mnil))
                          (level 100000000) (scope 0) (id 825))))
                       (level 1) (scope 0) (id 786))
                      Cok))
                    (level 100000000) (scope 0) (id 787))))
                 (level 1) (scope 0) (id 781))
                Cok))
              (level 100000000) (scope 0) (id 782))))
           (level 1) (scope 0) (id 779)))
         (val_kind Val_reg))
        Exported)))
     (str_final_env <opaque>))

    lambda
    ------
    ((module_ident Test_0) (main_module_block_size 1)
     (required_globals (Assert_failure))
     (code
      (Llet Strict Pgenval f_80
       (Lfunction
        ((kind Curried) (params ((z_81 Pintval) (param_84 Pgenval)))
         (return Pgenval)
         (body
          (Lstaticcatch
           (Lstaticcatch
            (Lifthenelse
             (Lprim (Pintcomp Cne)
              ((Lvar z_81) (Lconst (Const_base (Const_int 0)))))
             (Lifthenelse
              (Lprim (Pintcomp Cne)
               ((Lvar z_81) (Lconst (Const_base (Const_int 1)))))
              (Lstaticraise 1)
              (Llet Strict Pgenval y_88 (Lconst (Const_base (Const_int 0)))
               (Llet Strict Pgenval x_87 (Lconst (Const_base (Const_int 122)))
                (Lstaticraise 2 ((Lvar y_88) (Lvar x_87))))))
             (Llet Strict Pgenval y_86
              (Lprim Psubint ((Lvar z_81) (Lconst (Const_base (Const_int 1)))))
              (Llet Strict Pgenval x_85 (Lconst (Const_base (Const_int 0)))
               (Lstaticraise 2 ((Lvar y_86) (Lvar x_85))))))
            (Lprim (Praise Raise_regular)
             ((Lprim (Pmakeblock 0 Immutable)
               ((Lprim (Pgetglobal Assert_failure))
                (Lconst
                 (Const_block 0
                  ((Const_base (Const_string "")) (Const_base (Const_int 7))
                   (Const_base (Const_int 15))))))))))
           (2 ((y_83 Pintval) (x_82 Pintval)))
           (Lprim (Pmakeblock 0 Immutable ((Pintval)))
            ((Lprim Paddint ((Lvar x_82) (Lvar y_83)))))))
         (attr
          ((inline Default_inline) (specialise Default_specialise)
           (local Default_local) (is_a_functor false) (stub false)))))
       (Lprim (Pmakeblock 0 Immutable) ((Lvar f_80))))))

    simplif_lambda
    ------
    (Llet Strict Pgenval f_80
     (Lfunction
      ((kind Curried) (params ((z_81 Pintval) (param_84 Pgenval)))
       (return Pgenval)
       (body
        (Lstaticcatch
         (Lifthenelse
          (Lprim (Pintcomp Cne)
           ((Lvar z_81) (Lconst (Const_base (Const_int 0)))))
          (Lifthenelse
           (Lprim (Pintcomp Cne)
            ((Lvar z_81) (Lconst (Const_base (Const_int 1)))))
           (Lprim (Praise Raise_regular)
            ((Lprim (Pmakeblock 0 Immutable)
              ((Lprim (Pgetglobal Assert_failure))
               (Lconst
                (Const_block 0
                 ((Const_base (Const_string "")) (Const_base (Const_int 7))
                  (Const_base (Const_int 15)))))))))
           (Llet Strict Pgenval y_88 (Lconst (Const_base (Const_int 0)))
            (Llet Strict Pgenval x_87 (Lconst (Const_base (Const_int 122)))
             (Lstaticraise 2 ((Lvar y_88) (Lvar x_87))))))
          (Llet Strict Pgenval y_86
           (Lprim Psubint ((Lvar z_81) (Lconst (Const_base (Const_int 1)))))
           (Llet Strict Pgenval x_85 (Lconst (Const_base (Const_int 0)))
            (Lstaticraise 2 ((Lvar y_86) (Lvar x_85))))))
         (2 ((y_83 Pintval) (x_82 Pintval)))
         (Lprim (Pmakeblock 0 Immutable ((Pintval)))
          ((Lprim Paddint ((Lvar x_82) (Lvar y_83)))))))
       (attr
        ((inline Default_inline) (specialise Default_specialise)
         (local Default_local) (is_a_functor false) (stub false)))))
     (Lprim (Pmakeblock 0 Immutable) ((Lvar f_80))))

    flambda
    ------
    ((imported_symbols (caml_exn_Assert_failure))
     (program_body
      (Let_symbol camlTest__const_string_36 (Allocated_const (String ""))
       (Let_symbol camlTest__const_block_37
        (Block 0
         ((Symbol camlTest__const_string_36) (Const (Int 7)) (Const (Int 15))))
        (Let_symbol camlTest__Pmakeblock_46
         (Block 0
          ((Symbol caml_exn_Assert_failure) (Symbol camlTest__const_block_37)))
         (Let_symbol camlTest__f_45
          (Set_of_closures
           ((function_decls
             ((is_classic_mode false) (set_of_closures_id Test.11)
              (set_of_closures_origin Test.1)
              (funs
               ((f_5
                 ((closure_origin f/9) (params (z/8 param/7))
                  (body
                   (Let
                    ((var Pintcomp_arg_16) (defining_expr (Const (Int 0)))
                     (body
                      (Let
                       ((var Pintcomp_17)
                        (defining_expr
                         (Prim (Pintcomp Cne) (z_8 Pintcomp_arg_16)))
                        (body
                         (Static_catch 7 (y_10 x_11)
                          (If_then_else Pintcomp_17
                           (Let
                            ((var Pintcomp_arg_26)
                             (defining_expr (Const (Int 1)))
                             (body
                              (Let
                               ((var Pintcomp_27)
                                (defining_expr
                                 (Prim (Pintcomp Cne) (z_8 Pintcomp_arg_26)))
                                (body
                                 (If_then_else Pintcomp_27
                                  (Let
                                   ((var raise_arg_33)
                                    (defining_expr
                                     (Symbol camlTest__Pmakeblock_46))
                                    (body
                                     (Let
                                      ((var raise_34)
                                       (defining_expr
                                        (Prim (Praise Raise_regular)
                                         (raise_arg_33)))
                                       (body (Var raise_34))
                                       (free_vars_of_defining_expr
                                        (raise_arg_33))
                                       (free_vars_of_body (raise_34)))))
                                    (free_vars_of_body (raise_arg_33))))
                                  (Let
                                   ((var y_29) (defining_expr (Const (Int 0)))
                                    (body
                                     (Let
                                      ((var x_31)
                                       (defining_expr (Const (Int 122)))
                                       (body (Static_raise 7 (y_29 x_31)))
                                       (free_vars_of_body (x_31 y_29)))))
                                    (free_vars_of_body (y_29))))))
                                (free_vars_of_defining_expr
                                 (Pintcomp_arg_26 z_8))
                                (free_vars_of_body (Pintcomp_27)))))
                             (free_vars_of_body (Pintcomp_arg_26 z_8))))
                           (Let
                            ((var Psubint_arg_21) (defining_expr (Const (Int 1)))
                             (body
                              (Let
                               ((var Psubint_22)
                                (defining_expr
                                 (Prim Psubint (z_8 Psubint_arg_21)))
                                (body
                                 (Let
                                  ((var x_23) (defining_expr (Const (Int 0)))
                                   (body (Static_raise 7 (Psubint_22 x_23)))
                                   (free_vars_of_body (x_23 Psubint_22)))))
                                (free_vars_of_defining_expr (Psubint_arg_21 z_8))
                                (free_vars_of_body (Psubint_22)))))
                             (free_vars_of_body (Psubint_arg_21 z_8)))))
                          (Let
                           ((var Pmakeblock_arg_13)
                            (defining_expr (Prim Paddint (x_11 y_10)))
                            (body
                             (Let
                              ((var Pmakeblock_14)
                               (defining_expr
                                (Prim (Pmakeblock 0 Immutable ((Pintval)))
                                 (Pmakeblock_arg_13)))
                               (body (Var Pmakeblock_14))
                               (free_vars_of_defining_expr (Pmakeblock_arg_13))
                               (free_vars_of_body (Pmakeblock_14)))))
                            (free_vars_of_defining_expr (x_11 y_10))
                            (free_vars_of_body (Pmakeblock_arg_13))))))
                        (free_vars_of_defining_expr (Pintcomp_arg_16 z_8))
                        (free_vars_of_body (Pintcomp_17 z_8)))))
                     (free_vars_of_body (Pintcomp_arg_16 z_8)))))
                  (free_variables (z_8)) (free_symbols (camlTest__Pmakeblock_46))
                  (stub false) (inline Default_inline)
                  (specialise Default_specialise) (is_a_functor false)))))))))
          (Let_symbol camlTest__f_5_closure (Project_closure camlTest__f_45 f_5)
           (Let_symbol camlTest (Block 0 ((Symbol camlTest__f_5_closure)))
            (End camlTest)))))))))

    clambda_convert
    ------
    ((expr (Uconst (Uconst_ptr 0)))
     (structured_constants
      ((camlTest (Uconst_block 0 ((Uconst_ref camlTest__f_5_closure))))
       (camlTest__Pmakeblock_46
        (Uconst_block 0
         ((Uconst_ref caml_exn_Assert_failure)
          (Uconst_ref camlTest__const_block_37))))
       (camlTest__f_45
        (Uconst_closure
         (((label camlTest__f_5) (arity 2)
           (params ((z/91 Pgenval) (param/90 Pgenval))) (return Pgenval)
           (body
            (Ulet Immutable Pgenval Pintcomp_arg/92 (Uconst (Uconst_int 0))
             (Ulet Immutable Pgenval Pintcomp/93
              (Uprim (Pintcomp Cne) ((Uvar z/91) (Uvar Pintcomp_arg/92)))
              (Ucatch 7 ((y/95 Pgenval) (x/94 Pgenval))
               (Uifthenelse (Uvar Pintcomp/93)
                (Ulet Immutable Pgenval Pintcomp_arg/101 (Uconst (Uconst_int 1))
                 (Ulet Immutable Pgenval Pintcomp/102
                  (Uprim (Pintcomp Cne) ((Uvar z/91) (Uvar Pintcomp_arg/101)))
                  (Uifthenelse (Uvar Pintcomp/102)
                   (Ulet Immutable Pgenval raise_arg/105
                    (Uconst (Uconst_ref camlTest__Pmakeblock_46))
                    (Ulet Immutable Pgenval raise/106
                     (Uprim (Praise Raise_regular) ((Uvar raise_arg/105)))
                     (Uvar raise/106)))
                   (Ulet Immutable Pgenval y/103 (Uconst (Uconst_int 0))
                    (Ulet Immutable Pgenval x/104 (Uconst (Uconst_int 122))
                     (Ustaticfail 7 ((Uvar y/103) (Uvar x/104))))))))
                (Ulet Immutable Pgenval Psubint_arg/98 (Uconst (Uconst_int 1))
                 (Ulet Immutable Pgenval Psubint/99
                  (Uprim Psubint ((Uvar z/91) (Uvar Psubint_arg/98)))
                  (Ulet Immutable Pgenval x/100 (Uconst (Uconst_int 0))
                   (Ustaticfail 7 ((Uvar Psubint/99) (Uvar x/100)))))))
               (Ulet Immutable Pgenval Pmakeblock_arg/96
                (Uprim Paddint ((Uvar x/94) (Uvar y/95)))
                (Ulet Immutable Pgenval Pmakeblock/97
                 (Uprim (Pmakeblock 0 Immutable ((Pintval)))
                  ((Uvar Pmakeblock_arg/96)))
                 (Uvar Pmakeblock/97)))))))))
         camlTest__f_45))
       (camlTest__const_block_37
        (Uconst_block 0
         ((Uconst_ref camlTest__const_string_36) (Uconst_int 7) (Uconst_int 15))))
       (camlTest__const_string_36 (Uconst_string ""))))
     (exported
      ((sets_of_closures
        ((Test.11
          ((is_classic_mode false) (set_of_closures_id Test.11)
           (set_of_closures_origin Test.1)
           (funs
            ((f_5
              ((closure_origin f/9) (params (z/8 param/7))
               (function_body
                (((free_variables (z_8)) (free_symbols (camlTest__Pmakeblock_46))
                  (stub false) (inline Default_inline)
                  (specialise Default_specialise) (is_a_functor false)
                  (body
                   (Let
                    ((var Pintcomp_arg_16) (defining_expr (Const (Int 0)))
                     (body
                      (Let
                       ((var Pintcomp_17)
                        (defining_expr
                         (Prim (Pintcomp Cne) (z_8 Pintcomp_arg_16)))
                        (body
                         (Static_catch 7 (y_10 x_11)
                          (If_then_else Pintcomp_17
                           (Let
                            ((var Pintcomp_arg_26)
                             (defining_expr (Const (Int 1)))
                             (body
                              (Let
                               ((var Pintcomp_27)
                                (defining_expr
                                 (Prim (Pintcomp Cne) (z_8 Pintcomp_arg_26)))
                                (body
                                 (If_then_else Pintcomp_27
                                  (Let
                                   ((var raise_arg_33)
                                    (defining_expr
                                     (Symbol camlTest__Pmakeblock_46))
                                    (body
                                     (Let
                                      ((var raise_34)
                                       (defining_expr
                                        (Prim (Praise Raise_regular)
                                         (raise_arg_33)))
                                       (body (Var raise_34))
                                       (free_vars_of_defining_expr
                                        (raise_arg_33))
                                       (free_vars_of_body (raise_34)))))
                                    (free_vars_of_body (raise_arg_33))))
                                  (Let
                                   ((var y_29) (defining_expr (Const (Int 0)))
                                    (body
                                     (Let
                                      ((var x_31)
                                       (defining_expr (Const (Int 122)))
                                       (body (Static_raise 7 (y_29 x_31)))
                                       (free_vars_of_body (x_31 y_29)))))
                                    (free_vars_of_body (y_29))))))
                                (free_vars_of_defining_expr
                                 (Pintcomp_arg_26 z_8))
                                (free_vars_of_body (Pintcomp_27)))))
                             (free_vars_of_body (Pintcomp_arg_26 z_8))))
                           (Let
                            ((var Psubint_arg_21) (defining_expr (Const (Int 1)))
                             (body
                              (Let
                               ((var Psubint_22)
                                (defining_expr
                                 (Prim Psubint (z_8 Psubint_arg_21)))
                                (body
                                 (Let
                                  ((var x_23) (defining_expr (Const (Int 0)))
                                   (body (Static_raise 7 (Psubint_22 x_23)))
                                   (free_vars_of_body (x_23 Psubint_22)))))
                                (free_vars_of_defining_expr (Psubint_arg_21 z_8))
                                (free_vars_of_body (Psubint_22)))))
                             (free_vars_of_body (Psubint_arg_21 z_8)))))
                          (Let
                           ((var Pmakeblock_arg_13)
                            (defining_expr (Prim Paddint (x_11 y_10)))
                            (body
                             (Let
                              ((var Pmakeblock_14)
                               (defining_expr
                                (Prim (Pmakeblock 0 Immutable ((Pintval)))
                                 (Pmakeblock_arg_13)))
                               (body (Var Pmakeblock_14))
                               (free_vars_of_defining_expr (Pmakeblock_arg_13))
                               (free_vars_of_body (Pmakeblock_14)))))
                            (free_vars_of_defining_expr (x_11 y_10))
                            (free_vars_of_body (Pmakeblock_arg_13))))))
                        (free_vars_of_defining_expr (Pintcomp_arg_16 z_8))
                        (free_vars_of_body (Pintcomp_17 z_8)))))
                     (free_vars_of_body (Pintcomp_arg_16 z_8))))))))))))))))
       (values
        ((Test
          ((Test.10 (Value_block 0 ((Value_symbol camlTest__f_5_closure))))
           (Test.9
            (Value_closure
             ((closure_id f_5)
              (set_of_closures
               ((set_of_closures_id Test.11) (results ((f_5 Value_unknown)))
                (aliased_symbol (camlTest__f_45)))))))
           (Test.6
            (Value_set_of_closures
             ((set_of_closures_id Test.11) (results ((f_5 Value_unknown)))
              (aliased_symbol (camlTest__f_45)))))
           (Test.5
            (Value_block 0
             ((Value_symbol caml_exn_Assert_failure)
              (Value_symbol camlTest__const_block_37))))
           (Test.4 (Value_int 15)) (Test.3 (Value_int 7))
           (Test.2
            (Value_block 0
             ((Value_symbol camlTest__const_string_36) (Value_id Test.3)
              (Value_id Test.4))))
           (Test.1 (Value_string ((contents Unknown_or_mutable) (size 0))))))))
       (symbol_id
        ((camlTest Test.10) (camlTest__f_5_closure Test.9)
         (camlTest__Pmakeblock_46 Test.5) (camlTest__f_45 Test.6)
         (camlTest__const_block_37 Test.2) (camlTest__const_string_36 Test.1)))
       (offset_fun ((f_5 0))) (constant_closures (f_5)))))

    un_anf_clambda
    ------
    ((camlTest (Uconst_block 0 ((Uconst_ref camlTest__f_5_closure))))
     (camlTest__Pmakeblock_46
      (Uconst_block 0
       ((Uconst_ref caml_exn_Assert_failure)
        (Uconst_ref camlTest__const_block_37))))
     (camlTest__f_45
      (Uconst_closure
       (((label camlTest__f_5) (arity 2)
         (params ((z/91 Pgenval) (param/90 Pgenval))) (return Pgenval)
         (body
          (Ucatch 7 ((y/95 Pgenval) (x/94 Pgenval))
           (Uifthenelse
            (Uprim (Pintcomp Cne) ((Uvar z/91) (Uconst (Uconst_int 0))))
            (Uifthenelse
             (Uprim (Pintcomp Cne) ((Uvar z/91) (Uconst (Uconst_int 1))))
             (Uprim (Praise Raise_regular)
              ((Uconst (Uconst_ref camlTest__Pmakeblock_46))))
             (Ustaticfail 7 ((Uconst (Uconst_int 0)) (Uconst (Uconst_int 122)))))
            (Ustaticfail 7
             ((Uprim Psubint ((Uvar z/91) (Uconst (Uconst_int 1))))
              (Uconst (Uconst_int 0)))))
           (Uprim (Pmakeblock 0 Immutable ((Pintval)))
            ((Uprim Paddint ((Uvar x/94) (Uvar y/95)))))))))
       camlTest__f_45))
     (camlTest__const_block_37
      (Uconst_block 0
       ((Uconst_ref camlTest__const_string_36) (Uconst_int 7) (Uconst_int 15))))
     (camlTest__const_string_36 (Uconst_string "")))

    cmm
    ------
    ((Cdata
      ((Cglobal_symbol camlTest__gc_roots) (Cdefine_symbol camlTest__gc_roots)
       (Cint 0)))
     (Cfunction
      ((fun_name camlTest__f_5) (fun_args ((z/91 (Val)) (param/90 (Val))))
       (fun_body
        (Ccatch Nonrecursive
         ((7 ((y/95 (Val)) (x/94 (Val)))
           (Cop Calloc
            ((Cblockheader 1024)
             (Cop Caddi ((Cop Caddi ((Cvar x/94) (Cvar y/95))) (Cconst_int -1)))))))
         (Cifthenelse (Cop (Ccmpi Cne) ((Cvar z/91) (Cconst_int 1)))
          (Cifthenelse (Cop (Ccmpi Cne) ((Cvar z/91) (Cconst_int 3)))
           (Cop (Craise Raise_notrace) ((Cconst_symbol camlTest__Pmakeblock_46)))
           (Cexit 7 ((Cconst_int 1) (Cconst_int 245))))
          (Cexit 7 ((Cop Caddi ((Cvar z/91) (Cconst_int -2))) (Cconst_int 1))))))))
     (Cdata
      ((Cint 4087) (Cglobal_symbol camlTest__f_45)
       (Cdefine_symbol camlTest__f_45) (Csymbol_address caml_curry2) (Cint 5)
       (Csymbol_address camlTest__f_5)))
     (Cdata
      ((Cint 1792) (Cglobal_symbol camlTest) (Cdefine_symbol camlTest)
       (Csymbol_address camlTest__f_5_closure)))
     (Cdata
      ((Cint 2816) (Cglobal_symbol camlTest__Pmakeblock_46)
       (Cdefine_symbol camlTest__Pmakeblock_46)
       (Csymbol_address caml_exn_Assert_failure)
       (Csymbol_address camlTest__const_block_37)))
     (Cdata
      ((Cint 3840) (Cglobal_symbol camlTest__const_block_37)
       (Cdefine_symbol camlTest__const_block_37)
       (Csymbol_address camlTest__const_string_36) (Cint 15) (Cint 31)))
     (Cdata
      ((Cint 2044) (Cglobal_symbol camlTest__const_string_36)
       (Cdefine_symbol camlTest__const_string_36) (Cstring "") (Cskip 7)
       (Cint8 7)))
     (Cfunction
      ((fun_name camlTest__entry) (fun_body (Cconst_pointer 1))
       (fun_codegen_options (Reduce_code_size)))))

    asm
    ------
    	.file ""
    	.section .rodata.cst8,"a",@progbits
    	.align	16
    caml_negf_mask:
    	.quad	0x8000000000000000
    	.quad	0
    	.align	16
    caml_absf_mask:
    	.quad	0x7fffffffffffffff
    	.quad	-1
    	.data
    	.globl	camlTest__data_begin
    camlTest__data_begin:
    	.text
    	.globl	camlTest__code_begin
    camlTest__code_begin:
    	.data
    	.align	8
    	.globl	camlTest__gc_roots
    camlTest__gc_roots:
    	.quad	0
    	.text
    	.align	16
    	.globl	camlTest__f_5
    camlTest__f_5:
    	.cfi_startproc
    	subq	$8, %rsp
    	.cfi_adjust_cfa_offset 8
    .L103:
    	cmpq	$1, %rax
    	je	.L101
    	cmpq	$3, %rax
    	je	.L102
    	movq	camlTest__Pmakeblock_46@GOTPCREL(%rip), %rax
    	movq	%r14, %rsp
    	popq	%r14
    	ret
    	.align	4
    .L102:
    	movq	$245, %rdi
    	movq	$1, %rbx
    	jmp	.L100
    	.align	4
    .L101:
    	movq	$1, %rdi
    	addq	$-2, %rax
    	movq	%rax, %rbx
    .L100:
    .L104:
    	subq	$16, %r15
    	movq	caml_young_limit@GOTPCREL(%rip), %rax
    	cmpq	(%rax), %r15
    	jb	.L105
    	leaq	8(%r15), %rax
    	movq	$1024, -8(%rax)
    	leaq	-1(%rdi,%rbx), %rbx
    	movq	%rbx, (%rax)
    	addq	$8, %rsp
    	.cfi_adjust_cfa_offset -8
    	ret
    	.cfi_adjust_cfa_offset 8
    .L105:
    	call	caml_call_gc@PLT
    .L106:
    	jmp	.L104
    	.cfi_adjust_cfa_offset -8
    	.cfi_endproc
    	.type camlTest__f_5,@function
    	.size camlTest__f_5,. - camlTest__f_5
    	.data
    	.align	8
    	.quad	4087
    	.globl	camlTest__f_45
    camlTest__f_45:
    	.quad	caml_curry2
    	.quad	5
    	.quad	camlTest__f_5
    	.data
    	.align	8
    	.quad	1792
    	.globl	camlTest
    camlTest:
    	.quad	camlTest__f_5_closure
    	.data
    	.align	8
    	.quad	2816
    	.globl	camlTest__Pmakeblock_46
    camlTest__Pmakeblock_46:
    	.quad	caml_exn_Assert_failure
    	.quad	camlTest__const_block_37
    	.data
    	.align	8
    	.data
    	.align	8
    	.quad	3840
    	.globl	camlTest__const_block_37
    camlTest__const_block_37:
    	.quad	camlTest__const_string_36
    	.quad	15
    	.quad	31
    	.data
    	.align	8
    	.quad	2044
    	.globl	camlTest__const_string_36
    camlTest__const_string_36:
    	.ascii	""
    	.space	7
    	.byte	7
    	.text
    	.align	16
    	.globl	camlTest__entry
    camlTest__entry:
    	.cfi_startproc
    .L107:
    	movq	$1, %rax
    	ret
    	.cfi_endproc
    	.type camlTest__entry,@function
    	.size camlTest__entry,. - camlTest__entry
    	.text
    	.globl	camlTest__code_end
    camlTest__code_end:
    	.data
    				/* relocation table start */
    	.align	8
    				/* relocation table end */
    	.data
    	.quad	0
    	.globl	camlTest__data_end
    camlTest__data_end:
    	.quad	0
    	.align	8
    	.globl	camlTest__frametable
    camlTest__frametable:
    	.quad	1
    	.quad	.L106
    	.word	16
    	.word	2
    	.word	3
    	.word	5
    	.align	8
    	.section .note.GNU-stack,"",%progbits |}]
