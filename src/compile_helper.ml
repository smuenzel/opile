open! Core
open! Compiler_with_sexp

type t =
  { parsetree : Parsetree.structure
  ; typedtree : Typedtree.structure
  ; lambda : Lambda.program
  ; simplif_lambda : Lambda.lambda
  ; clambda_convert : Clambda.ulambda * Clambda.preallocated_block list * Clambda.preallocated_constant list
  ; cmm : Cmm.phrase list
  ; asm : string
  } [@@deriving sexp_of]

let ld_library_path_contents () =
  match Sys.getenv_exn "CAML_LD_LIBRARY_PATH" with
  | s ->
    Misc.split_path_contents s

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

let compile_structure str =
  Clflags.native_code := true;
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
    Simplif.simplify_lambda lambda.code
  in
  let clambda_convert =
    Flambda_middle_end.lambda_to_clambda
      ~ppf_dump:empty_formatter
      ~prefixname:""
      ~backend
      ~filename:"test.ml"
      { lambda with code = simplif_lambda
      }
  in
  (* {[
       let export_info =
         Build_export_info.build_transient ~backend flambda
       in
       let clambda_convert = 
         Flambda_to_clambda.convert (flambda,export_info)
       in
     ]} *)
  (* {[
       let un_anf_clambda =
         Un_anf.apply
           ~ppf_dump:empty_formatter
           (Tuple3.get1 clambda_convert)
           ~what:(Symbol.of_variable (Variable.create Internal_variable_names.unit))
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
           ~what:(Symbol.of_variable (Variable.create Internal_variable_names.unit))
       and do_un_anf_preallocated_constant : Clambda.preallocated_constant -> _ = function
         | a -> a
       in
       let un_anf_functions =
         (Tuple3.get3 clambda_convert)
         |> List.map
           ~f:do_un_anf_preallocated_constant
       in
     ]} *)
  let cmm : Cmm.phrase list =
    (* {[
         let constants =
           List.map ~f:(fun (symbol, definition) ->
               { Clambda.symbol = Linkage_name.to_string (Symbol.label symbol);
                 exported = true;
                 definition;
                 provenance = None;
               })
             (Symbol.Map.bindings un_anf_functions)
         in
       ]} *)
    Cmmgen.compunit
      clambda_convert
      (* {[ (un_anf_clambda, clambda_convert.preallocated_blocks, constants) ]} *)
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
  { parsetree
  ; typedtree
  ; lambda
  ; simplif_lambda
  ; clambda_convert
  ; cmm 
  ; asm
  }
;;
