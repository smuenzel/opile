(* This file is generated from ../raw-compiler/flambda_to_clambda.mli using sexpify *)

open! Core
type result = Compiler_without_sexp.Flambda_to_clambda.result =
  {
  expr: Clambda.ulambda ;
  preallocated_blocks: Clambda.preallocated_block list ;
  structured_constants: Clambda.ustructured_constant Symbol.Map.t ;
  exported: Export_info.t }[@@deriving sexp_of]
include
  (Compiler_without_sexp.Flambda_to_clambda :
    module type of
      struct include Compiler_without_sexp.Flambda_to_clambda end with type
       result :=  Compiler_without_sexp.Flambda_to_clambda.result)
