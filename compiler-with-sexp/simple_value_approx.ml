(* This file is generated from ../raw-compiler/simple_value_approx.mli using sexpify *)

open! Core
type 'a boxed_int = 'a Compiler_without_sexp.Simple_value_approx.boxed_int =
  | Int32: int32 boxed_int 
  | Int64: int64 boxed_int 
  | Nativeint: nativeint boxed_int [@@deriving sexp_of]
type value_string = Compiler_without_sexp.Simple_value_approx.value_string =
  {
  contents: string option ;
  size: int }[@@deriving sexp_of]
type unresolved_value =
  Compiler_without_sexp.Simple_value_approx.unresolved_value =
  | Set_of_closures_id of Set_of_closures_id.t 
  | Symbol of Symbol.t [@@deriving sexp_of]
type unknown_because_of =
  Compiler_without_sexp.Simple_value_approx.unknown_because_of =
  | Unresolved_value of unresolved_value 
  | Other [@@deriving sexp_of]
type t = Compiler_without_sexp.Simple_value_approx.t = private
  {
  descr: descr ;
  var: Variable.t option ;
  symbol: (Symbol.t * int option) option }
and descr = Compiler_without_sexp.Simple_value_approx.descr = private
  | Value_block of Tag.t * t array 
  | Value_int of int 
  | Value_char of char 
  | Value_constptr of int 
  | Value_float of float option 
  | Value_boxed_int: 'a boxed_int * 'a -> descr 
  | Value_set_of_closures of value_set_of_closures 
  | Value_closure of value_closure 
  | Value_string of value_string 
  | Value_float_array of value_float_array 
  | Value_unknown of unknown_because_of 
  | Value_bottom 
  | Value_extern of Export_id.t 
  | Value_symbol of Symbol.t 
  | Value_unresolved of unresolved_value 
and value_closure = Compiler_without_sexp.Simple_value_approx.value_closure =
  {
  set_of_closures: t ;
  closure_id: Closure_id.t }
and function_declarations =
  Compiler_without_sexp.Simple_value_approx.function_declarations = private
  {
  is_classic_mode: bool ;
  set_of_closures_id: Set_of_closures_id.t ;
  set_of_closures_origin: Set_of_closures_origin.t ;
  funs: function_declaration Variable.Map.t }
and function_body = Compiler_without_sexp.Simple_value_approx.function_body =
  private
  {
  free_variables: Variable.Set.t ;
  free_symbols: Symbol.Set.t ;
  stub: bool ;
  dbg: Debuginfo.t ;
  inline: Lambda.inline_attribute ;
  specialise: Lambda.specialise_attribute ;
  is_a_functor: bool ;
  body: Flambda.t }
and function_declaration =
  Compiler_without_sexp.Simple_value_approx.function_declaration = private
  {
  closure_origin: Closure_origin.t ;
  params: Parameter.t list ;
  function_body: function_body option }
and value_set_of_closures =
  Compiler_without_sexp.Simple_value_approx.value_set_of_closures = private
  {
  function_decls: function_declarations ;
  bound_vars: t Var_within_closure.Map.t ;
  free_vars: Flambda.specialised_to Variable.Map.t ;
  invariant_params: Variable.Set.t Variable.Map.t Lazy.t ;
  recursive: Variable.Set.t Lazy.t ;
  size: int option Variable.Map.t Lazy.t ;
  specialised_args: Flambda.specialised_to Variable.Map.t ;
  freshening: Freshening.Project_var.t ;
  direct_call_surrogates: Closure_id.t Closure_id.Map.t }
and value_float_array_contents =
  Compiler_without_sexp.Simple_value_approx.value_float_array_contents =
  | Contents of t array 
  | Unknown_or_mutable 
and value_float_array =
  Compiler_without_sexp.Simple_value_approx.value_float_array =
  {
  contents: value_float_array_contents ;
  size: int }[@@deriving sexp_of]
type simplification_summary =
  Compiler_without_sexp.Simple_value_approx.simplification_summary =
  | Nothing_done 
  | Replaced_term [@@deriving sexp_of]
type simplification_result = (Flambda.t * simplification_summary * t)
[@@deriving sexp_of]
type simplification_result_named =
  (Flambda.named * simplification_summary * t)[@@deriving sexp_of]
type get_field_result =
  Compiler_without_sexp.Simple_value_approx.get_field_result =
  | Ok of t 
  | Unreachable [@@deriving sexp_of]
type checked_approx_for_block =
  Compiler_without_sexp.Simple_value_approx.checked_approx_for_block =
  | Wrong 
  | Ok of Tag.t * t array [@@deriving sexp_of]
type strict_checked_approx_for_set_of_closures =
  Compiler_without_sexp.Simple_value_approx.strict_checked_approx_for_set_of_closures
  =
  | Wrong 
  | Ok of Variable.t option * value_set_of_closures [@@deriving sexp_of]
type checked_approx_for_set_of_closures =
  Compiler_without_sexp.Simple_value_approx.checked_approx_for_set_of_closures
  =
  | Wrong 
  | Unresolved of unresolved_value 
  | Unknown 
  | Unknown_because_of_unresolved_value of unresolved_value 
  | Ok of Variable.t option * value_set_of_closures [@@deriving sexp_of]
type checked_approx_for_closure =
  Compiler_without_sexp.Simple_value_approx.checked_approx_for_closure =
  | Wrong 
  | Ok of value_closure * Variable.t option * Symbol.t option *
  value_set_of_closures [@@deriving sexp_of]
type checked_approx_for_closure_allowing_unresolved =
  Compiler_without_sexp.Simple_value_approx.checked_approx_for_closure_allowing_unresolved
  =
  | Wrong 
  | Unresolved of unresolved_value 
  | Unknown 
  | Unknown_because_of_unresolved_value of unresolved_value 
  | Ok of value_closure * Variable.t option * Symbol.t option *
  value_set_of_closures [@@deriving sexp_of]
type switch_branch_selection =
  Compiler_without_sexp.Simple_value_approx.switch_branch_selection =
  | Cannot_be_taken 
  | Can_be_taken 
  | Must_be_taken [@@deriving sexp_of]
include
  (Compiler_without_sexp.Simple_value_approx :
    module type of
      struct include Compiler_without_sexp.Simple_value_approx end with type
      'a boxed_int :=  'a Compiler_without_sexp.Simple_value_approx.boxed_int
      and type  checked_approx_for_block := 
      Compiler_without_sexp.Simple_value_approx.checked_approx_for_block and
      type  checked_approx_for_closure := 
      Compiler_without_sexp.Simple_value_approx.checked_approx_for_closure
      and type  checked_approx_for_closure_allowing_unresolved := 
      Compiler_without_sexp.Simple_value_approx.checked_approx_for_closure_allowing_unresolved
      and type  checked_approx_for_set_of_closures := 
      Compiler_without_sexp.Simple_value_approx.checked_approx_for_set_of_closures
      and type  descr :=  Compiler_without_sexp.Simple_value_approx.descr and
      type  function_body := 
      Compiler_without_sexp.Simple_value_approx.function_body and type
       function_declaration := 
      Compiler_without_sexp.Simple_value_approx.function_declaration and type
       function_declarations := 
      Compiler_without_sexp.Simple_value_approx.function_declarations and
      type  get_field_result := 
      Compiler_without_sexp.Simple_value_approx.get_field_result and type
       simplification_result := 
      Compiler_without_sexp.Simple_value_approx.simplification_result and
      type  simplification_result_named := 
      Compiler_without_sexp.Simple_value_approx.simplification_result_named
      and type  simplification_summary := 
      Compiler_without_sexp.Simple_value_approx.simplification_summary and
      type  strict_checked_approx_for_set_of_closures := 
      Compiler_without_sexp.Simple_value_approx.strict_checked_approx_for_set_of_closures
      and type  switch_branch_selection := 
      Compiler_without_sexp.Simple_value_approx.switch_branch_selection and
      type  t :=  Compiler_without_sexp.Simple_value_approx.t and type
       unknown_because_of := 
      Compiler_without_sexp.Simple_value_approx.unknown_because_of and type
       unresolved_value := 
      Compiler_without_sexp.Simple_value_approx.unresolved_value and type
       value_closure := 
      Compiler_without_sexp.Simple_value_approx.value_closure and type
       value_float_array := 
      Compiler_without_sexp.Simple_value_approx.value_float_array and type
       value_float_array_contents := 
      Compiler_without_sexp.Simple_value_approx.value_float_array_contents
      and type  value_set_of_closures := 
      Compiler_without_sexp.Simple_value_approx.value_set_of_closures and
      type  value_string := 
      Compiler_without_sexp.Simple_value_approx.value_string)
