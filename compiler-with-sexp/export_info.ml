(* This file is generated from ../raw-compiler/export_info.mli using sexpify *)

open! Core
module A = Simple_value_approx

type value_string_contents = Compiler_without_sexp.Export_info.value_string_contents =
  | Contents of string
  | Unknown_or_mutable
[@@deriving sexp_of]

type value_string = Compiler_without_sexp.Export_info.value_string =
  { contents : value_string_contents
  ; size : int
  }
[@@deriving sexp_of]

type value_float_array_contents =
      Compiler_without_sexp.Export_info.value_float_array_contents =
  | Contents of float option array
  | Unknown_or_mutable
[@@deriving sexp_of]

type value_float_array = Compiler_without_sexp.Export_info.value_float_array =
  { contents : value_float_array_contents
  ; size : int
  }
[@@deriving sexp_of]

type descr = Compiler_without_sexp.Export_info.descr =
  | Value_block of Tag.t * approx array
  | Value_mutable_block of Tag.t * int
  | Value_int of int
  | Value_char of char
  | Value_constptr of int
  | Value_float of float
  | Value_float_array of value_float_array
  | Value_boxed_int : 'a A.boxed_int * 'a -> descr
  | Value_string of value_string
  | Value_closure of value_closure
  | Value_set_of_closures of value_set_of_closures
  | Value_unknown_descr

and value_closure = Compiler_without_sexp.Export_info.value_closure =
  { closure_id : Closure_id.t
  ; set_of_closures : value_set_of_closures
  }

and value_set_of_closures = Compiler_without_sexp.Export_info.value_set_of_closures =
  { set_of_closures_id : Set_of_closures_id.t
  ; bound_vars : approx Var_within_closure.Map.t
  ; free_vars : Flambda.specialised_to Variable.Map.t
  ; results : approx Closure_id.Map.t
  ; aliased_symbol : Symbol.t option
  }

and approx = Compiler_without_sexp.Export_info.approx =
  | Value_unknown
  | Value_id of Export_id.t
  | Value_symbol of Symbol.t
[@@deriving sexp_of]

type t = Compiler_without_sexp.Export_info.t = private
  { sets_of_closures : A.function_declarations Set_of_closures_id.Map.t
  ; values : descr Export_id.Map.t Compilation_unit.Map.t
  ; symbol_id : Export_id.t Symbol.Map.t
  ; offset_fun : int Closure_id.Map.t
  ; offset_fv : int Var_within_closure.Map.t
  ; constant_closures : Closure_id.Set.t
  ; invariant_params : Variable.Set.t Variable.Map.t Set_of_closures_id.Map.t
  ; recursive : Variable.Set.t Set_of_closures_id.Map.t
  }
[@@deriving sexp_of]

type transient = Compiler_without_sexp.Export_info.transient = private
  { sets_of_closures : A.function_declarations Set_of_closures_id.Map.t
  ; values : descr Export_id.Map.t Compilation_unit.Map.t
  ; symbol_id : Export_id.t Symbol.Map.t
  ; invariant_params : Variable.Set.t Variable.Map.t Set_of_closures_id.Map.t
  ; recursive : Variable.Set.t Set_of_closures_id.Map.t
  ; relevant_local_closure_ids : Closure_id.Set.t
  ; relevant_imported_closure_ids : Closure_id.Set.t
  ; relevant_local_vars_within_closure : Var_within_closure.Set.t
  ; relevant_imported_vars_within_closure : Var_within_closure.Set.t
  }
[@@deriving sexp_of]

include (
  Compiler_without_sexp.Export_info :
    module type of struct
      include Compiler_without_sexp.Export_info
    end
    with type approx := Compiler_without_sexp.Export_info.approx
     and type descr := Compiler_without_sexp.Export_info.descr
     and type t := Compiler_without_sexp.Export_info.t
     and type transient := Compiler_without_sexp.Export_info.transient
     and type value_closure := Compiler_without_sexp.Export_info.value_closure
     and type value_float_array := Compiler_without_sexp.Export_info.value_float_array
     and type value_float_array_contents :=
          Compiler_without_sexp.Export_info.value_float_array_contents
     and type value_set_of_closures :=
          Compiler_without_sexp.Export_info.value_set_of_closures
     and type value_string := Compiler_without_sexp.Export_info.value_string
     and type value_string_contents :=
          Compiler_without_sexp.Export_info.value_string_contents
     and module A := Compiler_without_sexp.Export_info.A)

