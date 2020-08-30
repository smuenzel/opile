(* This file is generated from ../raw-compiler/typetexp.mli using sexpify *)

open! Core
open! Types
type variable_context =
  ((Compiler_without_sexp.Typetexp.variable_context)[@sexp.opaque ])[@@deriving
                                                                    sexp_of]
type error = Compiler_without_sexp.Typetexp.error =
  | Unbound_type_variable of string 
  | Undefined_type_constructor of Path.t 
  | Type_arity_mismatch of Longident.t * int * int 
  | Bound_type_variable of string 
  | Recursive_type 
  | Unbound_row_variable of Longident.t 
  | Type_mismatch of Ctype.Unification_trace.t 
  | Alias_type_mismatch of Ctype.Unification_trace.t 
  | Present_has_conjunction of string 
  | Present_has_no_type of string 
  | Constructor_mismatch of type_expr * type_expr 
  | Not_a_variant of type_expr 
  | Variant_tags of string * string 
  | Invalid_variable_name of string 
  | Cannot_quantify of string * type_expr 
  | Multiple_constraints_on_type of Longident.t 
  | Method_mismatch of string * type_expr * type_expr 
  | Opened_object of Path.t option 
  | Not_an_object of type_expr [@@deriving sexp_of]
include
  (Compiler_without_sexp.Typetexp :
    module type of struct include Compiler_without_sexp.Typetexp end with
      type  error :=  Compiler_without_sexp.Typetexp.error and type
       variable_context :=  Compiler_without_sexp.Typetexp.variable_context)
