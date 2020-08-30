(* This file is generated from ../raw-compiler/includemod.mli using sexpify *)

open! Core
open! Typedtree
open! Types
open! Format
type mark = Compiler_without_sexp.Includemod.mark =
  | Mark_both 
  | Mark_positive 
  | Mark_negative 
  | Mark_neither [@@deriving sexp_of]
type symptom = Compiler_without_sexp.Includemod.symptom =
  | Missing_field of Ident.t * Location.t * string 
  | Value_descriptions of Ident.t * value_description * value_description 
  | Type_declarations of Ident.t * type_declaration * type_declaration *
  Includecore.type_mismatch 
  | Extension_constructors of Ident.t * extension_constructor *
  extension_constructor * Includecore.extension_constructor_mismatch 
  | Module_types of module_type * module_type 
  | Modtype_infos of Ident.t * modtype_declaration * modtype_declaration 
  | Modtype_permutation of Types.module_type * Typedtree.module_coercion 
  | Interface_mismatch of string * string 
  | Class_type_declarations of Ident.t * class_type_declaration *
  class_type_declaration * Ctype.class_match_failure list 
  | Class_declarations of Ident.t * class_declaration * class_declaration *
  Ctype.class_match_failure list 
  | Unbound_modtype_path of Path.t 
  | Unbound_module_path of Path.t 
  | Invalid_module_alias of Path.t [@@deriving sexp_of]
type pos = Compiler_without_sexp.Includemod.pos =
  | Module of Ident.t 
  | Modtype of Ident.t 
  | Arg of functor_parameter 
  | Body of functor_parameter [@@deriving sexp_of]
type error = (pos list * Env.t * symptom)[@@deriving sexp_of]
include
  (Compiler_without_sexp.Includemod :
    module type of struct include Compiler_without_sexp.Includemod end with
      type  error :=  Compiler_without_sexp.Includemod.error and type
       mark :=  Compiler_without_sexp.Includemod.mark and type  pos := 
      Compiler_without_sexp.Includemod.pos and type  symptom := 
      Compiler_without_sexp.Includemod.symptom)
