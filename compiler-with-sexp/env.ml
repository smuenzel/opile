(* This file is generated from ../raw-compiler/env.mli using sexpify *)

open! Core
open! Types
open! Misc
type value_unbound_reason = Compiler_without_sexp.Env.value_unbound_reason =
  | Val_unbound_instance_variable 
  | Val_unbound_self 
  | Val_unbound_ancestor 
  | Val_unbound_ghost_recursive of Location.t [@@deriving sexp_of]
type module_unbound_reason = Compiler_without_sexp.Env.module_unbound_reason
  =
  | Mod_unbound_illegal_recursion [@@deriving sexp_of]
type summary = Compiler_without_sexp.Env.summary =
  | Env_empty 
  | Env_value of summary * Ident.t * value_description 
  | Env_type of summary * Ident.t * type_declaration 
  | Env_extension of summary * Ident.t * extension_constructor 
  | Env_module of summary * Ident.t * module_presence * module_declaration 
  | Env_modtype of summary * Ident.t * modtype_declaration 
  | Env_class of summary * Ident.t * class_declaration 
  | Env_cltype of summary * Ident.t * class_type_declaration 
  | Env_open of summary * Path.t 
  | Env_functor_arg of summary * Ident.t 
  | Env_constraints of summary * type_declaration Path.Map.t 
  | Env_copy_types of summary 
  | Env_persistent of summary * Ident.t 
  | Env_value_unbound of summary * string * value_unbound_reason 
  | Env_module_unbound of summary * string * module_unbound_reason [@@deriving
                                                                    sexp_of]
type address = Compiler_without_sexp.Env.address =
  | Aident of Ident.t 
  | Adot of address * int [@@deriving sexp_of]
type t = ((Compiler_without_sexp.Env.t)[@sexp.opaque ])[@@deriving sexp_of]
type type_descriptions =
  (constructor_description list * label_description list)[@@deriving sexp_of]
type iter_cont = ((Compiler_without_sexp.Env.iter_cont)[@sexp.opaque ])
[@@deriving sexp_of]
type constructor_usage = Compiler_without_sexp.Env.constructor_usage =
  | Positive 
  | Pattern 
  | Privatize [@@deriving sexp_of]
type unbound_value_hint = Compiler_without_sexp.Env.unbound_value_hint =
  | No_hint 
  | Missing_rec of Location.t [@@deriving sexp_of]
type lookup_error = Compiler_without_sexp.Env.lookup_error =
  | Unbound_value of Longident.t * unbound_value_hint 
  | Unbound_type of Longident.t 
  | Unbound_constructor of Longident.t 
  | Unbound_label of Longident.t 
  | Unbound_module of Longident.t 
  | Unbound_class of Longident.t 
  | Unbound_modtype of Longident.t 
  | Unbound_cltype of Longident.t 
  | Unbound_instance_variable of string 
  | Not_an_instance_variable of string 
  | Masked_instance_variable of Longident.t 
  | Masked_self_variable of Longident.t 
  | Masked_ancestor_variable of Longident.t 
  | Structure_used_as_functor of Longident.t 
  | Abstract_used_as_functor of Longident.t 
  | Functor_used_as_structure of Longident.t 
  | Abstract_used_as_structure of Longident.t 
  | Generative_used_as_applicative of Longident.t 
  | Illegal_reference_to_recursive_module 
  | Cannot_scrape_alias of Longident.t * Path.t [@@deriving sexp_of]
type error = Compiler_without_sexp.Env.error =
  | Missing_module of Location.t * Path.t * Path.t 
  | Illegal_value_name of Location.t * string 
  | Lookup_error of Location.t * t * lookup_error [@@deriving sexp_of]
open! Format
include
  (Compiler_without_sexp.Env :
    module type of struct include Compiler_without_sexp.Env end with type
       address :=  Compiler_without_sexp.Env.address and type
       constructor_usage :=  Compiler_without_sexp.Env.constructor_usage and
      type  error :=  Compiler_without_sexp.Env.error and type  iter_cont := 
      Compiler_without_sexp.Env.iter_cont and type  lookup_error := 
      Compiler_without_sexp.Env.lookup_error and type
       module_unbound_reason := 
      Compiler_without_sexp.Env.module_unbound_reason and type  summary := 
      Compiler_without_sexp.Env.summary and type  t := 
      Compiler_without_sexp.Env.t and type  type_descriptions := 
      Compiler_without_sexp.Env.type_descriptions and type
       unbound_value_hint :=  Compiler_without_sexp.Env.unbound_value_hint
      and type  value_unbound_reason := 
      Compiler_without_sexp.Env.value_unbound_reason)
