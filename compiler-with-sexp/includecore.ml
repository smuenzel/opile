(* This file is generated from ../raw-compiler/includecore.mli using sexpify *)

open! Core
open! Typedtree
open! Types
type position = Compiler_without_sexp.Includecore.position =
  | First 
  | Second [@@deriving sexp_of]
type label_mismatch = Compiler_without_sexp.Includecore.label_mismatch =
  | Type 
  | Mutability of position [@@deriving sexp_of]
type record_mismatch = Compiler_without_sexp.Includecore.record_mismatch =
  | Label_mismatch of label_declaration * label_declaration * label_mismatch
  
  | Label_names of int * Ident.t * Ident.t 
  | Label_missing of position * Ident.t 
  | Unboxed_float_representation of position [@@deriving sexp_of]
type constructor_mismatch =
  Compiler_without_sexp.Includecore.constructor_mismatch =
  | Type 
  | Arity 
  | Inline_record of record_mismatch 
  | Kind of position 
  | Explicit_return_type of position [@@deriving sexp_of]
type variant_mismatch = Compiler_without_sexp.Includecore.variant_mismatch =
  | Constructor_mismatch of constructor_declaration * constructor_declaration
  * constructor_mismatch 
  | Constructor_names of int * Ident.t * Ident.t 
  | Constructor_missing of position * Ident.t [@@deriving sexp_of]
type extension_constructor_mismatch =
  Compiler_without_sexp.Includecore.extension_constructor_mismatch =
  | Constructor_privacy 
  | Constructor_mismatch of Ident.t * extension_constructor *
  extension_constructor * constructor_mismatch [@@deriving sexp_of]
type type_mismatch = Compiler_without_sexp.Includecore.type_mismatch =
  | Arity 
  | Privacy 
  | Kind 
  | Constraint 
  | Manifest 
  | Variance 
  | Record_mismatch of record_mismatch 
  | Variant_mismatch of variant_mismatch 
  | Unboxed_representation of position 
  | Immediate of Caml_type_immediacy.Violation.t [@@deriving sexp_of]
include
  (Compiler_without_sexp.Includecore :
    module type of struct include Compiler_without_sexp.Includecore end with
      type  constructor_mismatch := 
      Compiler_without_sexp.Includecore.constructor_mismatch and type
       extension_constructor_mismatch := 
      Compiler_without_sexp.Includecore.extension_constructor_mismatch and
      type  label_mismatch := 
      Compiler_without_sexp.Includecore.label_mismatch and type  position := 
      Compiler_without_sexp.Includecore.position and type  record_mismatch :=
       Compiler_without_sexp.Includecore.record_mismatch and type
       type_mismatch :=  Compiler_without_sexp.Includecore.type_mismatch and
      type  variant_mismatch := 
      Compiler_without_sexp.Includecore.variant_mismatch)
