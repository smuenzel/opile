(* This file is generated from ../raw-compiler/includecore.mli using sexpify *)

open! Core
open! Typedtree
open! Types

type type_mismatch = Compiler_without_sexp.Includecore.type_mismatch =
  | Arity
  | Privacy
  | Kind
  | Constraint
  | Manifest
  | Variance
  | Field_type of Ident.t
  | Field_mutable of Ident.t
  | Field_arity of Ident.t
  | Field_names of int * Ident.t * Ident.t
  | Field_missing of bool * Ident.t
  | Record_representation of bool
  | Unboxed_representation of bool
  | Immediate
[@@deriving sexp_of]

include (
  Compiler_without_sexp.Includecore :
    module type of struct
      include Compiler_without_sexp.Includecore
    end
    with type type_mismatch := Compiler_without_sexp.Includecore.type_mismatch)

