(* This file is generated from ../raw-compiler/type_immediacy.mli using sexpify *)

open! Core
type t = Compiler_without_sexp.Type_immediacy.t =
  | Unknown 
  | Always 
  | Always_on_64bits [@@deriving sexp_of]
module Violation =
  struct
    type t = Compiler_without_sexp.Type_immediacy.Violation.t =
      | Not_always_immediate 
      | Not_always_immediate_on_64bits [@@deriving sexp_of]
  end
include
  (Compiler_without_sexp.Type_immediacy :
    module type of struct include Compiler_without_sexp.Type_immediacy end
      with type  t :=  Compiler_without_sexp.Type_immediacy.t and
      module Violation := Compiler_without_sexp.Type_immediacy.Violation)
