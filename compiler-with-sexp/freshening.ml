(* This file is generated from ../raw-compiler/freshening.mli using sexpify *)

open! Core
type t = ((Compiler_without_sexp.Freshening.t)[@sexp.opaque ])[@@deriving
                                                                sexp_of]
type subst = t[@@deriving sexp_of]
module Project_var =
  struct
    type t =
      ((Compiler_without_sexp.Freshening.Project_var.t)[@sexp.opaque ])
    [@@deriving sexp_of]
    include
      (Compiler_without_sexp.Freshening.Project_var :
        module type of
          struct include Compiler_without_sexp.Freshening.Project_var end
          with type  t :=  Compiler_without_sexp.Freshening.Project_var.t)
  end
include
  (Compiler_without_sexp.Freshening :
    module type of struct include Compiler_without_sexp.Freshening end with
      type  subst :=  Compiler_without_sexp.Freshening.subst and type  t := 
      Compiler_without_sexp.Freshening.t and module Project_var :=
      Compiler_without_sexp.Freshening.Project_var)
