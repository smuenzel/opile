(* This file is generated from ../raw-compiler/projection.mli using sexpify *)

open! Core

type project_closure = Compiler_without_sexp.Projection.project_closure =
  { set_of_closures : Variable.t
  ; closure_id : Closure_id.t
  }
[@@deriving sexp_of]

type move_within_set_of_closures =
      Compiler_without_sexp.Projection.move_within_set_of_closures =
  { closure : Variable.t
  ; start_from : Closure_id.t
  ; move_to : Closure_id.t
  }
[@@deriving sexp_of]

type project_var = Compiler_without_sexp.Projection.project_var =
  { closure : Variable.t
  ; closure_id : Closure_id.t
  ; var : Var_within_closure.t
  }
[@@deriving sexp_of]

type t = Compiler_without_sexp.Projection.t =
  | Project_var of project_var
  | Project_closure of project_closure
  | Move_within_set_of_closures of move_within_set_of_closures
  | Field of int * Variable.t
[@@deriving sexp_of]

include (
  Compiler_without_sexp.Projection :
    module type of struct
      include Compiler_without_sexp.Projection
    end
    with type move_within_set_of_closures :=
          Compiler_without_sexp.Projection.move_within_set_of_closures
     and type project_closure := Compiler_without_sexp.Projection.project_closure
     and type project_var := Compiler_without_sexp.Projection.project_var
     and type t := Compiler_without_sexp.Projection.t)

