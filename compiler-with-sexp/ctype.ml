open! Core
open! Asttypes
open! Types

module Unification_trace = struct
  type position = Compiler_without_sexp.Ctype.Unification_trace.position =
    | First
    | Second
  [@@deriving sexp_of]

  type desc = Compiler_without_sexp.Ctype.Unification_trace.desc =
    { t : type_expr
    ; expanded : type_expr option
    }
  [@@deriving sexp_of]

  type 'a diff = 'a Compiler_without_sexp.Ctype.Unification_trace.diff =
    { got : 'a
    ; expected : 'a
    }
  [@@deriving sexp_of]

  type 'a escape = 'a Compiler_without_sexp.Ctype.Unification_trace.escape =
    | Constructor of Path.t
    | Univ of type_expr
    | Self
    | Module_type of Path.t
    | Equation of 'a
  [@@deriving sexp_of]

  type variant = Compiler_without_sexp.Ctype.Unification_trace.variant =
    | No_intersection
    | No_tags of position * (Asttypes.label * row_field) list
    | Incompatible_types_for of string
  [@@deriving sexp_of]

  type obj = Compiler_without_sexp.Ctype.Unification_trace.obj =
    | Missing_field of position * string
    | Abstract_row of position
    | Self_cannot_be_closed
  [@@deriving sexp_of]

  type 'a elt = 'a Compiler_without_sexp.Ctype.Unification_trace.elt =
    | Diff of 'a diff
    | Variant of variant
    | Obj of obj
    | Escape of
        { context : type_expr option
        ; kind : 'a escape
        }
    | Incompatible_fields of
        { name : string
        ; diff : type_expr diff
        }
    | Rec_occur of type_expr * type_expr
  [@@deriving sexp_of]

  type t = desc elt list [@@deriving sexp_of]

  include (
    Compiler_without_sexp.Ctype.Unification_trace :
      module type of struct
        include Compiler_without_sexp.Ctype.Unification_trace
      end
      with type desc := Compiler_without_sexp.Ctype.Unification_trace.desc
       and type 'a diff := 'a Compiler_without_sexp.Ctype.Unification_trace.diff
       and type 'a elt := 'a Compiler_without_sexp.Ctype.Unification_trace.elt
       and type 'a escape := 'a Compiler_without_sexp.Ctype.Unification_trace.escape
       and type obj := Compiler_without_sexp.Ctype.Unification_trace.obj
       and type position := Compiler_without_sexp.Ctype.Unification_trace.position
       and type t := Compiler_without_sexp.Ctype.Unification_trace.t
       and type variant := Compiler_without_sexp.Ctype.Unification_trace.variant)
end

type levels = Compiler_without_sexp.Ctype.levels =
  { current_level : int
  ; nongen_level : int
  ; global_level : int
  ; saved_level : (int * int) list
  }
[@@deriving sexp_of]

type class_match_failure = Compiler_without_sexp.Ctype.class_match_failure =
  | CM_Virtual_class
  | CM_Parameter_arity_mismatch of int * int
  | CM_Type_parameter_mismatch of Env.t * Unification_trace.t
  | CM_Class_type_mismatch of Env.t * class_type * class_type
  | CM_Parameter_mismatch of Env.t * Unification_trace.t
  | CM_Val_type_mismatch of string * Env.t * Unification_trace.t
  | CM_Meth_type_mismatch of string * Env.t * Unification_trace.t
  | CM_Non_mutable_value of string
  | CM_Non_concrete_value of string
  | CM_Missing_value of string
  | CM_Missing_method of string
  | CM_Hide_public of string
  | CM_Hide_virtual of string * string
  | CM_Public_method of string
  | CM_Private_method of string
  | CM_Virtual_method of string
[@@deriving sexp_of]

type closed_class_failure = Compiler_without_sexp.Ctype.closed_class_failure =
  | CC_Method of type_expr * bool * string * type_expr
  | CC_Value of type_expr * bool * string * type_expr
[@@deriving sexp_of]

include (
  Compiler_without_sexp.Ctype :
    module type of struct
      include Compiler_without_sexp.Ctype
    end
    with type class_match_failure := Compiler_without_sexp.Ctype.class_match_failure
     and type closed_class_failure := Compiler_without_sexp.Ctype.closed_class_failure
     and type levels := Compiler_without_sexp.Ctype.levels
     and module Unification_trace := Compiler_without_sexp.Ctype.Unification_trace)

