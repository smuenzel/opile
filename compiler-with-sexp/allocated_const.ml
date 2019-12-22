(* This file is generated from ../raw-compiler/allocated_const.mli using sexpify *)

open! Core

type t = Compiler_without_sexp.Allocated_const.t =
  | Float of float
  | Int32 of int32
  | Int64 of int64
  | Nativeint of nativeint
  | Float_array of float list
  | Immutable_float_array of float list
  | String of string
  | Immutable_string of string
[@@deriving sexp_of]

include (
  Compiler_without_sexp.Allocated_const :
    module type of struct
      include Compiler_without_sexp.Allocated_const
    end
    with type t := Compiler_without_sexp.Allocated_const.t)

