(* This file is generated from ../raw-compiler/asttypes.mli using sexpify *)

open! Core

type constant = Compiler_without_sexp.Asttypes.constant =
  | Const_int of int
  | Const_char of char
  | Const_string of string * string option
  | Const_float of string
  | Const_int32 of int32
  | Const_int64 of int64
  | Const_nativeint of nativeint
[@@deriving sexp_of]

type rec_flag = Compiler_without_sexp.Asttypes.rec_flag =
  | Nonrecursive
  | Recursive
[@@deriving sexp_of]

type direction_flag = Compiler_without_sexp.Asttypes.direction_flag =
  | Upto
  | Downto
[@@deriving sexp_of]

type private_flag = Compiler_without_sexp.Asttypes.private_flag =
  | Private
  | Public
[@@deriving sexp_of]

type mutable_flag = Compiler_without_sexp.Asttypes.mutable_flag =
  | Immutable
  | Mutable
[@@deriving sexp_of]

type virtual_flag = Compiler_without_sexp.Asttypes.virtual_flag =
  | Virtual
  | Concrete
[@@deriving sexp_of]

type override_flag = Compiler_without_sexp.Asttypes.override_flag =
  | Override
  | Fresh
[@@deriving sexp_of]

type closed_flag = Compiler_without_sexp.Asttypes.closed_flag =
  | Closed
  | Open
[@@deriving sexp_of]

type label = string [@@deriving sexp_of]

type arg_label = Compiler_without_sexp.Asttypes.arg_label =
  | Nolabel
  | Labelled of string
  | Optional of string
[@@deriving sexp_of]

type 'a loc = 'a Compiler_without_sexp.Asttypes.loc =
  { txt : 'a
  ; loc : Location.t
  }
[@@deriving sexp_of]

type variance = Compiler_without_sexp.Asttypes.variance =
  | Covariant
  | Contravariant
  | Invariant
[@@deriving sexp_of]

