open! Core

type constant = Asttypes.constant =
  | Const_int of int
  | Const_char of char
  | Const_string of string * string option
  | Const_float of string
  | Const_int32 of int32
  | Const_int64 of int64
  | Const_nativeint of nativeint
[@@deriving sexp_of]

type rec_flag = Asttypes.rec_flag =
  | Nonrecursive
  | Recursive
[@@deriving sexp_of]

type direction_flag = Asttypes.direction_flag =
  | Upto
  | Downto
[@@deriving sexp_of]

type private_flag = Asttypes.private_flag =
  | Private
  | Public
[@@deriving sexp_of]

type mutable_flag = Asttypes.mutable_flag =
  | Immutable
  | Mutable
[@@deriving sexp_of]

type virtual_flag = Asttypes.virtual_flag =
  | Virtual
  | Concrete
[@@deriving sexp_of]

type override_flag = Asttypes.override_flag =
  | Override
  | Fresh
[@@deriving sexp_of]

type closed_flag = Asttypes.closed_flag =
  | Closed
  | Open
[@@deriving sexp_of]

type label = string [@@deriving sexp_of]

type arg_label = Asttypes.arg_label =
  | Nolabel
  | Labelled of string
  | Optional of string
[@@deriving sexp_of]

type 'a loc = 'a Asttypes.loc =
  { txt : 'a
  ; loc : Location_with_sexp.t
  }
[@@deriving sexp_of]

type variance = Asttypes.variance =
  | Covariant
  | Contravariant
  | Invariant
[@@deriving sexp_of]

