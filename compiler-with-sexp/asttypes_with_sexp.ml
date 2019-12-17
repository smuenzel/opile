open Core

type constant = Asttypes.constant =
  | Const_int of int
  | Const_char of char
  | Const_string of string * string option
  | Const_float of string
  | Const_int32 of int32
  | Const_int64 of int64
  | Const_nativeint of nativeint
[@@deriving sexp]

type rec_flag = Asttypes.rec_flag =
  | Nonrecursive
  | Recursive
[@@deriving sexp]

type direction_flag = Asttypes.direction_flag =
  | Upto
  | Downto
[@@deriving sexp]

type private_flag = Asttypes.private_flag =
  | Private
  | Public
[@@deriving sexp]

type mutable_flag = Asttypes.mutable_flag =
  | Immutable
  | Mutable
[@@deriving sexp]

type virtual_flag = Asttypes.virtual_flag =
  | Virtual
  | Concrete
[@@deriving sexp]

type override_flag = Asttypes.override_flag =
  | Override
  | Fresh
[@@deriving sexp]

type closed_flag = Asttypes.closed_flag =
  | Closed
  | Open
[@@deriving sexp]

type label = string [@@deriving sexp]

type arg_label = Asttypes.arg_label =
  | Nolabel
  | Labelled of string
  | Optional of string
[@@deriving sexp]

type 'a loc = 'a Asttypes.loc =
  { txt : 'a
  ; loc : Location_with_sexp.t
  }
[@@deriving sexp]

type variance = Asttypes.variance =
  | Covariant
  | Contravariant
  | Invariant
[@@deriving sexp]

