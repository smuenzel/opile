open! Core

type t = Compiler_without_sexp.Longident.t =
  | Lident of string
  | Ldot of t * string
  | Lapply of t * t
[@@deriving sexp_of]

include (
  Compiler_without_sexp.Longident :
    module type of struct
      include Compiler_without_sexp.Longident
    end
    with type t := Compiler_without_sexp.Longident.t)

