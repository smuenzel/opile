open! Core

type t = Compiler_without_sexp.Ident.t

let sexp_of_t t =
  Compiler_without_sexp.Ident.unique_name t
  |> [%sexp_of: string]
