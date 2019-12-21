open! Core

include Compiler_without_sexp.Var_within_closure

let sexp_of_t t =
  unique_name t
  |> [%sexp_of: string]
