open! Core

include Compiler_without_sexp.Mutable_variable

let sexp_of_t t =
  unique_name t
  |> [%sexp_of: string]
