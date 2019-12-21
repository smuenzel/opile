open! Core

include Compiler_without_sexp.Targetint

let sexp_of_t t =
  to_string t
  |> [%sexp_of: string]
