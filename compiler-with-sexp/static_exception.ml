open! Core

include Compiler_without_sexp.Static_exception

let sexp_of_t t =
  to_int t
  |> [%sexp_of: int]
