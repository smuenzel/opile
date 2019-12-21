open! Core

include Compiler_without_sexp.Tag

let sexp_of_t t =
  print Format.str_formatter t;
  Format.flush_str_formatter ()
  |> [%sexp_of: string]
