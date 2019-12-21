open! Core

include Caml.Digest

let sexp_of_t t =
  Caml.Digest.to_hex t
  |> [%sexp_of: string]
