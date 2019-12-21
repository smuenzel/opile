open! Core

type t = Compiler_without_sexp.Ident.t

let sexp_of_t t =
  Compiler_without_sexp.Ident.unique_name t
  |> [%sexp_of: string]

module Set = struct
  include Compiler_without_sexp.Ident.Set

  let sexp_of_t t = fold (fun key acc -> key :: acc) t [] |> [%sexp_of: t list]
end
