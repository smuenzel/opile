open! Core

include Compiler_without_sexp.Backend_var

let sexp_of_t t =
  print Format.str_formatter t;
  Format.flush_str_formatter ()
  |> [%sexp_of: string]

module With_provenance = struct

  include Compiler_without_sexp.Backend_var.With_provenance

  let sexp_of_t t =
    print Format.str_formatter t;
    Format.flush_str_formatter ()
    |> [%sexp_of: string]

end
