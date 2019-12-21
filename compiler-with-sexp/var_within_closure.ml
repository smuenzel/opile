open! Core

include Compiler_without_sexp.Var_within_closure

let sexp_of_t t =
  unique_name t
  |> [%sexp_of: string]

module Set = struct
  include Compiler_without_sexp.Var_within_closure.Set

  let sexp_of_t t = fold (fun key acc -> key :: acc) t [] |> [%sexp_of: t list]
end

module Map = struct
  include Compiler_without_sexp.Var_within_closure.Map

  let sexp_of_t sexp_of_a t =
    fold (fun key data acc -> (key, data) :: acc) t [] |> [%sexp_of: (t * a) list]
  ;;
end
