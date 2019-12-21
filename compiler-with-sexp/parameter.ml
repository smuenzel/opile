open! Core

include Compiler_without_sexp.Parameter

let sexp_of_t t =
  print Format.str_formatter t;
  Format.flush_str_formatter ()
  |> [%sexp_of: string]

module Set = struct
  include Compiler_without_sexp.Parameter.Set

  let sexp_of_t t = fold (fun key acc -> key :: acc) t [] |> [%sexp_of: t list]
end

module Map = struct
  include Compiler_without_sexp.Parameter.Map

  let sexp_of_t sexp_of_a t =
    fold (fun key data acc -> (key, data) :: acc) t [] |> [%sexp_of: (t * a) list]
  ;;
end
