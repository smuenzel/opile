open! Core

include Compiler_without_sexp.Set_of_closures_id

let sexp_of_t t =
  print Format.str_formatter t;
  Format.flush_str_formatter ()
  |> [%sexp_of: string]

module Map = struct
  include Compiler_without_sexp.Set_of_closures_id.Map

  let sexp_of_t sexp_of_a t =
    fold (fun key data acc -> (key, data) :: acc) t [] |> [%sexp_of: (t * a) list]
  ;;
end
