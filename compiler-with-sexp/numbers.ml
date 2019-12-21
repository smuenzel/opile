open! Core

module Int = struct
  module Set = struct
    include (Compiler_without_sexp.Numbers.Int.Set
             : module type of struct include Compiler_without_sexp.Numbers.Int.Set end
            )

    let sexp_of_t t = fold (fun key acc -> key :: acc) t [] |> [%sexp_of: int list]
  end

  include (Compiler_without_sexp.Numbers.Int
           : module type of struct include Compiler_without_sexp.Numbers.Int end
           with module Set := Compiler_without_sexp.Numbers.Int.Set
          )

end

include (Compiler_without_sexp.Numbers
         : module type of struct include Compiler_without_sexp.Numbers end
         with module Int := Compiler_without_sexp.Numbers.Int
        )
