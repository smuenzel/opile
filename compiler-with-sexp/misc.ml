open! Core

module Stdlib = struct
  module String = struct
    module Map = struct
      include (Compiler_without_sexp.Misc.Stdlib.String.Map
               : module type of struct include Compiler_without_sexp.Misc.Stdlib.String.Map end
              )

      let sexp_of_t sexp_of_a t =
        fold (fun key data acc -> (key, data) :: acc) t [] |> [%sexp_of: (string * a) list]
      ;;
    end
    include (Compiler_without_sexp.Misc.Stdlib.String
             : module type of struct include Compiler_without_sexp.Misc.Stdlib.String end
             with module Map := Compiler_without_sexp.Misc.Stdlib.String.Map
            )
  end

  include (Compiler_without_sexp.Misc.Stdlib
           : module type of struct include Compiler_without_sexp.Misc.Stdlib end
           with module String := Compiler_without_sexp.Misc.Stdlib.String
          )
end

include (Compiler_without_sexp.Misc
         : module type of struct include Compiler_without_sexp.Misc end
         with module Stdlib := Compiler_without_sexp.Misc.Stdlib
        )
