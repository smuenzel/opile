open! Core

type t = Location.t =
  { loc_start : Lexing_with_sexp.position
  ; loc_end : Lexing_with_sexp.position
  ; loc_ghost : bool
  }
[@@deriving sexp_of]

type 'a loc = 'a Location.loc =
  { txt : 'a
  ; loc : t
  }
[@@deriving sexp_of]

include (
  Location :
    module type of struct
      include Location
    end
    with type t := Location.t
     and type 'a loc := 'a Location.loc)

