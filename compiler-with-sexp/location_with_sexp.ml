open Core

type t = Location.t = {
  loc_start : Lexing_with_sexp.position;
  loc_end : Lexing_with_sexp.position;
  loc_ghost : bool;
}
[@@deriving sexp]

type 'a loc = 'a Location.loc = { txt : 'a; loc : t } [@@deriving sexp]

include (
  Location : module type of Location with type t := t and type 'a loc := 'a loc )
