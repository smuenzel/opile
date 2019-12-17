open Core

type t = Longident.t =
  | Lident of string
  | Ldot of t * string
  | Lapply of t * t
[@@deriving sexp]

include (
  Longident :
    module type of struct
      include Longident
    end
    with type t := Longident.t)

