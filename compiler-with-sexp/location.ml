(* This file is generated from ../raw-compiler/location.mli using sexpify *)

open! Core
type t = Compiler_without_sexp.Location.t =
  {
  loc_start: Lexing.position ;
  loc_end: Lexing.position ;
  loc_ghost: bool }[@@deriving sexp_of]
type 'a loc = 'a Compiler_without_sexp.Location.loc = {
  txt: 'a ;
  loc: t }[@@deriving sexp_of]
include
  (Compiler_without_sexp.Location :
    module type of struct include Compiler_without_sexp.Location end with
      type 'a loc :=  'a Compiler_without_sexp.Location.loc and type  t := 
      Compiler_without_sexp.Location.t)
