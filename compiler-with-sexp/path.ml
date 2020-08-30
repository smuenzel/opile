(* This file is generated from ../raw-compiler/path.mli using sexpify *)

open! Core
type t = Compiler_without_sexp.Path.t =
  | Pident of Ident.t 
  | Pdot of t * string 
  | Papply of t * t [@@deriving sexp_of]
type typath = Compiler_without_sexp.Path.typath =
  | Regular of t 
  | Ext of t * string 
  | LocalExt of Ident.t 
  | Cstr of t * string [@@deriving sexp_of]
module Map =
  struct
    include Compiler_without_sexp.Path.Map
    let sexp_of_t sexp_of_a t =
      (fold (fun key -> fun data -> fun acc -> (key, data) :: acc) t []) |>
        ([%sexp_of :(t * a) list])
  end
module Set =
  struct
    include Compiler_without_sexp.Path.Set
    let sexp_of_t t =
      (fold (fun key -> fun acc -> key :: acc) t []) |> ([%sexp_of :t list])
  end
include
  (Compiler_without_sexp.Path :
    module type of struct include Compiler_without_sexp.Path end with type
       t :=  Compiler_without_sexp.Path.t and type  typath := 
      Compiler_without_sexp.Path.typath and module Map :=
      Compiler_without_sexp.Path.Map and module Set :=
      Compiler_without_sexp.Path.Set)
