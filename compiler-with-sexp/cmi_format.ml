(* This file is generated from ../raw-compiler/cmi_format.mli using sexpify *)

open! Core

type pers_flags = Compiler_without_sexp.Cmi_format.pers_flags =
  | Rectypes
  | Alerts of string Misc.Stdlib.String.Map.t
  | Opaque
  | Unsafe_string
[@@deriving sexp_of]

type cmi_infos = Compiler_without_sexp.Cmi_format.cmi_infos =
  { cmi_name : string
  ; cmi_sign : Types.signature_item list
  ; cmi_crcs : (string * Caml_digest.t option) list
  ; cmi_flags : pers_flags list
  }
[@@deriving sexp_of]

type error = Compiler_without_sexp.Cmi_format.error =
  | Not_an_interface of string
  | Wrong_version_interface of string * string
  | Corrupted_interface of string
[@@deriving sexp_of]

open! Format

include (
  Compiler_without_sexp.Cmi_format :
    module type of struct
      include Compiler_without_sexp.Cmi_format
    end
    with type cmi_infos := Compiler_without_sexp.Cmi_format.cmi_infos
     and type error := Compiler_without_sexp.Cmi_format.error
     and type pers_flags := Compiler_without_sexp.Cmi_format.pers_flags)

