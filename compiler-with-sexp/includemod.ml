open! Core
open! Typedtree
open! Types

type mark = Compiler_without_sexp.Includemod.mark =
  | Mark_both [@ocaml.doc " Mark definitions used from both arguments "]
  | Mark_positive
      [@ocaml.doc " Mark definitions used from the positive (first) argument "]
  | Mark_negative
      [@ocaml.doc " Mark definitions used from the negative (second) argument "]
  | Mark_neither [@ocaml.doc " Do not mark definitions used from either argument "]
[@@deriving sexp_of]

type symptom = Compiler_without_sexp.Includemod.symptom =
  | Missing_field of Ident.t * Location.t * string
  | Value_descriptions of Ident.t * value_description * value_description
  | Type_declarations of
      Ident.t * type_declaration * type_declaration * Includecore.type_mismatch
  | Extension_constructors of
      Ident.t * extension_constructor * extension_constructor * Includecore.type_mismatch
  | Module_types of module_type * module_type
  | Modtype_infos of Ident.t * modtype_declaration * modtype_declaration
  | Modtype_permutation
  | Interface_mismatch of string * string
  | Class_type_declarations of
      Ident.t
      * class_type_declaration
      * class_type_declaration
      * Ctype.class_match_failure list
  | Class_declarations of
      Ident.t * class_declaration * class_declaration * Ctype.class_match_failure list
  | Unbound_modtype_path of Path.t
  | Unbound_module_path of Path.t
  | Invalid_module_alias of Path.t
[@@deriving sexp_of]

type pos = Compiler_without_sexp.Includemod.pos =
  | Module of Ident.t
  | Modtype of Ident.t
  | Arg of Ident.t
  | Body of Ident.t
[@@deriving sexp_of]

type error = pos list * Env.t * symptom [@@deriving sexp_of]

include (
  Compiler_without_sexp.Includemod :
    module type of struct
      include Compiler_without_sexp.Includemod
    end
    with type error := Compiler_without_sexp.Includemod.error
     and type mark := Compiler_without_sexp.Includemod.mark
     and type pos := Compiler_without_sexp.Includemod.pos
     and type symptom := Compiler_without_sexp.Includemod.symptom)

