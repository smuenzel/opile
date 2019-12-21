open! Core
open! Types

type summary = Compiler_without_sexp.Env.summary =
  | Env_empty
  | Env_value of summary * Ident.t * value_description
  | Env_type of summary * Ident.t * type_declaration
  | Env_extension of summary * Ident.t * extension_constructor
  | Env_module of summary * Ident.t * module_presence * module_declaration
  | Env_modtype of summary * Ident.t * modtype_declaration
  | Env_class of summary * Ident.t * class_declaration
  | Env_cltype of summary * Ident.t * class_type_declaration
  | Env_open of summary * Path.t
      [@ocaml.doc
        " The string set argument of [Env_open] represents a list of module names\n\
        \      to skip, i.e. that won't be imported in the toplevel namespace. "]
  | Env_functor_arg of summary * Ident.t
  | Env_constraints of summary * type_declaration Path.Map.t
  | Env_copy_types of summary * string list
  | Env_persistent of summary * Ident.t
[@@deriving sexp_of]

type address = Compiler_without_sexp.Env.address =
  | Aident of Ident.t
  | Adot of address * int
[@@deriving sexp_of]

type t = (Compiler_without_sexp.Env.t[@sexp.opaque]) [@@deriving sexp_of]

type type_descriptions = constructor_description list * label_description list
[@@deriving sexp_of]

type iter_cont = (Compiler_without_sexp.Env.iter_cont[@sexp.opaque]) [@@deriving sexp_of]

type copy_of_types = (Compiler_without_sexp.Env.copy_of_types[@sexp.opaque])
[@@deriving sexp_of]

type error = Compiler_without_sexp.Env.error =
  | Illegal_renaming of string * string * string
  | Inconsistent_import of string * string * string
  | Need_recursive_types of string * string
  | Depend_on_unsafe_string_unit of string * string
  | Missing_module of Location.t * Path.t * Path.t
  | Illegal_value_name of Location.t * string
[@@deriving sexp_of]

open! Format

type constructor_usage = Compiler_without_sexp.Env.constructor_usage =
  | Positive
  | Pattern
  | Privatize
[@@deriving sexp_of]

module Persistent_signature = struct
  type t = Compiler_without_sexp.Env.Persistent_signature.t =
    { filename : string [@ocaml.doc " Name of the file containing the signature. "]
    ; cmi : Cmi_format.cmi_infos
    }
  [@@deriving sexp_of]

  include (
    Compiler_without_sexp.Env.Persistent_signature :
      module type of struct
        include Compiler_without_sexp.Env.Persistent_signature
      end
      with type t := Compiler_without_sexp.Env.Persistent_signature.t)
end

include (
  Compiler_without_sexp.Env :
    module type of struct
      include Compiler_without_sexp.Env
    end
    with type address := Compiler_without_sexp.Env.address
     and type constructor_usage := Compiler_without_sexp.Env.constructor_usage
     and type copy_of_types := Compiler_without_sexp.Env.copy_of_types
     and type error := Compiler_without_sexp.Env.error
     and type iter_cont := Compiler_without_sexp.Env.iter_cont
     and type summary := Compiler_without_sexp.Env.summary
     and type t := Compiler_without_sexp.Env.t
     and type type_descriptions := Compiler_without_sexp.Env.type_descriptions
     and module Persistent_signature := Compiler_without_sexp.Env.Persistent_signature)

