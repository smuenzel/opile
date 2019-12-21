open! Core

type boxed_integer = Compiler_without_sexp.Primitive.boxed_integer =
  | Pnativeint
  | Pint32
  | Pint64
[@@deriving sexp_of]

type native_repr = Compiler_without_sexp.Primitive.native_repr =
  | Same_as_ocaml_repr
  | Unboxed_float
  | Unboxed_integer of boxed_integer
  | Untagged_int
[@@deriving sexp_of]

type description = Compiler_without_sexp.Primitive.description = private
  { prim_name : string
  ; prim_arity : int
  ; prim_alloc : bool
  ; prim_native_name : string
  ; prim_native_repr_args : native_repr list
  ; prim_native_repr_res : native_repr
  }
[@@deriving sexp_of]

type error = Compiler_without_sexp.Primitive.error =
  | Old_style_float_with_native_repr_attribute
  | Old_style_noalloc_with_noalloc_attribute
  | No_native_primitive_with_repr_attribute
[@@deriving sexp_of]

include (
  Compiler_without_sexp.Primitive :
    module type of struct
      include Compiler_without_sexp.Primitive
    end
    with type boxed_integer := Compiler_without_sexp.Primitive.boxed_integer
     and type description := Compiler_without_sexp.Primitive.description
     and type error := Compiler_without_sexp.Primitive.error
     and type native_repr := Compiler_without_sexp.Primitive.native_repr)

