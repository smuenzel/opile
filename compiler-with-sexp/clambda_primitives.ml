(* This file is generated from ../raw-compiler/clambda_primitives.mli using sexpify *)

open! Core
type mutable_flag = Asttypes.mutable_flag[@@deriving sexp_of]
type immediate_or_pointer = Lambda.immediate_or_pointer[@@deriving sexp_of]
type initialization_or_assignment = Lambda.initialization_or_assignment
[@@deriving sexp_of]
type is_safe = Lambda.is_safe[@@deriving sexp_of]
type boxed = Compiler_without_sexp.Clambda_primitives.boxed =
  | Boxed 
  | Unboxed [@@deriving sexp_of]
type memory_access_size =
  Compiler_without_sexp.Clambda_primitives.memory_access_size =
  | Sixteen 
  | Thirty_two 
  | Sixty_four [@@deriving sexp_of]
type primitive = Compiler_without_sexp.Clambda_primitives.primitive =
  | Pread_symbol of string 
  | Pmakeblock of int * mutable_flag * block_shape 
  | Pfield of int 
  | Pfield_computed 
  | Psetfield of int * immediate_or_pointer * initialization_or_assignment 
  | Psetfield_computed of immediate_or_pointer * initialization_or_assignment
  
  | Pfloatfield of int 
  | Psetfloatfield of int * initialization_or_assignment 
  | Pduprecord of Types.record_representation * int 
  | Pccall of Primitive.description 
  | Praise of raise_kind 
  | Psequand 
  | Psequor 
  | Pnot 
  | Pnegint 
  | Paddint 
  | Psubint 
  | Pmulint 
  | Pdivint of is_safe 
  | Pmodint of is_safe 
  | Pandint 
  | Porint 
  | Pxorint 
  | Plslint 
  | Plsrint 
  | Pasrint 
  | Pintcomp of integer_comparison 
  | Poffsetint of int 
  | Poffsetref of int 
  | Pintoffloat 
  | Pfloatofint 
  | Pnegfloat 
  | Pabsfloat 
  | Paddfloat 
  | Psubfloat 
  | Pmulfloat 
  | Pdivfloat 
  | Pfloatcomp of float_comparison 
  | Pstringlength 
  | Pstringrefu 
  | Pstringrefs 
  | Pbyteslength 
  | Pbytesrefu 
  | Pbytessetu 
  | Pbytesrefs 
  | Pbytessets 
  | Pmakearray of array_kind * mutable_flag 
  | Pduparray of array_kind * mutable_flag 
  | Parraylength of array_kind 
  | Parrayrefu of array_kind 
  | Parraysetu of array_kind 
  | Parrayrefs of array_kind 
  | Parraysets of array_kind 
  | Pisint 
  | Pisout 
  | Pbintofint of boxed_integer 
  | Pintofbint of boxed_integer 
  | Pcvtbint of boxed_integer * boxed_integer 
  | Pnegbint of boxed_integer 
  | Paddbint of boxed_integer 
  | Psubbint of boxed_integer 
  | Pmulbint of boxed_integer 
  | Pdivbint of {
  size: boxed_integer ;
  is_safe: is_safe } 
  | Pmodbint of {
  size: boxed_integer ;
  is_safe: is_safe } 
  | Pandbint of boxed_integer 
  | Porbint of boxed_integer 
  | Pxorbint of boxed_integer 
  | Plslbint of boxed_integer 
  | Plsrbint of boxed_integer 
  | Pasrbint of boxed_integer 
  | Pbintcomp of boxed_integer * integer_comparison 
  | Pbigarrayref of bool * int * bigarray_kind * bigarray_layout 
  | Pbigarrayset of bool * int * bigarray_kind * bigarray_layout 
  | Pbigarraydim of int 
  | Pstring_load of (memory_access_size * is_safe) 
  | Pbytes_load of (memory_access_size * is_safe) 
  | Pbytes_set of (memory_access_size * is_safe) 
  | Pbigstring_load of (memory_access_size * is_safe) 
  | Pbigstring_set of (memory_access_size * is_safe) 
  | Pbswap16 
  | Pbbswap of boxed_integer 
  | Pint_as_pointer 
  | Popaque 
and integer_comparison =
  Compiler_without_sexp.Clambda_primitives.integer_comparison =
  | Ceq 
  | Cne 
  | Clt 
  | Cgt 
  | Cle 
  | Cge 
and float_comparison =
  Compiler_without_sexp.Clambda_primitives.float_comparison =
  | CFeq 
  | CFneq 
  | CFlt 
  | CFnlt 
  | CFgt 
  | CFngt 
  | CFle 
  | CFnle 
  | CFge 
  | CFnge 
and array_kind = Compiler_without_sexp.Clambda_primitives.array_kind =
  | Pgenarray 
  | Paddrarray 
  | Pintarray 
  | Pfloatarray 
and value_kind = Compiler_without_sexp.Clambda_primitives.value_kind =
  | Pgenval 
  | Pfloatval 
  | Pboxedintval of boxed_integer 
  | Pintval 
and block_shape = Lambda.block_shape
and boxed_integer = Compiler_without_sexp.Clambda_primitives.boxed_integer =
  | Pnativeint 
  | Pint32 
  | Pint64 
and bigarray_kind = Compiler_without_sexp.Clambda_primitives.bigarray_kind =
  | Pbigarray_unknown 
  | Pbigarray_float32 
  | Pbigarray_float64 
  | Pbigarray_sint8 
  | Pbigarray_uint8 
  | Pbigarray_sint16 
  | Pbigarray_uint16 
  | Pbigarray_int32 
  | Pbigarray_int64 
  | Pbigarray_caml_int 
  | Pbigarray_native_int 
  | Pbigarray_complex32 
  | Pbigarray_complex64 
and bigarray_layout =
  Compiler_without_sexp.Clambda_primitives.bigarray_layout =
  | Pbigarray_unknown_layout 
  | Pbigarray_c_layout 
  | Pbigarray_fortran_layout 
and raise_kind = Compiler_without_sexp.Clambda_primitives.raise_kind =
  | Raise_regular 
  | Raise_reraise 
  | Raise_notrace [@@deriving sexp_of]
include
  (Compiler_without_sexp.Clambda_primitives :
    module type of
      struct include Compiler_without_sexp.Clambda_primitives end with type
       array_kind :=  Compiler_without_sexp.Clambda_primitives.array_kind and
      type  bigarray_kind := 
      Compiler_without_sexp.Clambda_primitives.bigarray_kind and type
       bigarray_layout := 
      Compiler_without_sexp.Clambda_primitives.bigarray_layout and type
       block_shape :=  Compiler_without_sexp.Clambda_primitives.block_shape
      and type  boxed :=  Compiler_without_sexp.Clambda_primitives.boxed and
      type  boxed_integer := 
      Compiler_without_sexp.Clambda_primitives.boxed_integer and type
       float_comparison := 
      Compiler_without_sexp.Clambda_primitives.float_comparison and type
       immediate_or_pointer := 
      Compiler_without_sexp.Clambda_primitives.immediate_or_pointer and type
       initialization_or_assignment := 
      Compiler_without_sexp.Clambda_primitives.initialization_or_assignment
      and type  integer_comparison := 
      Compiler_without_sexp.Clambda_primitives.integer_comparison and type
       is_safe :=  Compiler_without_sexp.Clambda_primitives.is_safe and type
       memory_access_size := 
      Compiler_without_sexp.Clambda_primitives.memory_access_size and type
       mutable_flag :=  Compiler_without_sexp.Clambda_primitives.mutable_flag
      and type  primitive := 
      Compiler_without_sexp.Clambda_primitives.primitive and type
       raise_kind :=  Compiler_without_sexp.Clambda_primitives.raise_kind and
      type  value_kind := 
      Compiler_without_sexp.Clambda_primitives.value_kind)
