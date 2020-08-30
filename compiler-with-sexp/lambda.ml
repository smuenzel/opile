(* This file is generated from ../raw-compiler/lambda.mli using sexpify *)

open! Core
open! Asttypes
type compile_time_constant =
  Compiler_without_sexp.Lambda.compile_time_constant =
  | Big_endian 
  | Word_size 
  | Int_size 
  | Max_wosize 
  | Ostype_unix 
  | Ostype_win32 
  | Ostype_cygwin 
  | Backend_type [@@deriving sexp_of]
type immediate_or_pointer = Compiler_without_sexp.Lambda.immediate_or_pointer
  =
  | Immediate 
  | Pointer [@@deriving sexp_of]
type initialization_or_assignment =
  Compiler_without_sexp.Lambda.initialization_or_assignment =
  | Assignment 
  | Heap_initialization 
  | Root_initialization [@@deriving sexp_of]
type is_safe = Compiler_without_sexp.Lambda.is_safe =
  | Safe 
  | Unsafe [@@deriving sexp_of]
type primitive = Compiler_without_sexp.Lambda.primitive =
  | Pidentity 
  | Pbytes_to_string 
  | Pbytes_of_string 
  | Pignore 
  | Prevapply 
  | Pdirapply 
  | Pgetglobal of Ident.t 
  | Psetglobal of Ident.t 
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
  | Pstring_load_16 of bool 
  | Pstring_load_32 of bool 
  | Pstring_load_64 of bool 
  | Pbytes_load_16 of bool 
  | Pbytes_load_32 of bool 
  | Pbytes_load_64 of bool 
  | Pbytes_set_16 of bool 
  | Pbytes_set_32 of bool 
  | Pbytes_set_64 of bool 
  | Pbigstring_load_16 of bool 
  | Pbigstring_load_32 of bool 
  | Pbigstring_load_64 of bool 
  | Pbigstring_set_16 of bool 
  | Pbigstring_set_32 of bool 
  | Pbigstring_set_64 of bool 
  | Pctconst of compile_time_constant 
  | Pbswap16 
  | Pbbswap of boxed_integer 
  | Pint_as_pointer 
  | Popaque 
and integer_comparison = Compiler_without_sexp.Lambda.integer_comparison =
  | Ceq 
  | Cne 
  | Clt 
  | Cgt 
  | Cle 
  | Cge 
and float_comparison = Compiler_without_sexp.Lambda.float_comparison =
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
and array_kind = Compiler_without_sexp.Lambda.array_kind =
  | Pgenarray 
  | Paddrarray 
  | Pintarray 
  | Pfloatarray 
and value_kind = Compiler_without_sexp.Lambda.value_kind =
  | Pgenval 
  | Pfloatval 
  | Pboxedintval of boxed_integer 
  | Pintval 
and block_shape = value_kind list option
and boxed_integer = Compiler_without_sexp.Lambda.boxed_integer =
  | Pnativeint 
  | Pint32 
  | Pint64 
and bigarray_kind = Compiler_without_sexp.Lambda.bigarray_kind =
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
and bigarray_layout = Compiler_without_sexp.Lambda.bigarray_layout =
  | Pbigarray_unknown_layout 
  | Pbigarray_c_layout 
  | Pbigarray_fortran_layout 
and raise_kind = Compiler_without_sexp.Lambda.raise_kind =
  | Raise_regular 
  | Raise_reraise 
  | Raise_notrace [@@deriving sexp_of]
type structured_constant = Compiler_without_sexp.Lambda.structured_constant
  =
  | Const_base of constant 
  | Const_pointer of int 
  | Const_block of int * structured_constant list 
  | Const_float_array of string list 
  | Const_immstring of string [@@deriving sexp_of]
type inline_attribute = Compiler_without_sexp.Lambda.inline_attribute =
  | Always_inline 
  | Never_inline 
  | Unroll of int 
  | Default_inline [@@deriving sexp_of]
type specialise_attribute = Compiler_without_sexp.Lambda.specialise_attribute
  =
  | Always_specialise 
  | Never_specialise 
  | Default_specialise [@@deriving sexp_of]
type local_attribute = Compiler_without_sexp.Lambda.local_attribute =
  | Always_local 
  | Never_local 
  | Default_local [@@deriving sexp_of]
type function_kind = Compiler_without_sexp.Lambda.function_kind =
  | Curried 
  | Tupled [@@deriving sexp_of]
type let_kind = Compiler_without_sexp.Lambda.let_kind =
  | Strict 
  | Alias 
  | StrictOpt 
  | Variable [@@deriving sexp_of]
type meth_kind = Compiler_without_sexp.Lambda.meth_kind =
  | Self 
  | Public 
  | Cached [@@deriving sexp_of]
type shared_code = (int * int) list[@@deriving sexp_of]
type function_attribute = Compiler_without_sexp.Lambda.function_attribute =
  {
  inline: inline_attribute ;
  specialise: specialise_attribute ;
  local: local_attribute ;
  is_a_functor: bool ;
  stub: bool }[@@deriving sexp_of]
type lambda = Compiler_without_sexp.Lambda.lambda =
  | Lvar of Ident.t 
  | Lconst of structured_constant 
  | Lapply of lambda_apply 
  | Lfunction of lfunction 
  | Llet of let_kind * value_kind * Ident.t * lambda * lambda 
  | Lletrec of (Ident.t * lambda) list * lambda 
  | Lprim of primitive * lambda list * Location.t 
  | Lswitch of lambda * lambda_switch * Location.t 
  | Lstringswitch of lambda * (string * lambda) list * lambda option *
  Location.t 
  | Lstaticraise of int * lambda list 
  | Lstaticcatch of lambda * (int * (Ident.t * value_kind) list) * lambda 
  | Ltrywith of lambda * Ident.t * lambda 
  | Lifthenelse of lambda * lambda * lambda 
  | Lsequence of lambda * lambda 
  | Lwhile of lambda * lambda 
  | Lfor of Ident.t * lambda * lambda * direction_flag * lambda 
  | Lassign of Ident.t * lambda 
  | Lsend of meth_kind * lambda * lambda * lambda list * Location.t 
  | Levent of lambda * lambda_event 
  | Lifused of Ident.t * lambda 
and lfunction = Compiler_without_sexp.Lambda.lfunction =
  {
  kind: function_kind ;
  params: (Ident.t * value_kind) list ;
  return: value_kind ;
  body: lambda ;
  attr: function_attribute ;
  loc: Location.t }
and lambda_apply = Compiler_without_sexp.Lambda.lambda_apply =
  {
  ap_func: lambda ;
  ap_args: lambda list ;
  ap_loc: Location.t ;
  ap_should_be_tailcall: bool ;
  ap_inlined: inline_attribute ;
  ap_specialised: specialise_attribute }
and lambda_switch = Compiler_without_sexp.Lambda.lambda_switch =
  {
  sw_numconsts: int ;
  sw_consts: (int * lambda) list ;
  sw_numblocks: int ;
  sw_blocks: (int * lambda) list ;
  sw_failaction: lambda option }
and lambda_event = Compiler_without_sexp.Lambda.lambda_event =
  {
  lev_loc: Location.t ;
  lev_kind: lambda_event_kind ;
  lev_repr: int ref option ;
  lev_env: Env.t }
and lambda_event_kind = Compiler_without_sexp.Lambda.lambda_event_kind =
  | Lev_before 
  | Lev_after of Types.type_expr 
  | Lev_function 
  | Lev_pseudo 
  | Lev_module_definition of Ident.t [@@deriving sexp_of]
type program = Compiler_without_sexp.Lambda.program =
  {
  module_ident: Ident.t ;
  main_module_block_size: int ;
  required_globals: Ident.Set.t ;
  code: lambda }[@@deriving sexp_of]
include
  (Compiler_without_sexp.Lambda :
    module type of struct include Compiler_without_sexp.Lambda end with type
       array_kind :=  Compiler_without_sexp.Lambda.array_kind and type
       bigarray_kind :=  Compiler_without_sexp.Lambda.bigarray_kind and type
       bigarray_layout :=  Compiler_without_sexp.Lambda.bigarray_layout and
      type  block_shape :=  Compiler_without_sexp.Lambda.block_shape and type
       boxed_integer :=  Compiler_without_sexp.Lambda.boxed_integer and type
       compile_time_constant := 
      Compiler_without_sexp.Lambda.compile_time_constant and type
       float_comparison :=  Compiler_without_sexp.Lambda.float_comparison and
      type  function_attribute := 
      Compiler_without_sexp.Lambda.function_attribute and type
       function_kind :=  Compiler_without_sexp.Lambda.function_kind and type
       immediate_or_pointer := 
      Compiler_without_sexp.Lambda.immediate_or_pointer and type
       initialization_or_assignment := 
      Compiler_without_sexp.Lambda.initialization_or_assignment and type
       inline_attribute :=  Compiler_without_sexp.Lambda.inline_attribute and
      type  integer_comparison := 
      Compiler_without_sexp.Lambda.integer_comparison and type  is_safe := 
      Compiler_without_sexp.Lambda.is_safe and type  lambda := 
      Compiler_without_sexp.Lambda.lambda and type  lambda_apply := 
      Compiler_without_sexp.Lambda.lambda_apply and type  lambda_event := 
      Compiler_without_sexp.Lambda.lambda_event and type
       lambda_event_kind :=  Compiler_without_sexp.Lambda.lambda_event_kind
      and type  lambda_switch :=  Compiler_without_sexp.Lambda.lambda_switch
      and type  let_kind :=  Compiler_without_sexp.Lambda.let_kind and type
       lfunction :=  Compiler_without_sexp.Lambda.lfunction and type
       local_attribute :=  Compiler_without_sexp.Lambda.local_attribute and
      type  meth_kind :=  Compiler_without_sexp.Lambda.meth_kind and type
       primitive :=  Compiler_without_sexp.Lambda.primitive and type
       program :=  Compiler_without_sexp.Lambda.program and type
       raise_kind :=  Compiler_without_sexp.Lambda.raise_kind and type
       shared_code :=  Compiler_without_sexp.Lambda.shared_code and type
       specialise_attribute := 
      Compiler_without_sexp.Lambda.specialise_attribute and type
       structured_constant := 
      Compiler_without_sexp.Lambda.structured_constant and type
       value_kind :=  Compiler_without_sexp.Lambda.value_kind)
