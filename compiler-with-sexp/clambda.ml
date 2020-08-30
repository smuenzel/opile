(* This file is generated from ../raw-compiler/clambda.mli using sexpify *)

open! Core
open! Asttypes
open! Lambda
type function_label = string[@@deriving sexp_of]
type ustructured_constant =
  Compiler_without_sexp.Clambda.ustructured_constant =
  | Uconst_float of float 
  | Uconst_int32 of int32 
  | Uconst_int64 of int64 
  | Uconst_nativeint of nativeint 
  | Uconst_block of int * uconstant list 
  | Uconst_float_array of float list 
  | Uconst_string of string 
  | Uconst_closure of ufunction list * string * uconstant list 
and uconstant = Compiler_without_sexp.Clambda.uconstant =
  | Uconst_ref of string * ustructured_constant option 
  | Uconst_int of int 
  | Uconst_ptr of int 
and uphantom_defining_expr =
  Compiler_without_sexp.Clambda.uphantom_defining_expr =
  | Uphantom_const of uconstant 
  | Uphantom_var of Backend_var.t 
  | Uphantom_offset_var of {
  var: Backend_var.t ;
  offset_in_words: int } 
  | Uphantom_read_field of {
  var: Backend_var.t ;
  field: int } 
  | Uphantom_read_symbol_field of {
  sym: string ;
  field: int } 
  | Uphantom_block of {
  tag: int ;
  fields: Backend_var.t list } 
and ulambda = Compiler_without_sexp.Clambda.ulambda =
  | Uvar of Backend_var.t 
  | Uconst of uconstant 
  | Udirect_apply of function_label * ulambda list * Debuginfo.t 
  | Ugeneric_apply of ulambda * ulambda list * Debuginfo.t 
  | Uclosure of ufunction list * ulambda list 
  | Uoffset of ulambda * int 
  | Ulet of mutable_flag * value_kind * Backend_var.With_provenance.t *
  ulambda * ulambda 
  | Uphantom_let of Backend_var.With_provenance.t * uphantom_defining_expr
  option * ulambda 
  | Uletrec of (Backend_var.With_provenance.t * ulambda) list * ulambda 
  | Uprim of Clambda_primitives.primitive * ulambda list * Debuginfo.t 
  | Uswitch of ulambda * ulambda_switch * Debuginfo.t 
  | Ustringswitch of ulambda * (string * ulambda) list * ulambda option 
  | Ustaticfail of int * ulambda list 
  | Ucatch of int * (Backend_var.With_provenance.t * value_kind) list *
  ulambda * ulambda 
  | Utrywith of ulambda * Backend_var.With_provenance.t * ulambda 
  | Uifthenelse of ulambda * ulambda * ulambda 
  | Usequence of ulambda * ulambda 
  | Uwhile of ulambda * ulambda 
  | Ufor of Backend_var.With_provenance.t * ulambda * ulambda *
  direction_flag * ulambda 
  | Uassign of Backend_var.t * ulambda 
  | Usend of meth_kind * ulambda * ulambda * ulambda list * Debuginfo.t 
  | Uunreachable 
and ufunction = Compiler_without_sexp.Clambda.ufunction =
  {
  label: function_label ;
  arity: int ;
  params: (Backend_var.With_provenance.t * value_kind) list ;
  return: value_kind ;
  body: ulambda ;
  dbg: Debuginfo.t ;
  env: Backend_var.t option }
and ulambda_switch = Compiler_without_sexp.Clambda.ulambda_switch =
  {
  us_index_consts: int array ;
  us_actions_consts: ulambda array ;
  us_index_blocks: int array ;
  us_actions_blocks: ulambda array }[@@deriving sexp_of]
type function_description =
  Compiler_without_sexp.Clambda.function_description =
  {
  fun_label: function_label ;
  fun_arity: int ;
  mutable fun_closed: bool ;
  mutable fun_inline: (Backend_var.With_provenance.t list * ulambda) option ;
  mutable fun_float_const_prop: bool }[@@deriving sexp_of]
type value_approximation = Compiler_without_sexp.Clambda.value_approximation
  =
  | Value_closure of function_description * value_approximation 
  | Value_tuple of value_approximation array 
  | Value_unknown 
  | Value_const of uconstant 
  | Value_global_field of string * int [@@deriving sexp_of]
type usymbol_provenance = Compiler_without_sexp.Clambda.usymbol_provenance =
  {
  original_idents: Ident.t list ;
  module_path: Path.t }[@@deriving sexp_of]
type uconstant_block_field =
  Compiler_without_sexp.Clambda.uconstant_block_field =
  | Uconst_field_ref of string 
  | Uconst_field_int of int [@@deriving sexp_of]
type preallocated_block = Compiler_without_sexp.Clambda.preallocated_block =
  {
  symbol: string ;
  exported: bool ;
  tag: int ;
  fields: uconstant_block_field option list ;
  provenance: usymbol_provenance option }[@@deriving sexp_of]
type preallocated_constant =
  Compiler_without_sexp.Clambda.preallocated_constant =
  {
  symbol: string ;
  exported: bool ;
  definition: ustructured_constant ;
  provenance: usymbol_provenance option }[@@deriving sexp_of]
type with_constants =
  (ulambda * preallocated_block list * preallocated_constant list)[@@deriving
                                                                    sexp_of]
include
  (Compiler_without_sexp.Clambda :
    module type of struct include Compiler_without_sexp.Clambda end with type
       function_description := 
      Compiler_without_sexp.Clambda.function_description and type
       function_label :=  Compiler_without_sexp.Clambda.function_label and
      type  preallocated_block := 
      Compiler_without_sexp.Clambda.preallocated_block and type
       preallocated_constant := 
      Compiler_without_sexp.Clambda.preallocated_constant and type
       uconstant :=  Compiler_without_sexp.Clambda.uconstant and type
       uconstant_block_field := 
      Compiler_without_sexp.Clambda.uconstant_block_field and type
       ufunction :=  Compiler_without_sexp.Clambda.ufunction and type
       ulambda :=  Compiler_without_sexp.Clambda.ulambda and type
       ulambda_switch :=  Compiler_without_sexp.Clambda.ulambda_switch and
      type  uphantom_defining_expr := 
      Compiler_without_sexp.Clambda.uphantom_defining_expr and type
       ustructured_constant := 
      Compiler_without_sexp.Clambda.ustructured_constant and type
       usymbol_provenance := 
      Compiler_without_sexp.Clambda.usymbol_provenance and type
       value_approximation := 
      Compiler_without_sexp.Clambda.value_approximation and type
       with_constants :=  Compiler_without_sexp.Clambda.with_constants)
