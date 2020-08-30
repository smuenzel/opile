(* This file is generated from ../raw-compiler/cmm.mli using sexpify *)

open! Core
type machtype_component = Compiler_without_sexp.Cmm.machtype_component =
  | Val 
  | Addr 
  | Int 
  | Float [@@deriving sexp_of]
type machtype = machtype_component array[@@deriving sexp_of]
type integer_comparison = Compiler_without_sexp.Cmm.integer_comparison =
  | Ceq 
  | Cne 
  | Clt 
  | Cgt 
  | Cle 
  | Cge [@@deriving sexp_of]
type float_comparison = Compiler_without_sexp.Cmm.float_comparison =
  | CFeq 
  | CFneq 
  | CFlt 
  | CFnlt 
  | CFgt 
  | CFngt 
  | CFle 
  | CFnle 
  | CFge 
  | CFnge [@@deriving sexp_of]
type label = int[@@deriving sexp_of]
type rec_flag = Compiler_without_sexp.Cmm.rec_flag =
  | Nonrecursive 
  | Recursive [@@deriving sexp_of]
type phantom_defining_expr = Compiler_without_sexp.Cmm.phantom_defining_expr
  =
  | Cphantom_const_int of Targetint.t 
  | Cphantom_const_symbol of string 
  | Cphantom_var of Backend_var.t 
  | Cphantom_offset_var of {
  var: Backend_var.t ;
  offset_in_words: int } 
  | Cphantom_read_field of {
  var: Backend_var.t ;
  field: int } 
  | Cphantom_read_symbol_field of {
  sym: string ;
  field: int } 
  | Cphantom_block of {
  tag: int ;
  fields: Backend_var.t list } [@@deriving sexp_of]
type memory_chunk = Compiler_without_sexp.Cmm.memory_chunk =
  | Byte_unsigned 
  | Byte_signed 
  | Sixteen_unsigned 
  | Sixteen_signed 
  | Thirtytwo_unsigned 
  | Thirtytwo_signed 
  | Word_int 
  | Word_val 
  | Single 
  | Double 
  | Double_u 
and operation = Compiler_without_sexp.Cmm.operation =
  | Capply of machtype 
  | Cextcall of string * machtype * bool * label option 
  | Cload of memory_chunk * Asttypes.mutable_flag 
  | Calloc 
  | Cstore of memory_chunk * Lambda.initialization_or_assignment 
  | Caddi 
  | Csubi 
  | Cmuli 
  | Cmulhi 
  | Cdivi 
  | Cmodi 
  | Cand 
  | Cor 
  | Cxor 
  | Clsl 
  | Clsr 
  | Casr 
  | Ccmpi of integer_comparison 
  | Caddv 
  | Cadda 
  | Ccmpa of integer_comparison 
  | Cnegf 
  | Cabsf 
  | Caddf 
  | Csubf 
  | Cmulf 
  | Cdivf 
  | Cfloatofint 
  | Cintoffloat 
  | Ccmpf of float_comparison 
  | Craise of Lambda.raise_kind 
  | Ccheckbound 
and expression = Compiler_without_sexp.Cmm.expression =
  | Cconst_int of int * Debuginfo.t 
  | Cconst_natint of nativeint * Debuginfo.t 
  | Cconst_float of float * Debuginfo.t 
  | Cconst_symbol of string * Debuginfo.t 
  | Cconst_pointer of int * Debuginfo.t 
  | Cconst_natpointer of nativeint * Debuginfo.t 
  | Cblockheader of nativeint * Debuginfo.t 
  | Cvar of Backend_var.t 
  | Clet of Backend_var.With_provenance.t * expression * expression 
  | Cphantom_let of Backend_var.With_provenance.t * phantom_defining_expr
  option * expression 
  | Cassign of Backend_var.t * expression 
  | Ctuple of expression list 
  | Cop of operation * expression list * Debuginfo.t 
  | Csequence of expression * expression 
  | Cifthenelse of expression * Debuginfo.t * expression * Debuginfo.t *
  expression * Debuginfo.t 
  | Cswitch of expression * int array * (expression * Debuginfo.t) array *
  Debuginfo.t 
  | Ccatch of rec_flag * (int * (Backend_var.With_provenance.t * machtype)
  list * expression * Debuginfo.t) list * expression 
  | Cexit of int * expression list 
  | Ctrywith of expression * Backend_var.With_provenance.t * expression *
  Debuginfo.t [@@deriving sexp_of]
type codegen_option = Compiler_without_sexp.Cmm.codegen_option =
  | Reduce_code_size 
  | No_CSE [@@deriving sexp_of]
type fundecl = Compiler_without_sexp.Cmm.fundecl =
  {
  fun_name: string ;
  fun_args: (Backend_var.With_provenance.t * machtype) list ;
  fun_body: expression ;
  fun_codegen_options: codegen_option list ;
  fun_dbg: Debuginfo.t }[@@deriving sexp_of]
type data_item = Compiler_without_sexp.Cmm.data_item =
  | Cdefine_symbol of string 
  | Cglobal_symbol of string 
  | Cint8 of int 
  | Cint16 of int 
  | Cint32 of nativeint 
  | Cint of nativeint 
  | Csingle of float 
  | Cdouble of float 
  | Csymbol_address of string 
  | Cstring of string 
  | Cskip of int 
  | Calign of int [@@deriving sexp_of]
type phrase = Compiler_without_sexp.Cmm.phrase =
  | Cfunction of fundecl 
  | Cdata of data_item list [@@deriving sexp_of]
include
  (Compiler_without_sexp.Cmm :
    module type of struct include Compiler_without_sexp.Cmm end with type
       codegen_option :=  Compiler_without_sexp.Cmm.codegen_option and type
       data_item :=  Compiler_without_sexp.Cmm.data_item and type
       expression :=  Compiler_without_sexp.Cmm.expression and type
       float_comparison :=  Compiler_without_sexp.Cmm.float_comparison and
      type  fundecl :=  Compiler_without_sexp.Cmm.fundecl and type
       integer_comparison :=  Compiler_without_sexp.Cmm.integer_comparison
      and type  label :=  Compiler_without_sexp.Cmm.label and type
       machtype :=  Compiler_without_sexp.Cmm.machtype and type
       machtype_component :=  Compiler_without_sexp.Cmm.machtype_component
      and type  memory_chunk :=  Compiler_without_sexp.Cmm.memory_chunk and
      type  operation :=  Compiler_without_sexp.Cmm.operation and type
       phantom_defining_expr := 
      Compiler_without_sexp.Cmm.phantom_defining_expr and type  phrase := 
      Compiler_without_sexp.Cmm.phrase and type  rec_flag := 
      Compiler_without_sexp.Cmm.rec_flag)
