(* This file is generated from ../raw-compiler/lexing.mli using sexpify *)

open! Core
type position = Compiler_without_sexp.Lexing.position =
  {
  pos_fname: string ;
  pos_lnum: int ;
  pos_bol: int ;
  pos_cnum: int }[@@deriving sexp_of]
type lexbuf = Compiler_without_sexp.Lexing.lexbuf =
  {
  refill_buff: lexbuf -> unit ;
  mutable lex_buffer: bytes ;
  mutable lex_buffer_len: int ;
  mutable lex_abs_pos: int ;
  mutable lex_start_pos: int ;
  mutable lex_curr_pos: int ;
  mutable lex_last_pos: int ;
  mutable lex_last_action: int ;
  mutable lex_eof_reached: bool ;
  mutable lex_mem: int array ;
  mutable lex_start_p: position ;
  mutable lex_curr_p: position }[@@deriving sexp_of]
type lex_tables = Compiler_without_sexp.Lexing.lex_tables =
  {
  lex_base: string ;
  lex_backtrk: string ;
  lex_default: string ;
  lex_trans: string ;
  lex_check: string ;
  lex_base_code: string ;
  lex_backtrk_code: string ;
  lex_default_code: string ;
  lex_trans_code: string ;
  lex_check_code: string ;
  lex_code: string }[@@deriving sexp_of]
include
  (Compiler_without_sexp.Lexing :
    module type of struct include Compiler_without_sexp.Lexing end with type
       lex_tables :=  Compiler_without_sexp.Lexing.lex_tables and type
       lexbuf :=  Compiler_without_sexp.Lexing.lexbuf and type  position := 
      Compiler_without_sexp.Lexing.position)
