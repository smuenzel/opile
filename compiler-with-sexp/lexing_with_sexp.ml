open Core

type position = Lexing.position = {
  pos_fname : string;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
}
[@@deriving sexp]

type lexbuf = Lexing.lexbuf = {
  refill_buff : lexbuf -> unit;
  mutable lex_buffer : bytes;
  mutable lex_buffer_len : int;
  mutable lex_abs_pos : int;
  mutable lex_start_pos : int;
  mutable lex_curr_pos : int;
  mutable lex_last_pos : int;
  mutable lex_last_action : int;
  mutable lex_eof_reached : bool;
  mutable lex_mem : int array;
  mutable lex_start_p : position;
  mutable lex_curr_p : position;
}
[@@deriving sexp]

type lex_tables = Lexing.lex_tables = {
  lex_base : string;
  lex_backtrk : string;
  lex_default : string;
  lex_trans : string;
  lex_check : string;
  lex_base_code : string;
  lex_backtrk_code : string;
  lex_default_code : string;
  lex_trans_code : string;
  lex_check_code : string;
  lex_code : string;
}
[@@deriving sexp]

include (
  Lexing :
    module type of Lexing
      with type position := position
       and type lexbuf := lexbuf
       and type lex_tables := lex_tables )
