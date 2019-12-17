(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** {1 Source code locations (ranges of positions), used in parsetree}

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

type t = Warnings.loc = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
  loc_ghost: bool;
}

(** Note on the use of Lexing.position in this module.
   If [pos_fname = ""], then use [!input_name] instead.
   If [pos_lnum = -1], then [pos_bol = 0]. Use [pos_cnum] and
     re-parse the file to get the line and character numbers.
   Else all fields are correct.
*)

val none : t
(** An arbitrary value of type [t]; describes an empty ghost range. *)

val in_file : string -> t
(** Return an empty ghost range located in a given file. *)

val init : Lexing.lexbuf -> string -> unit
(** Set the file name and line number of the [lexbuf] to be the start
    of the named file. *)

val curr : Lexing.lexbuf -> t
(** Get the location of the current token from the [lexbuf]. *)

val symbol_rloc: unit -> t
val symbol_gloc: unit -> t

(** [rhs_loc n] returns the location of the symbol at position [n], starting
  at 1, in the current parser rule. *)
val rhs_loc: int -> t

val rhs_interval: int -> int -> t

val get_pos_info: Lexing.position -> string * int * int
(** file, line, char *)

type 'a loc = {
  txt : 'a;
  loc : t;
}

val mknoloc : 'a -> 'a loc
val mkloc : 'a -> t -> 'a loc


(** {1 Input info} *)

val input_name: string ref
val input_lexbuf: Lexing.lexbuf option ref


(** {1 Toplevel-specific functions} *)

val echo_eof: unit -> unit
val reset: unit -> unit


(** {1 Printing locations} *)

val rewrite_absolute_path: string -> string
    (** rewrite absolute path to honor the BUILD_PATH_PREFIX_MAP
        variable (https://reproducible-builds.org/specs/build-path-prefix-map/)
        if it is set. *)

val absolute_path: string -> string

val show_filename: string -> string
    (** In -absname mode, return the absolute path for this filename.
        Otherwise, returns the filename unchanged. *)

val print_filename: formatter -> string -> unit

val print_loc: formatter -> t -> unit
val print_locs: formatter -> t list -> unit


(** {1 Toplevel-specific location highlighting} *)

val highlight_terminfo:
  Lexing.lexbuf -> formatter -> t list -> unit


(** {1 Reporting errors and warnings} *)

(** {2 The type of reports and report printers} *)

