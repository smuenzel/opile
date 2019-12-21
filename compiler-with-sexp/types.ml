open! Core
open! Asttypes

type type_expr = Compiler_without_sexp.Types.type_expr =
  { mutable desc : type_desc
  ; mutable level : int
  ; mutable scope : int
  ; id : int
  }

and type_desc = Compiler_without_sexp.Types.type_desc =
  | Tvar of string option
  | Tarrow of arg_label * type_expr * type_expr * commutable
  | Ttuple of type_expr list
  | Tconstr of Path.t * type_expr list * abbrev_memo ref
  | Tobject of type_expr * (Path.t * type_expr list) option ref
  | Tfield of string * field_kind * type_expr * type_expr
  | Tnil
  | Tlink of type_expr
  | Tsubst of type_expr
  | Tvariant of row_desc
  | Tunivar of string option
  | Tpoly of type_expr * type_expr list
  | Tpackage of Path.t * Longident.t list * type_expr list

and row_desc = Compiler_without_sexp.Types.row_desc =
  { row_fields : (label * row_field) list
  ; row_more : type_expr
  ; row_bound : unit
  ; row_closed : bool
  ; row_fixed : bool
  ; row_name : (Path.t * type_expr list) option
  }

and row_field = Compiler_without_sexp.Types.row_field =
  | Rpresent of type_expr option
  | Reither of bool * type_expr list * bool * row_field option ref
  | Rabsent

and abbrev_memo = Compiler_without_sexp.Types.abbrev_memo =
  | Mnil
  | Mcons of private_flag * Path.t * type_expr * type_expr * abbrev_memo
  | Mlink of abbrev_memo ref

and field_kind = Compiler_without_sexp.Types.field_kind =
  | Fvar of field_kind option ref
  | Fpresent
  | Fabsent

and commutable = Compiler_without_sexp.Types.commutable =
  | Cok
  | Cunknown
  | Clink of commutable ref
[@@deriving sexp_of]

module TypeOps = struct
  type t = type_expr [@@deriving sexp_of]

  include (
    Compiler_without_sexp.Types.TypeOps :
      module type of struct
        include Compiler_without_sexp.Types.TypeOps
      end
      with type t := Compiler_without_sexp.Types.TypeOps.t)
end

module Meths = struct
  include Compiler_without_sexp.Types.Meths

  let sexp_of_t sexp_of_a t =
    fold (fun key data acc -> (key, data) :: acc) t [] |> [%sexp_of: (string * a) list]
  ;;
end

module Vars = struct
  include Compiler_without_sexp.Types.Vars

  let sexp_of_t sexp_of_a t =
    fold (fun key data acc -> (key, data) :: acc) t [] |> [%sexp_of: (string * a) list]
  ;;
end

type value_description = Compiler_without_sexp.Types.value_description =
  { val_type : type_expr
  ; val_kind : value_kind
  ; val_loc : Location.t
  ; val_attributes : Parsetree.attributes
  }

and value_kind = Compiler_without_sexp.Types.value_kind =
  | Val_reg
  | Val_prim of Primitive.description
  | Val_ivar of mutable_flag * string
  | Val_self of
      (Ident.t * type_expr) Meths.t ref
      * (Ident.t * mutable_flag * virtual_flag * type_expr) Vars.t ref
      * string
      * type_expr
  | Val_anc of (string * Ident.t) list * string
  | Val_unbound of value_unbound_reason

and value_unbound_reason = Compiler_without_sexp.Types.value_unbound_reason =
  | Val_unbound_instance_variable
  | Val_unbound_ghost_recursive
[@@deriving sexp_of]

module Variance = struct
  type t = (Compiler_without_sexp.Types.Variance.t[@sexp.opaque]) [@@deriving sexp_of]

  type f = Compiler_without_sexp.Types.Variance.f =
    | May_pos
    | May_neg
    | May_weak
    | Inj
    | Pos
    | Neg
    | Inv
  [@@deriving sexp_of]

  include (
    Compiler_without_sexp.Types.Variance :
      module type of struct
        include Compiler_without_sexp.Types.Variance
      end
      with type f := Compiler_without_sexp.Types.Variance.f
       and type t := Compiler_without_sexp.Types.Variance.t)
end

type type_declaration = Compiler_without_sexp.Types.type_declaration =
  { type_params : type_expr list
  ; type_arity : int
  ; type_kind : type_kind
  ; type_private : private_flag
  ; type_manifest : type_expr option
  ; type_variance : Variance.t list
  ; type_is_newtype : bool
  ; type_expansion_scope : int
  ; type_loc : Location.t
  ; type_attributes : Parsetree.attributes
  ; type_immediate : bool
  ; type_unboxed : unboxed_status
  }

and type_kind = Compiler_without_sexp.Types.type_kind =
  | Type_abstract
  | Type_record of label_declaration list * record_representation
  | Type_variant of constructor_declaration list
  | Type_open

and record_representation = Compiler_without_sexp.Types.record_representation =
  | Record_regular
  | Record_float
  | Record_unboxed of bool
  | Record_inlined of int
  | Record_extension of Path.t

and label_declaration = Compiler_without_sexp.Types.label_declaration =
  { ld_id : Ident.t
  ; ld_mutable : mutable_flag
  ; ld_type : type_expr
  ; ld_loc : Location.t
  ; ld_attributes : Parsetree.attributes
  }

and constructor_declaration = Compiler_without_sexp.Types.constructor_declaration =
  { cd_id : Ident.t
  ; cd_args : constructor_arguments
  ; cd_res : type_expr option
  ; cd_loc : Location.t
  ; cd_attributes : Parsetree.attributes
  }

and constructor_arguments = Compiler_without_sexp.Types.constructor_arguments =
  | Cstr_tuple of type_expr list
  | Cstr_record of label_declaration list

and unboxed_status = Compiler_without_sexp.Types.unboxed_status = private
  { unboxed : bool
  ; default : bool
  }
[@@deriving sexp_of]

type extension_constructor = Compiler_without_sexp.Types.extension_constructor =
  { ext_type_path : Path.t
  ; ext_type_params : type_expr list
  ; ext_args : constructor_arguments
  ; ext_ret_type : type_expr option
  ; ext_private : private_flag
  ; ext_loc : Location.t
  ; ext_attributes : Parsetree.attributes
  }

and type_transparence = Compiler_without_sexp.Types.type_transparence =
  | Type_public
  | Type_new
  | Type_private
[@@deriving sexp_of]

module Concr = struct
  include Compiler_without_sexp.Types.Concr

  let sexp_of_t t = fold (fun key acc -> key :: acc) t [] |> [%sexp_of: string list]
end

type class_type = Compiler_without_sexp.Types.class_type =
  | Cty_constr of Path.t * type_expr list * class_type
  | Cty_signature of class_signature
  | Cty_arrow of arg_label * type_expr * class_type

and class_signature = Compiler_without_sexp.Types.class_signature =
  { csig_self : type_expr
  ; csig_vars : (Asttypes.mutable_flag * Asttypes.virtual_flag * type_expr) Vars.t
  ; csig_concr : Concr.t
  ; csig_inher : (Path.t * type_expr list) list
  }
[@@deriving sexp_of]

type class_declaration = Compiler_without_sexp.Types.class_declaration =
  { cty_params : type_expr list
  ; mutable cty_type : class_type
  ; cty_path : Path.t
  ; cty_new : type_expr option
  ; cty_variance : Variance.t list
  ; cty_loc : Location.t
  ; cty_attributes : Parsetree.attributes
  }
[@@deriving sexp_of]

type class_type_declaration = Compiler_without_sexp.Types.class_type_declaration =
  { clty_params : type_expr list
  ; clty_type : class_type
  ; clty_path : Path.t
  ; clty_variance : Variance.t list
  ; clty_loc : Location.t
  ; clty_attributes : Parsetree.attributes
  }
[@@deriving sexp_of]

type visibility = Compiler_without_sexp.Types.visibility =
  | Exported
  | Hidden
[@@deriving sexp_of]

type module_type = Compiler_without_sexp.Types.module_type =
  | Mty_ident of Path.t
  | Mty_signature of signature
  | Mty_functor of Ident.t * module_type option * module_type
  | Mty_alias of Path.t

and module_presence = Compiler_without_sexp.Types.module_presence =
  | Mp_present
  | Mp_absent

and signature = signature_item list

and signature_item = Compiler_without_sexp.Types.signature_item =
  | Sig_value of Ident.t * value_description * visibility
  | Sig_type of Ident.t * type_declaration * rec_status * visibility
  | Sig_typext of Ident.t * extension_constructor * ext_status * visibility
  | Sig_module of Ident.t * module_presence * module_declaration * rec_status * visibility
  | Sig_modtype of Ident.t * modtype_declaration * visibility
  | Sig_class of Ident.t * class_declaration * rec_status * visibility
  | Sig_class_type of Ident.t * class_type_declaration * rec_status * visibility

and module_declaration = Compiler_without_sexp.Types.module_declaration =
  { md_type : module_type
  ; md_attributes : Parsetree.attributes
  ; md_loc : Location.t
  }

and modtype_declaration = Compiler_without_sexp.Types.modtype_declaration =
  { mtd_type : module_type option
  ; mtd_attributes : Parsetree.attributes
  ; mtd_loc : Location.t
  }

and rec_status = Compiler_without_sexp.Types.rec_status =
  | Trec_not
  | Trec_first
  | Trec_next

and ext_status = Compiler_without_sexp.Types.ext_status =
  | Text_first
  | Text_next
  | Text_exception
[@@deriving sexp_of]

type constructor_description = Compiler_without_sexp.Types.constructor_description =
  { cstr_name : string
  ; cstr_res : type_expr
  ; cstr_existentials : type_expr list
  ; cstr_args : type_expr list
  ; cstr_arity : int
  ; cstr_tag : constructor_tag
  ; cstr_consts : int
  ; cstr_nonconsts : int
  ; cstr_normal : int
  ; cstr_generalized : bool
  ; cstr_private : private_flag
  ; cstr_loc : Location.t
  ; cstr_attributes : Parsetree.attributes
  ; cstr_inlined : type_declaration option
  }

and constructor_tag = Compiler_without_sexp.Types.constructor_tag =
  | Cstr_constant of int
  | Cstr_block of int
  | Cstr_unboxed
  | Cstr_extension of Path.t * bool
[@@deriving sexp_of]

type label_description = Compiler_without_sexp.Types.label_description =
  { lbl_name : string
  ; lbl_res : type_expr
  ; lbl_arg : type_expr
  ; lbl_mut : mutable_flag
  ; lbl_pos : int
  ; lbl_all : label_description array
  ; lbl_repres : record_representation
  ; lbl_private : private_flag
  ; lbl_loc : Location.t
  ; lbl_attributes : Parsetree.attributes
  }
[@@deriving sexp_of]

include (
  Compiler_without_sexp.Types :
    module type of struct
      include Compiler_without_sexp.Types
    end
    with type abbrev_memo := Compiler_without_sexp.Types.abbrev_memo
     and type class_declaration := Compiler_without_sexp.Types.class_declaration
     and type class_signature := Compiler_without_sexp.Types.class_signature
     and type class_type := Compiler_without_sexp.Types.class_type
     and type class_type_declaration := Compiler_without_sexp.Types.class_type_declaration
     and type commutable := Compiler_without_sexp.Types.commutable
     and type constructor_arguments := Compiler_without_sexp.Types.constructor_arguments
     and type constructor_declaration :=
          Compiler_without_sexp.Types.constructor_declaration
     and type constructor_description :=
          Compiler_without_sexp.Types.constructor_description
     and type constructor_tag := Compiler_without_sexp.Types.constructor_tag
     and type ext_status := Compiler_without_sexp.Types.ext_status
     and type extension_constructor := Compiler_without_sexp.Types.extension_constructor
     and type field_kind := Compiler_without_sexp.Types.field_kind
     and type label_declaration := Compiler_without_sexp.Types.label_declaration
     and type label_description := Compiler_without_sexp.Types.label_description
     and type modtype_declaration := Compiler_without_sexp.Types.modtype_declaration
     and type module_declaration := Compiler_without_sexp.Types.module_declaration
     and type module_presence := Compiler_without_sexp.Types.module_presence
     and type module_type := Compiler_without_sexp.Types.module_type
     and type rec_status := Compiler_without_sexp.Types.rec_status
     and type record_representation := Compiler_without_sexp.Types.record_representation
     and type row_desc := Compiler_without_sexp.Types.row_desc
     and type row_field := Compiler_without_sexp.Types.row_field
     and type signature := Compiler_without_sexp.Types.signature
     and type signature_item := Compiler_without_sexp.Types.signature_item
     and type type_declaration := Compiler_without_sexp.Types.type_declaration
     and type type_desc := Compiler_without_sexp.Types.type_desc
     and type type_expr := Compiler_without_sexp.Types.type_expr
     and type type_kind := Compiler_without_sexp.Types.type_kind
     and type type_transparence := Compiler_without_sexp.Types.type_transparence
     and type unboxed_status := Compiler_without_sexp.Types.unboxed_status
     and type value_description := Compiler_without_sexp.Types.value_description
     and type value_kind := Compiler_without_sexp.Types.value_kind
     and type value_unbound_reason := Compiler_without_sexp.Types.value_unbound_reason
     and type visibility := Compiler_without_sexp.Types.visibility
     and module Concr := Compiler_without_sexp.Types.Concr
     and module Meths := Compiler_without_sexp.Types.Meths
     and module TypeOps := Compiler_without_sexp.Types.TypeOps
     and module Variance := Compiler_without_sexp.Types.Variance
     and module Vars := Compiler_without_sexp.Types.Vars)

