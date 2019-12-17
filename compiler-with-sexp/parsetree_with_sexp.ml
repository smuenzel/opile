open Core
open Asttypes_with_sexp

type constant = Parsetree.constant =
  | Pconst_integer of string * char option
  | Pconst_char of char
  | Pconst_string of string * string option
  | Pconst_float of string * char option
[@@deriving sexp]

type attribute = Parsetree.attribute = {
  attr_name : string loc;
  attr_payload : payload;
  attr_loc : Location_with_sexp.t;
}

and extension = string loc * payload

and attributes = attribute list

and payload = Parsetree.payload =
  | PStr of structure
  | PSig of signature
  | PTyp of core_type
  | PPat of pattern * expression option

and core_type = Parsetree.core_type = {
  ptyp_desc : core_type_desc;
  ptyp_loc : Location_with_sexp.t;
  ptyp_loc_stack : Location_with_sexp.t list;
  ptyp_attributes : attributes;
}

and core_type_desc = Parsetree.core_type_desc =
  | Ptyp_any
  | Ptyp_var of string
  | Ptyp_arrow of arg_label * core_type * core_type
  | Ptyp_tuple of core_type list
  | Ptyp_constr of Longident_with_sexp.t loc * core_type list
  | Ptyp_object of object_field list * closed_flag
  | Ptyp_class of Longident_with_sexp.t loc * core_type list
  | Ptyp_alias of core_type * string
  | Ptyp_variant of row_field list * closed_flag * label list option
  | Ptyp_poly of string loc list * core_type
  | Ptyp_package of package_type
  | Ptyp_extension of extension

and package_type =
  Longident_with_sexp.t loc * (Longident_with_sexp.t loc * core_type) list

and row_field = Parsetree.row_field = {
  prf_desc : row_field_desc;
  prf_loc : Location_with_sexp.t;
  prf_attributes : attributes;
}

and row_field_desc = Parsetree.row_field_desc =
  | Rtag of label loc * bool * core_type list
  | Rinherit of core_type

and object_field = Parsetree.object_field = {
  pof_desc : object_field_desc;
  pof_loc : Location_with_sexp.t;
  pof_attributes : attributes;
}

and object_field_desc = Parsetree.object_field_desc =
  | Otag of label loc * core_type
  | Oinherit of core_type

and pattern = Parsetree.pattern = {
  ppat_desc : pattern_desc;
  ppat_loc : Location_with_sexp.t;
  ppat_loc_stack : Location_with_sexp.t list;
  ppat_attributes : attributes;
}

and pattern_desc = Parsetree.pattern_desc =
  | Ppat_any
  | Ppat_var of string loc
  | Ppat_alias of pattern * string loc
  | Ppat_constant of constant
  | Ppat_interval of constant * constant
  | Ppat_tuple of pattern list
  | Ppat_construct of Longident_with_sexp.t loc * pattern option
  | Ppat_variant of label * pattern option
  | Ppat_record of (Longident_with_sexp.t loc * pattern) list * closed_flag
  | Ppat_array of pattern list
  | Ppat_or of pattern * pattern
  | Ppat_constraint of pattern * core_type
  | Ppat_type of Longident_with_sexp.t loc
  | Ppat_lazy of pattern
  | Ppat_unpack of string loc
  | Ppat_exception of pattern
  | Ppat_extension of extension
  | Ppat_open of Longident_with_sexp.t loc * pattern

and expression = Parsetree.expression = {
  pexp_desc : expression_desc;
  pexp_loc : Location_with_sexp.t;
  pexp_loc_stack : Location_with_sexp.t list;
  pexp_attributes : attributes;
}

and expression_desc = Parsetree.expression_desc =
  | Pexp_ident of Longident_with_sexp.t loc
  | Pexp_constant of constant
  | Pexp_let of rec_flag * value_binding list * expression
  | Pexp_function of case list
  | Pexp_fun of arg_label * expression option * pattern * expression
  | Pexp_apply of expression * (arg_label * expression) list
  | Pexp_match of expression * case list
  | Pexp_try of expression * case list
  | Pexp_tuple of expression list
  | Pexp_construct of Longident_with_sexp.t loc * expression option
  | Pexp_variant of label * expression option
  | Pexp_record of
      (Longident_with_sexp.t loc * expression) list * expression option
  | Pexp_field of expression * Longident_with_sexp.t loc
  | Pexp_setfield of expression * Longident_with_sexp.t loc * expression
  | Pexp_array of expression list
  | Pexp_ifthenelse of expression * expression * expression option
  | Pexp_sequence of expression * expression
  | Pexp_while of expression * expression
  | Pexp_for of pattern * expression * expression * direction_flag * expression
  | Pexp_constraint of expression * core_type
  | Pexp_coerce of expression * core_type option * core_type
  | Pexp_send of expression * label loc
  | Pexp_new of Longident_with_sexp.t loc
  | Pexp_setinstvar of label loc * expression
  | Pexp_override of (label loc * expression) list
  | Pexp_letmodule of string loc * module_expr * expression
  | Pexp_letexception of extension_constructor * expression
  | Pexp_assert of expression
  | Pexp_lazy of expression
  | Pexp_poly of expression * core_type option
  | Pexp_object of class_structure
  | Pexp_newtype of string loc * expression
  | Pexp_pack of module_expr
  | Pexp_open of open_declaration * expression
  | Pexp_letop of letop
  | Pexp_extension of extension
  | Pexp_unreachable

and case = Parsetree.case = {
  pc_lhs : pattern;
  pc_guard : expression option;
  pc_rhs : expression;
}

and letop = Parsetree.letop = {
  let_ : binding_op;
  ands : binding_op list;
  body : expression;
}

and binding_op = Parsetree.binding_op = {
  pbop_op : string loc;
  pbop_pat : pattern;
  pbop_exp : expression;
  pbop_loc : Location_with_sexp.t;
}

and value_description = Parsetree.value_description = {
  pval_name : string loc;
  pval_type : core_type;
  pval_prim : string list;
  pval_attributes : attributes;
  pval_loc : Location_with_sexp.t;
}

and type_declaration = Parsetree.type_declaration = {
  ptype_name : string loc;
  ptype_params : (core_type * variance) list;
  ptype_cstrs : (core_type * core_type * Location_with_sexp.t) list;
  ptype_kind : type_kind;
  ptype_private : private_flag;
  ptype_manifest : core_type option;
  ptype_attributes : attributes;
  ptype_loc : Location_with_sexp.t;
}

and type_kind = Parsetree.type_kind =
  | Ptype_abstract
  | Ptype_variant of constructor_declaration list
  | Ptype_record of label_declaration list
  | Ptype_open

and label_declaration = Parsetree.label_declaration = {
  pld_name : string loc;
  pld_mutable : mutable_flag;
  pld_type : core_type;
  pld_loc : Location_with_sexp.t;
  pld_attributes : attributes;
}

and constructor_declaration = Parsetree.constructor_declaration = {
  pcd_name : string loc;
  pcd_args : constructor_arguments;
  pcd_res : core_type option;
  pcd_loc : Location_with_sexp.t;
  pcd_attributes : attributes;
}

and constructor_arguments = Parsetree.constructor_arguments =
  | Pcstr_tuple of core_type list
  | Pcstr_record of label_declaration list

and type_extension = Parsetree.type_extension = {
  ptyext_path : Longident_with_sexp.t loc;
  ptyext_params : (core_type * variance) list;
  ptyext_constructors : extension_constructor list;
  ptyext_private : private_flag;
  ptyext_loc : Location_with_sexp.t;
  ptyext_attributes : attributes;
}

and extension_constructor = Parsetree.extension_constructor = {
  pext_name : string loc;
  pext_kind : extension_constructor_kind;
  pext_loc : Location_with_sexp.t;
  pext_attributes : attributes;
}

and type_exception = Parsetree.type_exception = {
  ptyexn_constructor : extension_constructor;
  ptyexn_loc : Location_with_sexp.t;
  ptyexn_attributes : attributes;
}

and extension_constructor_kind = Parsetree.extension_constructor_kind =
  | Pext_decl of constructor_arguments * core_type option
  | Pext_rebind of Longident_with_sexp.t loc

and class_type = Parsetree.class_type = {
  pcty_desc : class_type_desc;
  pcty_loc : Location_with_sexp.t;
  pcty_attributes : attributes;
}

and class_type_desc = Parsetree.class_type_desc =
  | Pcty_constr of Longident_with_sexp.t loc * core_type list
  | Pcty_signature of class_signature
  | Pcty_arrow of arg_label * core_type * class_type
  | Pcty_extension of extension
  | Pcty_open of open_description * class_type

and class_signature = Parsetree.class_signature = {
  pcsig_self : core_type;
  pcsig_fields : class_type_field list;
}

and class_type_field = Parsetree.class_type_field = {
  pctf_desc : class_type_field_desc;
  pctf_loc : Location_with_sexp.t;
  pctf_attributes : attributes;
}

and class_type_field_desc = Parsetree.class_type_field_desc =
  | Pctf_inherit of class_type
  | Pctf_val of (label loc * mutable_flag * virtual_flag * core_type)
  | Pctf_method of (label loc * private_flag * virtual_flag * core_type)
  | Pctf_constraint of (core_type * core_type)
  | Pctf_attribute of attribute
  | Pctf_extension of extension

and 'a class_infos = 'a Parsetree.class_infos = {
  pci_virt : virtual_flag;
  pci_params : (core_type * variance) list;
  pci_name : string loc;
  pci_expr : 'a;
  pci_loc : Location_with_sexp.t;
  pci_attributes : attributes;
}

and class_description = class_type class_infos

and class_type_declaration = class_type class_infos

and class_expr = Parsetree.class_expr = {
  pcl_desc : class_expr_desc;
  pcl_loc : Location_with_sexp.t;
  pcl_attributes : attributes;
}

and class_expr_desc = Parsetree.class_expr_desc =
  | Pcl_constr of Longident_with_sexp.t loc * core_type list
  | Pcl_structure of class_structure
  | Pcl_fun of arg_label * expression option * pattern * class_expr
  | Pcl_apply of class_expr * (arg_label * expression) list
  | Pcl_let of rec_flag * value_binding list * class_expr
  | Pcl_constraint of class_expr * class_type
  | Pcl_extension of extension
  | Pcl_open of open_description * class_expr

and class_structure = Parsetree.class_structure = {
  pcstr_self : pattern;
  pcstr_fields : class_field list;
}

and class_field = Parsetree.class_field = {
  pcf_desc : class_field_desc;
  pcf_loc : Location_with_sexp.t;
  pcf_attributes : attributes;
}

and class_field_desc = Parsetree.class_field_desc =
  | Pcf_inherit of override_flag * class_expr * string loc option
  | Pcf_val of (label loc * mutable_flag * class_field_kind)
  | Pcf_method of (label loc * private_flag * class_field_kind)
  | Pcf_constraint of (core_type * core_type)
  | Pcf_initializer of expression
  | Pcf_attribute of attribute
  | Pcf_extension of extension

and class_field_kind = Parsetree.class_field_kind =
  | Cfk_virtual of core_type
  | Cfk_concrete of override_flag * expression

and class_declaration = class_expr class_infos

and module_type = Parsetree.module_type = {
  pmty_desc : module_type_desc;
  pmty_loc : Location_with_sexp.t;
  pmty_attributes : attributes;
}

and module_type_desc = Parsetree.module_type_desc =
  | Pmty_ident of Longident_with_sexp.t loc
  | Pmty_signature of signature
  | Pmty_functor of string loc * module_type option * module_type
  | Pmty_with of module_type * with_constraint list
  | Pmty_typeof of module_expr
  | Pmty_extension of extension
  | Pmty_alias of Longident_with_sexp.t loc

and signature = signature_item list

and signature_item = Parsetree.signature_item = {
  psig_desc : signature_item_desc;
  psig_loc : Location_with_sexp.t;
}

and signature_item_desc = Parsetree.signature_item_desc =
  | Psig_value of value_description
  | Psig_type of rec_flag * type_declaration list
  | Psig_typesubst of type_declaration list
  | Psig_typext of type_extension
  | Psig_exception of type_exception
  | Psig_module of module_declaration
  | Psig_modsubst of module_substitution
  | Psig_recmodule of module_declaration list
  | Psig_modtype of module_type_declaration
  | Psig_open of open_description
  | Psig_include of include_description
  | Psig_class of class_description list
  | Psig_class_type of class_type_declaration list
  | Psig_attribute of attribute
  | Psig_extension of extension * attributes

and module_declaration = Parsetree.module_declaration = {
  pmd_name : string loc;
  pmd_type : module_type;
  pmd_attributes : attributes;
  pmd_loc : Location_with_sexp.t;
}

and module_substitution = Parsetree.module_substitution = {
  pms_name : string loc;
  pms_manifest : Longident_with_sexp.t loc;
  pms_attributes : attributes;
  pms_loc : Location_with_sexp.t;
}

and module_type_declaration = Parsetree.module_type_declaration = {
  pmtd_name : string loc;
  pmtd_type : module_type option;
  pmtd_attributes : attributes;
  pmtd_loc : Location_with_sexp.t;
}

and 'a open_infos = 'a Parsetree.open_infos = {
  popen_expr : 'a;
  popen_override : override_flag;
  popen_loc : Location_with_sexp.t;
  popen_attributes : attributes;
}

and open_description = Longident_with_sexp.t loc open_infos

and open_declaration = module_expr open_infos

and 'a include_infos = 'a Parsetree.include_infos = {
  pincl_mod : 'a;
  pincl_loc : Location_with_sexp.t;
  pincl_attributes : attributes;
}

and include_description = module_type include_infos

and include_declaration = module_expr include_infos

and with_constraint = Parsetree.with_constraint =
  | Pwith_type of Longident_with_sexp.t loc * type_declaration
  | Pwith_module of Longident_with_sexp.t loc * Longident_with_sexp.t loc
  | Pwith_typesubst of Longident_with_sexp.t loc * type_declaration
  | Pwith_modsubst of Longident_with_sexp.t loc * Longident_with_sexp.t loc

and module_expr = Parsetree.module_expr = {
  pmod_desc : module_expr_desc;
  pmod_loc : Location_with_sexp.t;
  pmod_attributes : attributes;
}

and module_expr_desc = Parsetree.module_expr_desc =
  | Pmod_ident of Longident_with_sexp.t loc
  | Pmod_structure of structure
  | Pmod_functor of string loc * module_type option * module_expr
  | Pmod_apply of module_expr * module_expr
  | Pmod_constraint of module_expr * module_type
  | Pmod_unpack of expression
  | Pmod_extension of extension

and structure = structure_item list

and structure_item = Parsetree.structure_item = {
  pstr_desc : structure_item_desc;
  pstr_loc : Location_with_sexp.t;
}

and structure_item_desc = Parsetree.structure_item_desc =
  | Pstr_eval of expression * attributes
  | Pstr_value of rec_flag * value_binding list
  | Pstr_primitive of value_description
  | Pstr_type of rec_flag * type_declaration list
  | Pstr_typext of type_extension
  | Pstr_exception of type_exception
  | Pstr_module of module_binding
  | Pstr_recmodule of module_binding list
  | Pstr_modtype of module_type_declaration
  | Pstr_open of open_declaration
  | Pstr_class of class_declaration list
  | Pstr_class_type of class_type_declaration list
  | Pstr_include of include_declaration
  | Pstr_attribute of attribute
  | Pstr_extension of extension * attributes

and value_binding = Parsetree.value_binding = {
  pvb_pat : pattern;
  pvb_expr : expression;
  pvb_attributes : attributes;
  pvb_loc : Location_with_sexp.t;
}

and module_binding = Parsetree.module_binding = {
  pmb_name : string loc;
  pmb_expr : module_expr;
  pmb_attributes : attributes;
  pmb_loc : Location_with_sexp.t;
}
[@@deriving sexp]

type toplevel_phrase = Parsetree.toplevel_phrase =
  | Ptop_def of structure
  | Ptop_dir of toplevel_directive

and toplevel_directive = Parsetree.toplevel_directive = {
  pdir_name : string loc;
  pdir_arg : directive_argument option;
  pdir_loc : Location_with_sexp.t;
}

and directive_argument = Parsetree.directive_argument = {
  pdira_desc : directive_argument_desc;
  pdira_loc : Location_with_sexp.t;
}

and directive_argument_desc = Parsetree.directive_argument_desc =
  | Pdir_string of string
  | Pdir_int of string * char option
  | Pdir_ident of Longident_with_sexp.t
  | Pdir_bool of bool
[@@deriving sexp]
