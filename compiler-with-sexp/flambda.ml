open! Core

type call_kind = Compiler_without_sexp.Flambda.call_kind =
  | Indirect
  | Direct of Closure_id.t
[@@deriving sexp_of]

type const = Compiler_without_sexp.Flambda.const =
  | Int of int
  | Char of char
  | Const_pointer of int
[@@deriving sexp_of]

type apply = Compiler_without_sexp.Flambda.apply =
  { func : Variable.t
  ; args : Variable.t list
  ; kind : call_kind
  ; dbg : Debuginfo.t
  ; inline : Lambda.inline_attribute
  ; specialise : Lambda.specialise_attribute
  }
[@@deriving sexp_of]

type assign = Compiler_without_sexp.Flambda.assign =
  { being_assigned : Mutable_variable.t
  ; new_value : Variable.t
  }
[@@deriving sexp_of]

type send = Compiler_without_sexp.Flambda.send =
  { kind : Lambda.meth_kind
  ; meth : Variable.t
  ; obj : Variable.t
  ; args : Variable.t list
  ; dbg : Debuginfo.t
  }
[@@deriving sexp_of]

type project_closure = Projection.project_closure [@@deriving sexp_of]

type move_within_set_of_closures = Projection.move_within_set_of_closures
[@@deriving sexp_of]

type project_var = Projection.project_var [@@deriving sexp_of]

type specialised_to = Compiler_without_sexp.Flambda.specialised_to =
  { var : Variable.t
  ; projection : Projection.t option
  }
[@@deriving sexp_of]

type t = Compiler_without_sexp.Flambda.t =
  | Var of Variable.t
  | Let of let_expr
  | Let_mutable of let_mutable
  | Let_rec of (Variable.t * named) list * t
  | Apply of apply
  | Send of send
  | Assign of assign
  | If_then_else of Variable.t * t * t
  | Switch of Variable.t * switch
  | String_switch of Variable.t * (string * t) list * t option
  | Static_raise of Static_exception.t * Variable.t list
  | Static_catch of Static_exception.t * Variable.t list * t * t
  | Try_with of t * Variable.t * t
  | While of t * t
  | For of for_loop
  | Proved_unreachable

and named = Compiler_without_sexp.Flambda.named =
  | Symbol of Symbol.t
  | Const of const
  | Allocated_const of Allocated_const.t
  | Read_mutable of Mutable_variable.t
  | Read_symbol_field of Symbol.t * int
  | Set_of_closures of set_of_closures
  | Project_closure of project_closure
  | Move_within_set_of_closures of move_within_set_of_closures
  | Project_var of project_var
  | Prim of Lambda.primitive * Variable.t list * Debuginfo.t
  | Expr of t

and let_expr = Compiler_without_sexp.Flambda.let_expr = private
  { var : Variable.t
  ; defining_expr : named
  ; body : t
  ; free_vars_of_defining_expr : Variable.Set.t
  ; free_vars_of_body : Variable.Set.t
  }

and let_mutable = Compiler_without_sexp.Flambda.let_mutable =
  { var : Mutable_variable.t
  ; initial_value : Variable.t
  ; contents_kind : Lambda.value_kind
  ; body : t
  }

and set_of_closures = Compiler_without_sexp.Flambda.set_of_closures = private
  { function_decls : function_declarations
  ; free_vars : specialised_to Variable.Map.t
  ; specialised_args : specialised_to Variable.Map.t
  ; direct_call_surrogates : Variable.t Variable.Map.t
  }

and function_declarations = Compiler_without_sexp.Flambda.function_declarations = private
  { is_classic_mode : bool
  ; set_of_closures_id : Set_of_closures_id.t
  ; set_of_closures_origin : Set_of_closures_origin.t
  ; funs : function_declaration Variable.Map.t
  }

and function_declaration = Compiler_without_sexp.Flambda.function_declaration = private
  { closure_origin : Closure_origin.t
  ; params : Parameter.t list
  ; body : t
  ; free_variables : Variable.Set.t
  ; free_symbols : Symbol.Set.t
  ; stub : bool
  ; dbg : Debuginfo.t
  ; inline : Lambda.inline_attribute
  ; specialise : Lambda.specialise_attribute
  ; is_a_functor : bool
  }

and switch = Compiler_without_sexp.Flambda.switch =
  { numconsts : Numbers.Int.Set.t
  ; consts : (int * t) list
  ; numblocks : Numbers.Int.Set.t
  ; blocks : (int * t) list
  ; failaction : t option
  }

and for_loop = Compiler_without_sexp.Flambda.for_loop =
  { bound_var : Variable.t
  ; from_value : Variable.t
  ; to_value : Variable.t
  ; direction : Asttypes.direction_flag
  ; body : t
  }

and constant_defining_value = Compiler_without_sexp.Flambda.constant_defining_value =
  | Allocated_const of Allocated_const.t
  | Block of Tag.t * constant_defining_value_block_field list
  | Set_of_closures of set_of_closures
  | Project_closure of Symbol.t * Closure_id.t

and constant_defining_value_block_field =
      Compiler_without_sexp.Flambda.constant_defining_value_block_field =
  | Symbol of Symbol.t
  | Const of const
[@@deriving sexp_of]

type expr = t [@@deriving sexp_of]

type program_body = Compiler_without_sexp.Flambda.program_body =
  | Let_symbol of Symbol.t * constant_defining_value * program_body
  | Let_rec_symbol of (Symbol.t * constant_defining_value) list * program_body
  | Initialize_symbol of Symbol.t * Tag.t * t list * program_body
  | Effect of t * program_body
  | End of Symbol.t
[@@deriving sexp_of]

type program = Compiler_without_sexp.Flambda.program =
  { imported_symbols : Symbol.Set.t
  ; program_body : program_body
  }
[@@deriving sexp_of]

module With_free_variables = struct
  type 'a t = ('a Compiler_without_sexp.Flambda.With_free_variables.t[@sexp.opaque])
  [@@deriving sexp_of]

  include (
    Compiler_without_sexp.Flambda.With_free_variables :
      module type of struct
        include Compiler_without_sexp.Flambda.With_free_variables
      end
      with type 'a t := 'a Compiler_without_sexp.Flambda.With_free_variables.t)
end

type maybe_named = Compiler_without_sexp.Flambda.maybe_named =
  | Is_expr of t
  | Is_named of named
[@@deriving sexp_of]

include (
  Compiler_without_sexp.Flambda :
    module type of struct
      include Compiler_without_sexp.Flambda
    end
    with type apply := Compiler_without_sexp.Flambda.apply
     and type assign := Compiler_without_sexp.Flambda.assign
     and type call_kind := Compiler_without_sexp.Flambda.call_kind
     and type const := Compiler_without_sexp.Flambda.const
     and type constant_defining_value :=
          Compiler_without_sexp.Flambda.constant_defining_value
     and type constant_defining_value_block_field :=
          Compiler_without_sexp.Flambda.constant_defining_value_block_field
     and type expr := Compiler_without_sexp.Flambda.expr
     and type for_loop := Compiler_without_sexp.Flambda.for_loop
     and type function_declaration := Compiler_without_sexp.Flambda.function_declaration
     and type function_declarations := Compiler_without_sexp.Flambda.function_declarations
     and type let_expr := Compiler_without_sexp.Flambda.let_expr
     and type let_mutable := Compiler_without_sexp.Flambda.let_mutable
     and type maybe_named := Compiler_without_sexp.Flambda.maybe_named
     and type move_within_set_of_closures :=
          Compiler_without_sexp.Flambda.move_within_set_of_closures
     and type named := Compiler_without_sexp.Flambda.named
     and type program := Compiler_without_sexp.Flambda.program
     and type program_body := Compiler_without_sexp.Flambda.program_body
     and type project_closure := Compiler_without_sexp.Flambda.project_closure
     and type project_var := Compiler_without_sexp.Flambda.project_var
     and type send := Compiler_without_sexp.Flambda.send
     and type set_of_closures := Compiler_without_sexp.Flambda.set_of_closures
     and type specialised_to := Compiler_without_sexp.Flambda.specialised_to
     and type switch := Compiler_without_sexp.Flambda.switch
     and type t := Compiler_without_sexp.Flambda.t
     and module With_free_variables := Compiler_without_sexp.Flambda.With_free_variables)

