type annot = {
  var : string;
  typ : typ;
}
[@@deriving show]

and typ =
  | T_Var of string
  | T_Generic of string * typ list
  | T_Arrow of typ * typ
[@@deriving show]

and expr =
  | E_Unit
  | E_Var of string
  | E_Int of int
  | E_Str of string
  | E_Bool of bool
  | E_Binary of op * expr * expr
  | E_If of expr * expr * expr
  | E_App of expr * expr
  | E_Abs of pat * expr
  | E_Let of pat * expr * expr
  | E_Struct of string * field_init list
[@@deriving show]

and op =
  | Op_Add
  | Op_Sub
  | Op_Mul
  | Op_Div
  | Op_Eq
  | Op_Neq
  | Op_Gt
  | Op_Gte
  | Op_Lt
  | Op_Lte
[@@deriving show]

and toplevel =
  | TL_Const of string * typ option * expr
  | TL_FnDecl of string * pat list * typ option * expr
  | TL_TyAlias of string * typ
  | TL_Struct of string * struct_field list
[@@deriving show]

and pat =
  | Unit
  | Annot of annot
  | Name of string
[@@deriving show]

and struct_field = {
  field_name : string;
  field_typ : typ;
}

and field_init = {
  field_ref : string;
  field_val : expr;
}

and program = { decls : toplevel list } [@@deriving show]

let app_foldl = List.fold_left (fun fn arg -> E_App (fn, arg))
let op_foldl op = List.fold_left (fun l r -> E_Binary (op, l, r))
