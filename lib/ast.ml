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
  | E_Var of string
  | E_Int of int
  | E_Str of string
  | E_Bool of bool
  | E_Binary of op * expr * expr
  | E_If of expr * expr * expr
  | E_App of expr * expr
  | E_Abs of string * expr
  | E_Let of string * expr * expr
  | E_TypAbs of annot * expr
  | E_TypLet of annot * expr * expr
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
  | TL_FnDecl of string * fn_arg list * typ option * expr
  | TL_TyAlias of string * typ
  | TL_Struct of string * struct_field list
[@@deriving show]

and fn_arg =
  | Annot of annot
  | Name of string
[@@deriving show]

and struct_field = {
  field_name : string;
  field_typ : typ;
}

and program = { decls : toplevel list } [@@deriving show]

let app_foldl = List.fold_left (fun fn arg -> E_App (fn, arg))
