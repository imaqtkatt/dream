%{
  open Ast
%}

%token <string> IDENT
%token <int> INTEGER
%token <bool> BOOL
%token UNIT

%token <string> TYPEVAR

%token LPARENS RPARENS
%token LBRACE RBRACE
%token LBRACKET RBRACKET

%token IF THEN ELSE
%token LET IN
%token TYPE STRUCT

%token ARROW
%token LAMBDA
%token COMMA COLON DOT

%token PLUS MINUS
%token ASTERISK SLASH

%token EQ NEQ
%token GT GTE
%token LT LTE

%token EOF

%right ARROW

%start <expr option> prog
%start <program option> prog2

%%

(* types *)

let sub_typ :=
  | arr_typ
  | prim_typ
  | LPARENS; t = sub_typ; RPARENS; { t }

let arr_typ :=
  | l = sub_typ; ARROW; r = sub_typ; { T_Arrow (l, r) }

let prim_typ :=
  | typ_var
  | i = IDENT; { T_Generic (i, []) }
  | i = IDENT; LBRACKET; ts = separated_nonempty_list(COMMA, sub_typ); RBRACKET;
    { T_Generic (i, ts) }
let typ_var :=
  | tv = TYPEVAR; { T_Var tv }

(* END types *)

(* program *)

let prog :=
  | e = expr; EOF; { Some e }
  | EOF; { None }

let prog2 :=
  | decls = decls; EOF; { Some { decls = decls } }
  | EOF; { None }

let decls :=
  | decl = toplevel; { [decl] }
  | decls = decls; next = toplevel; { decls @ [next] }

let toplevel :=
  | const_decl
  | fn_decl
  | typ_decl
  | struct_decl

let struct_decl :=
  | STRUCT; id = IDENT; LBRACE; fields = separated_nonempty_list(COMMA, struct_field); RBRACE;
    { TL_Struct (id, fields) }
let struct_field ==
  | name = IDENT; COLON; t = sub_typ; { { field_name = name; field_typ = t; } }

let const_decl :=
  | id = IDENT; t = fn_typ?; body = fn_body; { TL_Const (id, t, body) }

let fn_decl :=
  | id = IDENT; args = nonempty_list(pat); t = fn_typ?; body = fn_body;
    { TL_FnDecl (id, args, t, body) }
let pat :=
  | UNIT; { Unit }
  | id = IDENT; { Name id }
  | annot = annot; { Annot annot }
let fn_body ==
  | EQ; LBRACE; body = expr; RBRACE; { body }
  | EQ; body = expr; { body }
let fn_typ ==
  | COLON; t = sub_typ; { t }

let typ_decl :=
  | TYPE; id = IDENT; EQ; typ = sub_typ; { TL_TyAlias (id, typ) }

let expr :=
  | op2
  | lambda
  | if_expr
  | let_expr
  | struct_init

let if_expr :=
  | IF; cond = expr; THEN; e1 = expr; ELSE; e2 = expr;
    { E_If (cond, e1, e2) }

let annot :=
  | LPARENS; i = IDENT; COLON; t = sub_typ; RPARENS;
    { { var = i; typ = t; } (* Ast.annot *) }

let let_body :=
  | a = annot; EQ; v = expr; IN; next = expr; { E_TypLet (a, v, next) }
  | bind = IDENT; EQ; v = expr; IN; next = expr; { E_Let (bind, v, next) }

let let_expr :=
  | LET; lb = let_body; { lb }

let lambda :=
  | LAMBDA; x = IDENT; body = expr; { E_Abs (x, body) }
  | LAMBDA; a = annot; DOT; body = expr; { E_TypAbs (a, body) }

%inline op:
  | PLUS; { Op_Add }
  | MINUS; { Op_Sub }
  | ASTERISK; { Op_Mul }
  | SLASH; { Op_Div }
  | EQ; { Op_Eq }
  | NEQ; { Op_Neq }
  | GT; { Op_Gt }
  | GTE; { Op_Gte }
  | LT; { Op_Lt }
  | LTE; { Op_Lte }

let struct_init :=
  | id = IDENT; LBRACE; fields = separated_nonempty_list(COMMA, init_field); RBRACE;
    { E_Struct (id, fields) }
let init_field ==
  | id = IDENT; EQ; body = expr; { { field_ref = id; field_val = body; } }

let op2 :=
  | call
  | LPARENS; op = op; l = expr; r = call_args; RPARENS; { op_foldl op l r }

let call_args :=
  | a = expr; { [a] }
  | a = call_args; next = expr; { a @ [next] }

let call :=
  | sub_primary
  | LPARENS; callee = call; args = call_args; RPARENS;
    { app_foldl callee args }

let sub_primary :=
  | primary
  | LPARENS; p = expr; RPARENS; { p }

let primary :=
  | UNIT; { E_Unit }
  | id = IDENT; { E_Var id }
  | i = INTEGER; { E_Int i }
  | b = BOOL; { E_Bool b }

(* END program *)
