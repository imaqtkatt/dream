%{
  open Ast
%}

%token <string> IDENT
%token <int> INTEGER
%token <bool> BOOL

%token <string> TYPEVAR

%token LPARENS RPARENS
%token LBRACE RBRACE
%token LBRACKET RBRACKET

%token IF THEN ELSE
%token LET IN

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
%start <typ option> typ

%%

(* types *)

let typ :=
  | t = sub_typ; EOF; { Some t }
  | EOF; { None }

let sub_typ :=
  | arr_typ
  | prim_typ
  | LPARENS; t = sub_typ; RPARENS; { t }

let arr_typ :=
  | l = sub_typ; ARROW; r = sub_typ; { T_Arrow (l, r) }

let prim_typ :=
  | tv = TYPEVAR; { T_Var tv }
  | i = IDENT; { T_Generic (i, []) }
  | i = IDENT; LBRACKET; ts = separated_nonempty_list(COMMA, sub_typ); RBRACKET;
    { T_Generic (i, ts) }

(* END types *)

(* program *)

let prog :=
  | e = expr; EOF; { Some e }
  | EOF; { None }

let expr :=
  | op2
  | lambda
  | if_expr
  | let_expr

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

let op2 :=
  | call
  | LPARENS; op = op; l = expr; r = expr; RPARENS; { E_Binary (op, l, r) }

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
  | id = IDENT; { E_Var id }
  | i = INTEGER; { E_Int i }
  | b = BOOL; { E_Bool b }

(* END program *)
