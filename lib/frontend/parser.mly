%{
  open Typing.Structs.Ast
%}

// Tokens
%token <string> IDENT
%token <int> NUM
%token UNIT WILDCARD 

%token KW_IF 
%token KW_THEN 
%token KW_ELSE 
%token KW_TRUE
%token KW_FALSE
%token KW_LET 
%token KW_IN 
%token KW_MARSHAL 
%token KW_UNMARSHAL 
%token KW_MARK
%token KW_NUM
%token KW_BOOL
%token KW_UNIT

%token LPAREN LBRACE
%token RPAREN RBRACE
%token EXCLAMATION
%token QUESTION
%token ARROW
%token PERIOD COMMA
%token BACKSLASH
%token COLON 
%token PLUS MINUS
%token EQUALS
%token EOF

%type <id option> binder

%start <expr> prog

%%

prog:
  | e = expr EOF { e }

// ========== Types ========== 
typ_:
  | t = base_typ { t }
  | LBRACE ts = separated_list(COMMA, record_type_entry) RBRACE { T_Rec ts }

//   Right-associative function types: num -> num -> unit === num -> (num -> unit)
  | t1 = base_typ ARROW t2 = typ_ { T_Func { from = t1; to_ = t2 } }

record_type_entry:
  | x = IDENT COLON t = typ_ { (x, t) }

base_typ:
  | KW_NUM { T_Num }
  | KW_BOOL { T_Bool }
  | KW_UNIT { T_Unit }
  | LPAREN t = typ_ RPAREN { t }

// Misc
binder:
  | x = IDENT { Some x }
  | WILDCARD { None }

// ========== Expressions ========== 
expr:
  | e = marsh_expr { e }

marsh_expr:
  | e = abs_expr { e }
  | KW_MARK m = IDENT KW_IN e = marsh_expr { E_Mark (m, e) }
  | KW_MARSHAL m = IDENT e = marsh_expr { E_Marshal (m, e) }
  | KW_UNMARSHAL m = IDENT e = marsh_expr { E_Unmarshal (m, e) }

abs_expr:
  | e = cmpnd_expr { e }

//   Allow nested lambda abstraction: \(x: num) -> \(y: num) -> x + y === \(x: num) -> (\(y: num) -> x + y)
  | BACKSLASH LPAREN param_id = IDENT COLON param_typ = typ_ RPAREN ARROW e = abs_expr 
        { E_Abs { param = (param_id, param_typ); body = e } }

cmpnd_expr:
  | e = basic_expr { e }
  | KW_LET b = binder EQUALS v = expr KW_IN e = expr
        { E_Let { binder = b; value = v; body = e } }

  | KW_IF c = expr KW_THEN e1 = expr KW_ELSE e2 = expr
        { E_If { cond = c; if_ = e1; else_ = e2 } }

basic_expr:
  | e = app_expr { e }

//   Left-associative binary operations: a + b + c === (a + b) + c
  | e1 = basic_expr PLUS e2 = app_expr { E_BinOp (O_Add, e1, e2) }
  | e1 = basic_expr MINUS e2 = app_expr { E_BinOp (O_Sub, e1, e2) }
  | e = basic_expr QUESTION { E_ChanReceive e }

//   Left-associative channel send: a ! b ! c === (a ! b) ! c
  | e1 = basic_expr EXCLAMATION e2 = app_expr { E_ChanSend { chan = e1; package = e2 } }

app_expr:
  | e = simple { e }

//   Left-associative application: foo a b c === ((foo a) b) c
  | e1 = app_expr e2 = simple { E_App (e1, e2) }

simple:
  | v = IDENT { E_Var v }
  | n = NUM { E_Num n }
  | e = simple PERIOD id = IDENT { E_Access (e, id) }
  | LBRACE es = separated_list(COMMA, record_value_entry) RBRACE { E_Rec es }
  | KW_TRUE { E_Bool true }
  | KW_FALSE { E_Bool false }
  | UNIT { E_Unit }
  | LPAREN e = expr RPAREN { e }

record_value_entry:
  | x = IDENT COLON e = expr { (x, e) }