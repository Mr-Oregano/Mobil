%{
  open Types.Ast
%}

/* Tokens */
%token <string> IDENT
%token <int> NUM
%token UNIT

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
%token KW_UNIT
%token LPAREN
%token RPAREN
%token EXCLAMATION
%token QUESTION
%token ARROW
%token PERIOD
%token BACKSLASH
%token COLON 
%token PLUS
%token EQUALS
%token EOF

%start <expr> prog

%%

prog:
  | e = expr EOF { e }

// ========== Types ========== 
typ_:
  | t = base_typ { t }

//   Right-associative function types: num -> num -> unit === num -> (num -> unit)
  | t1 = base_typ; ARROW; t2 = typ_; { T_Func { from = t1; to_ = t2 } }

base_typ:
  | KW_NUM { T_Num }
  | KW_UNIT { T_Unit }
  | LPAREN; t = typ_; RPAREN { t }

// ========== Expressions ========== 
expr:
  | e = marsh_expr { e }

marsh_expr:
  | e = abs_expr { e }
//   Disallow nested marshals and marks? TODO: Does this even make any sense?
  | KW_MARK; m = IDENT; KW_IN; e = abs_expr { E_Mark (m, e) }
  | KW_MARSHAL; m = IDENT; e = abs_expr { E_Marshal (m, e) }
  | KW_UNMARSHAL; m = IDENT; e = abs_expr { E_Unmarshal (m, e) }

abs_expr:
  | e = cmpnd_expr { e }

//   Allow nested lambda abstraction: \(x: num). \(y: num). x + y === \(x: num). (\(y: num). x + y)
  | BACKSLASH; LPAREN; param_id = IDENT; COLON; param_typ = typ_; RPAREN; PERIOD; e = abs_expr 
        { E_Abs { param = (param_id, param_typ); body = e } }

cmpnd_expr:
  | e = basic_expr { e }
  | KW_LET; id = IDENT; EQUALS; v = cmpnd_expr; KW_IN; e = cmpnd_expr 
        { E_Let { binder = id; value = v; body = e } }

  | KW_IF; c = cmpnd_expr; KW_THEN; e1 = cmpnd_expr; KW_ELSE; e2 = cmpnd_expr 
        { E_If { cond = c; if_ = e1; else_ = e2 } }

basic_expr:
  | e = app_expr { e }

//   Left-associative addition: a + b + c === (a + b) + c
  | e1 = basic_expr; PLUS; e2 = app_expr { E_Add (e1, e2) }

//   Left-associative channel send: a ! b ! c === (a ! b) ! c
  | e1 = basic_expr; EXCLAMATION; e2 = app_expr { E_ChanSend { chan = e1; package = e2 } }

//   Allow nested channel receive? : a ? ? ? === ((a ?) ?) ? TODO: Does this even make any sense?
  | e = basic_expr; QUESTION { E_ChanReceive e }

app_expr:
  | e = simple { e }

//   Left-associative application: foo a b c === ((foo a) b) c
  | e1 = app_expr; e2 = simple { E_App (e1, e2) }

simple:
  | v = IDENT { E_Var v }
  | n = NUM { E_Num n }
  | KW_TRUE { E_Bool true }
  | KW_FALSE { E_Bool false }
  | UNIT { E_Unit }
  | LPAREN; e = expr; RPAREN { e }