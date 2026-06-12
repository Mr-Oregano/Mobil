(* TODO: Parameterize these structs with the coeffect type *)
module Ast = struct
  type prog = expr

  and typ =
    | T_Num
    | T_Bool
    | T_Unit
    | T_Func of {
        coeff : (id * typ) list;
        from : typ;
        to_ : typ;
      }
    | T_Chan of {
        coeff : (id * typ) list;
        typ : typ;
      }
    | T_Marsh of {
        coeff : (id * typ) list;
        typ : typ;
      }
    | T_Rec of (id * typ) list

  and mobility =
    | M_Mobile
    | M_iMobile

  and expr =
    | E_Marshal of {
        rebinds : id list;
        body : expr;
      }
    | E_Unmarshal of {
        rebinds : (id * expr) list;
        body : expr;
      }
    | E_Abs of {
        param : param;
        body : expr;
      }
    | E_If of {
        cond : expr;
        if_ : expr;
        else_ : expr;
      }
    | E_Let of {
        binder : id option;
        mobility : mobility;
        value : expr;
        body : expr;
      }
    | E_ChanSend of {
        chan : expr;
        package : expr;
      }
    | E_ChanReceive of expr
    | E_BinOp of (op * expr * expr)
    | E_App of (expr * expr)
    | E_Access of (expr * id)
    | E_Var of id
    | E_Num of int
    | E_Bool of bool
    | E_Rec of (id * expr) list
    | E_Unit

  and param = id * typ

  and op =
    | O_Add
    | O_Sub

  and id = string
end

module ET = struct
  type prog = expr

  and typ =
    | T_Num
    | T_Bool
    | T_Unit
    | T_Func of {
        coeff : (id * typ) list;
        from : typ;
        to_ : typ;
      }
    | T_Chan of {
        coeff : (id * typ) list;
        typ : typ;
      }
    | T_Marsh of {
        coeff : (id * typ) list;
        typ : typ;
      }
    | T_Rec of (id * typ) list

  and mobility = Ast.mobility
  and expr = _expr_desc * typ

  and _expr_desc =
    | E_Marshal of {
        rebinds : id list;
        body : expr;
      }
    | E_Unmarshal of {
        rebinds : (id * expr) list;
        body : expr;
      }
    | E_ChanSend of {
        chan : expr;
        package : expr;
      }
    | E_ChanReceive of expr
    | E_Abs of {
        param : param;
        body : expr;
      }
    | E_If of {
        cond : expr;
        if_ : expr;
        else_ : expr;
      }
    | E_Let of {
        binder : id option;
        mobility : mobility;
        value : expr;
        body : expr;
      }
    | E_BinOp of (op * expr * expr)
    | E_App of (expr * expr)
    | E_Access of (expr * id)
    | E_Var of id
    | E_Num of int
    | E_Bool of bool
    | E_Rec of (id * expr) list
    | E_Unit

  and param = id * typ
  and op = Ast.op
  and id = string
end
