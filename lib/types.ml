module Ast = struct
  type prog = expr

  and typ_ =
    | T_Num
    | T_Bool
    | T_Unit
    | T_Func of {
        from : typ_;
        to_ : typ_;
      }
    | T_Rec of (id * typ_) list

  and op =
    | O_Add
    | O_Sub

  and id = string
  and param = id * typ_

  and expr =
    | E_Mark of (id * expr)
    | E_Marshal of (id * expr)
    | E_Unmarshal of (id * expr)
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
end
