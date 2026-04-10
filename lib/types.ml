module Ast = struct
  type id = string

  and expr =
    | E_Abs of {
        param : param;
        body : expr;
      }
    | E_App of (expr * expr)
    | E_Add of (expr * expr)
    | E_If of {
        cond : expr;
        if_ : expr;
        else_ : expr;
      }
    | E_Let of {
        binder : id;
        value : expr;
        body : expr;
      }
    | E_ChanSend of {
        chan : expr;
        package : expr;
      }
    | E_ChanReceive of expr
    | E_Mark of (id * expr)
    | E_Marshal of (id * expr)
    | E_Unmarshal of (id * expr)
    | E_Var of id
    | E_Num of int
    | E_Bool of bool
    | E_Unit

  and typ_ =
    | T_Num
    | T_Unit
    | T_Func of {
        from : typ_;
        to_ : typ_;
      }

  and param = id * typ_
  and prog = expr
end
