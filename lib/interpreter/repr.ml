module RT = struct
  type value =
    | V_Num of int
    | V_Bool of bool
    | V_Unit
    | V_Func of {
        param : id;
        body : expr;
        env : (id * value) list;
      }
    | V_Chan of {
        coeff : id list;
        queue : value list;
      }
    | V_Marsh of {
        coeff : id list;
        data : value;
      }
    | V_Rec of (id * value) list

  and expr = expr'

  and expr' =
    | E_Marshal of expr
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
        value : expr;
        body : expr;
      }
    | E_ChanSend of {
        chan : expr;
        package : expr;
      }
    | E_ChanReceive of expr
    | E_NewChan of { requirements : id list }
    | E_BinOp of (op * expr * expr)
    | E_App of (expr * expr)
    | E_Access of (expr * id)
    | E_Var of id
    | E_Num of int
    | E_Bool of bool
    | E_Rec of (id * expr) list
    | E_Unit

  and op =
    | O_Add
    | O_Sub

  and param = id option
  and id = string
end
