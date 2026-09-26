(* TODO: Would be nice to have consistency with the env type and use the same structure everywhere *)

open Typing.Et

type value =
  | V_Num of int
  | V_Bool of bool
  | V_Unit
  | V_Clo of {
      param : param;
      body : expr;
      env : (id * value) list;
    }
  | V_Chan of {
      requirements : id list;
      queue : value list;
    }
  | V_Marsh of {
      env : (id * value) list;
      body : expr;
    }
  | V_Rec of (id * value) list
