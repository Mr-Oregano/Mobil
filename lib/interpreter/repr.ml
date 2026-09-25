(* TODO: Would be nice to have consistency with the env type and use the same structure everywhere *)

open Typing.Repr

module RT = struct
  type value =
    | V_Num of int
    | V_Bool of bool
    | V_Unit
    | V_Clo of {
        param : ET.param;
        body : ET.expr;
        env : (ET.id * value) list;
      }
    | V_Chan of {
        requirements : ET.id list;
        queue : value list;
      }
    | V_Marsh of {
        env : (ET.id * value) list;
        body : ET.expr;
      }
    | V_Rec of (ET.id * value) list
end
