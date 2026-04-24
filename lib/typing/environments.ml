open Structs.ET

module Context = struct
  module IDMap = Map.Make (String)

  type t = { vars : typ IDMap.t }

  let empty = { vars = IDMap.empty }

  (* TODO: Allow identifier shadowing *)
  let _assert_not_contains map id =
    if not (map |> IDMap.mem id) then ()
    else raise (Failure (Printf.sprintf "Already bound identifier '%s'" id))

  let add_var ctx (id, typ) =
    _assert_not_contains ctx.vars id;
    { vars = ctx.vars |> IDMap.add id typ }

  let remove_var ctx name = { vars = ctx.vars |> IDMap.remove name }
  let get_var ctx name = ctx.vars |> IDMap.find_opt name
  let get_vars ctx = IDMap.to_seq ctx.vars
end
