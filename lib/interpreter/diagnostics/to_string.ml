open Repr.RT
open Printf

let rec value_to_string v =
  (* TODO: Ideally restrict the total width of the string to a fixed amount of characters *)
  let entry_to_string (id, v) = sprintf "%s: %s" id (value_to_string v) in
  let entries_to_string format sep es =
    if List.is_empty es then String.empty
    else sprintf format (String.concat sep (List.map entry_to_string es))
  in
  let env_to_str env = entries_to_string "[ %s ]" ", " env in
  match v with
  | V_Num n -> Int.to_string n
  | V_Bool b -> Bool.to_string b
  | V_Unit -> "()"
  | V_Clo { param; body; env } ->
      let par_str = fst param |> Option.value ~default:"" in
      (* TODO: Add expression node print? *)
      sprintf "closure:\n\tenv: %s\n\t\\(%s) -> ..." (env_to_str env) par_str
  | V_Chan { requirements; queue } ->
      let req_str = String.concat ", " requirements in
      let que_str = String.concat ", " (List.map value_to_string queue) in
      sprintf "channel:\n\treq: [ %s ]\n\tqueue: [ %s ]" req_str que_str
  | V_Marsh { env; body } ->
      (* TODO: Add expression node print? *)
      sprintf "marsh:\n\tenv: %s" (env_to_str env)
  | V_Rec vs -> entries_to_string "{ %s }" ", " vs
