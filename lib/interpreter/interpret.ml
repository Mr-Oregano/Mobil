open Rt
open Typing.Et
open Env
open Printf

let interpret (prog : prog) : value =
  let env = Env.empty in
  let rec eval_expr (env : Env.t) (exp : expr) : value =
    match fst exp with
    | E_Marshal body -> (
        let typ = snd exp in
        match typ with
        | T_Marsh { coeff; typ } ->
            let env' = unbind (Env.to_seq env) (List.split coeff |> fst) in
            V_Marsh { env = List.of_seq env'; body }
        | _ -> failwith "Failed to marshal, typing inconsistency in ET")
    | E_Unmarshal { rebinds; body } -> (
        let body' = eval_expr env body in
        match body' with
        | V_Marsh { env = clo; body = marsh } ->
            let clo = Env.of_seq (List.to_seq clo) in
            (* Shadow with rebindings *)
            let env' =
              List.fold_left
                (fun env' (id, exp) -> Env.add_var env' (id, eval_expr env exp))
                clo rebinds
            in
            eval_expr env' marsh
        | _ -> failwith "Failed to unmarshal non-marshaled type")
    | E_ChanSend { chan; package } -> failwith "Not implemented yet"
    | E_ChanReceive chan -> failwith "Not implemented yet"
    | E_NewChan { requirements; typ } -> failwith "Not implemented yet"
    | E_Abs { param; body } -> V_Clo { param; body; env = List.of_seq (Env.to_seq env) }
    | E_If { cond; if_; else_ } -> (
        let cond' = eval_expr env cond in
        match cond' with
        | V_Bool v -> if v then eval_expr env if_ else eval_expr env else_
        | _ -> failwith "Failed to extact boolean value")
    | E_Let { binder; value; body } ->
        let v = eval_expr env value in
        let env' = match binder with None -> env | Some id -> Env.add_var env (id, v) in
        eval_expr env' body
    | E_BinOp (op, e1, e2) -> (
        let v1 = eval_expr env e1 in
        let v2 = eval_expr env e2 in
        match (op, v1, v2) with
        | O_Add, V_Num n1, V_Num n2 -> V_Num (n1 + n2)
        | O_Sub, V_Num n1, V_Num n2 -> V_Num (n1 - n2)
        | _ -> failwith "Failed to compute binary operation")
    | E_App (callee, arg) -> (
        let callee' = eval_expr env callee in
        match callee' with
        | V_Clo { param; body; env = clos } ->
            let arg' = eval_expr env arg in
            (* Shadow with the closure bindings *)
            let env' =
              List.fold_left (fun env' (id, value) -> Env.add_var env' (id, value)) env clos
            in
            (* Shadow with the parameter *)
            let env'' =
              match fst param with None -> env' | Some id -> Env.add_var env' (id, arg')
            in
            eval_expr env'' body
        | _ -> failwith "Failed to apply to non-function")
    | E_Access (rec_, id) -> (
        let rec_' = eval_expr env rec_ in
        match rec_' with
        | V_Rec vs -> (
            let e_opt = List.assoc_opt id vs in
            match e_opt with
            | Some v -> v
            | None -> failwith (sprintf "Unable to access '%s' from record, not found" id))
        | _ -> failwith "Failed to access entry from non-record")
    | E_Var id -> (
        let v_opt = Env.get_var env id in
        match v_opt with Some v -> v | None -> failwith (sprintf "Variable '%s' not found" id))
    | E_Num n -> V_Num n
    | E_Bool b -> V_Bool b
    | E_Unit -> V_Unit
    | E_Rec es ->
        let vs = List.map (fun (id, expr) -> (id, eval_expr env expr)) es in
        V_Rec vs
  and unbind env ids =
    let rec _aux v =
      match v with
      | V_Rec vs -> V_Rec (List.map (fun (id, v) -> (id, _aux v)) vs)
      | V_Clo { param; body; env } ->
          V_Clo { param; body; env = unbind (List.to_seq env) ids |> List.of_seq }
      | _ -> v
    in
    (* Filter out the given ids *)
    let env' = Seq.filter (fun (id, _) -> not (List.exists (( = ) id) ids)) env in
    (* Call on every value in environment *)
    Seq.map (fun (id, v) -> (id, _aux v)) env'
  in

  eval_expr env prog
