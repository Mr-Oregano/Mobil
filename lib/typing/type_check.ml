open Structs
open Environments
open Printf

let rec type_to_string (typ : ET.typ) =
  match typ with
  | T_Num -> "num"
  | T_Bool -> "bool"
  | T_Unit -> "unit"
  | T_Func { from; to_ } -> sprintf "%s -> %s" (type_to_string from) (type_to_string to_)
  | T_Rec es ->
      let entry_to_string (id, typ) = sprintf "%s: %s" id (type_to_string typ) in
      let es_str = String.concat ", " (List.map entry_to_string es) in
      sprintf "{ %s }" es_str

let rec type_check (prog : Ast.prog) =
  let ctx = Context.empty in
  type_check_expr ctx prog

and type_check_expr (ctx : Context.t) (expr : Ast.expr) =
  match expr with
  | E_Mark _ -> Failure "unimplemented" |> raise
  | E_Marshal _ -> Failure "unimplemented" |> raise
  | E_Unmarshal _ -> Failure "unimplemented" |> raise
  | E_ChanSend _ -> Failure "unimplemented" |> raise
  | E_ChanReceive _ -> Failure "unimplemented" |> raise
  | E_Abs { param; body } ->
      let param' = type_check_param ctx param in
      let ctx' = Context.add_var ctx param' in
      let body' = type_check_expr ctx' body in
      (ET.E_Abs { param = param'; body = body' }, ET.T_Func { from = snd param'; to_ = snd body' })
  | E_If { cond; if_; else_ } ->
      let cond' = type_check_expr ctx cond in
      let () =
        if snd cond' = ET.T_Bool then ()
        else Failure (sprintf "Expected type '%s'" (type_to_string ET.T_Bool)) |> raise
      in
      let if_' = type_check_expr ctx if_ in
      let else_' = type_check_expr ctx else_ in

      (* One branch can result in a subtype of the other branch, 
         the resulting type will be the type of the most 'generic' *)
      ( ET.E_If { cond = cond'; if_ = if_'; else_ = else_' },
        if snd if_' <= snd else_' then snd else_'
        else if snd else_' <= snd if_' then snd if_'
        else Failure "Branches disagree on resulting type" |> raise )
  | E_Let { binder; value; body } ->
      let value' = type_check_expr ctx value in
      let ctx' =
        if Option.is_some binder then Context.add_var ctx (Option.get binder, snd value') else ctx
      in
      let body' = type_check_expr ctx' body in
      (ET.E_Let { binder; value = value'; body = body' }, snd body')
  | E_BinOp (op, e1, e2) ->
      let e1' = type_check_expr ctx e1 in
      let e2' = type_check_expr ctx e2 in
      let () =
        if snd e1' <= ET.T_Num && snd e2' <= ET.T_Num then ()
        else Failure (sprintf "Expected type '%s'" (type_to_string ET.T_Num)) |> raise
      in
      (ET.E_BinOp (op, e1', e2'), ET.T_Num)
  | E_App (callee, arg) -> (
      let callee' = type_check_expr ctx callee in
      match snd callee' with
      | T_Func { from; to_ } ->
          let arg' = type_check_expr ctx arg in
          let () =
            if snd arg' <= from then ()
            else
              Failure
                (sprintf "Argument cannot be applied. Expected type '%s' but got '%s'"
                   (type_to_string from)
                   (type_to_string (snd arg')))
              |> raise
          in
          (ET.E_App (callee', arg'), to_)
      | _ ->
          Failure (sprintf "Argument cannot be applied to type '%s'" (type_to_string (snd callee')))
          |> raise)
  | E_Access (exp, id) -> (
      let exp' = type_check_expr ctx exp in
      match snd exp' with
      | T_Rec es ->
          let entry_opt = List.find_opt (fun (id', _) -> id = id') es in
          let id', typ =
            match entry_opt with
            | Some e -> e
            | None -> Failure (sprintf "'%s' not present in record" id) |> raise
          in
          (ET.E_Access (exp', id), typ)
      | _ -> Failure (sprintf "Cannot access '%s' from non-record" id) |> raise)
  | E_Var v -> (
      match Context.get_var ctx v with
      | None -> Failure (sprintf "Unbound variable: '%s'" v) |> raise
      | Some t -> (ET.E_Var v, t))
  | E_Num n -> (ET.E_Num n, ET.T_Num)
  | E_Bool v -> (ET.E_Bool v, ET.T_Bool)
  | E_Unit -> (ET.E_Unit, T_Unit)
  | E_Rec es ->
      let rec _aux_non_unique_opt xs =
        match xs with
        | [] -> None
        | (id, _) :: xs -> if List.mem_assoc id xs then Some id else _aux_non_unique_opt xs
      in
      let () =
        match _aux_non_unique_opt es with
        | None -> ()
        | Some id -> Failure (sprintf "Duplicate member '%s'" id) |> raise
      in
      let es' = List.map (fun (id, exp) -> (id, type_check_expr ctx exp)) es in
      let tys = List.map (fun (id, exp) -> (id, snd exp)) es' in
      (ET.E_Rec es', T_Rec tys)

and type_check_param (ctx : Context.t) ((name, typ) : Ast.param) = (name, type_check_type ctx typ)

and type_check_type (ctx : Context.t) (typ : Ast.typ_) =
  match typ with
  | T_Num -> ET.T_Num
  | T_Bool -> ET.T_Bool
  | T_Unit -> ET.T_Unit
  | T_Func { from; to_ } ->
      let from' = type_check_type ctx from in
      let to_' = type_check_type ctx to_ in
      ET.T_Func { from = from'; to_ = to_' }
  | T_Rec es ->
      let es' = List.map (fun (id, typ) -> (id, type_check_type ctx typ)) es in
      ET.T_Rec es'

(* Subsumption *)
and ( <= ) (t1 : ET.typ) (t2 : ET.typ) =
  match t1 with
  | T_Func { from; to_ } -> (
      match t2 with
      (* Note, subsumption on functions makes the inputs contravariant and outputs covariant *)
      | T_Func { from = from'; to_ = to_' } -> from' <= from && to_ <= to_'
      | _ -> false)
  | T_Rec es -> (
      (* A record is a subtype of another if it is a superset and its members respect subsumption rules *)
      match t2 with
      | T_Rec es' ->
          List.for_all
            (fun (id', typ') ->
              match List.assoc_opt id' es with None -> false | Some typ -> typ <= typ')
            es'
      | _ -> false)
  | _ -> t1 = t2
