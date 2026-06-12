open Structs
open Environments
open Printf
open List_ext

let assert_msg cond msg = if not cond then failwith msg

let rec assert_no_duplicates xs msg_func =
  match xs with
  | [] -> ()
  | (id, _) :: xs ->
      if List.mem_assoc id xs then failwith (msg_func id) else assert_no_duplicates xs msg_func

let ( @ ) = Coeffect.( @ )

let rec type_to_string (typ : ET.typ) =
  (* TODO: Ideally restrict the total width of the string to a fixed amount of characters *)
  let entry_to_string (id, typ) = sprintf "%s: %s" id (type_to_string typ) in
  let entries_to_string sep es = String.concat sep (List.map entry_to_string es) in
  match typ with
  | T_Num -> "num"
  | T_Bool -> "bool"
  | T_Unit -> "unit"
  | T_Func { coeff; from; to_ } ->
      let from_str = type_to_string from in
      let to_str = type_to_string to_ in
      sprintf "%s [ %s ] -> %s" from_str (entries_to_string ", " coeff) to_str
  | T_Chan { coeff; typ } ->
      sprintf "chan [ %s ] %s" (entries_to_string ", " coeff) (type_to_string typ)
  | T_Marsh { coeff; typ } ->
      sprintf "marsh [ %s ] %s" (entries_to_string ", " coeff) (type_to_string typ)
  | T_Rec es -> sprintf "{ %s }" (entries_to_string ", " es)

let rec type_check (prog : Ast.prog) =
  let ctx = Context.empty in
  type_check_expr ctx prog |> snd

and type_check_expr (ctx : Context.t) (expr : Ast.expr) : Coeffect.t * ET.expr =
  match expr with
  | E_Marshal { rebinds; body } ->
      (* Assert that there are no duplicate IDs in the rebindables list *)
      let () =
        assert_no_duplicates
          (List.map (fun id -> (id, ())) rebinds)
          (fun id -> sprintf "Duplicate member '%s' in marshal expression" id)
      in
      let r, body' = type_check_expr ctx body in
      let rebinds_with_types =
        List.map
          (fun v ->
            match Context.get_var ctx v with
            | Some t -> (v, t)
            | None -> failwith "Unbound variable: '%s'")
          rebinds
      in
      (* We convert the marshal rebinds into our Coeffect type to catch
         any errors, like in the theory! *)
      let s = Coeffect.from_ident_type_pairs (List.to_seq rebinds_with_types) in
      let coeff = Coeffect.get_vars (r @ s) |> List.of_seq in
      (Coeffect.empty, (ET.E_Marshal { rebinds; body = body' }, T_Marsh { coeff; typ = snd body' }))
  | E_Unmarshal { rebinds; body } ->
      (* Assert that there are no duplicate IDs in the context *)
      let () =
        assert_no_duplicates rebinds (fun id ->
            sprintf "Duplicate member '%s' in unmarshal expression" id)
      in
      let r, body' = type_check_expr ctx body in
      let rs, rebinds' =
        match snd body' with
        | T_Marsh { coeff; typ } ->
            (* A lot of things to do that we can finish off in one operation:
                 - We need to type check all rebind expressions 
                 - We need to convert these expressions to their ET type
                 - We need to ensure that all coeffect mappings have an associated expression
                 - We need to ensure that each expression associated with a coeffect satisfies the type
                 - We need to combine all our coeffects (graded type)
            *)
            let (rs, coeff'), rebinds' =
              List.map_and_foldl
                (fun (rs, coeff') (id, expr) ->
                  let r_i, expr' = type_check_expr ctx expr in
                  let () =
                    (* If the id exists in our coeff, ensure the types work! *)
                    match List.assoc_opt id coeff' with
                    | Some coeff_typ ->
                        assert_msg
                          (snd expr' <= coeff_typ)
                          (sprintf "Expected '%s' to be type '%s'" id (type_to_string coeff_typ))
                    | None -> ()
                  in
                  (* We remove the ID from our working coeffect if it exists *)
                  ((r_i @ rs, List.remove_assoc id coeff'), (id, expr')))
                (r, coeff) rebinds
            in
            (* Ensure that there are no remaining requirements *)
            let () =
              assert_msg (List.is_empty coeff')
                (sprintf "Missing some required rebindings: %s"
                   (String.concat ", " (List.map (fun (id, _) -> id) coeff')))
            in
            (rs, rebinds')
        | _ -> failwith "Expected marshaled type"
      in
      (rs, (ET.E_Unmarshal { rebinds = rebinds'; body = body' }, snd body'))
  | E_ChanSend { chan; package } ->
      let r, chan' = type_check_expr ctx chan in
      let s, package' =
        (* The chan expr must be of type 'T_Chan' with some coeffect 't' *)
        match snd chan' with
        | T_Chan { coeff; typ } -> (
            let _aux_fail t =
              failwith
                (sprintf "Expected type '%s' but got '%s'"
                   (type_to_string (ET.T_Marsh { coeff; typ }))
                   (type_to_string t))
            in
            let s, package' = type_check_expr ctx package in

            (* Now we must ensure that the argument is 'T_Marsh' with the same coeffect 't' *)
            match snd package' with
            | T_Marsh { coeff = coeff'; typ = typ' } as t ->
                (* Allow subcoeffecting and subtyping here *)
                (* TODO: Consider what it means semantically to do this? 
                         The channel guarantees the dynamic rebindings in its coeff
                         but what if the marsh doesn't need them all? What happens
                         in the unmarshal end in the case of shadowing? *)
                if t <= T_Marsh { coeff; typ } then (s, package') else _aux_fail t
            | _ -> _aux_fail (snd package'))
        | _ -> failwith "Expected channel type"
      in
      (r @ s, (ET.E_ChanSend { chan = chan'; package = package' }, T_Unit))
  | E_ChanReceive chan ->
      let r, chan' = type_check_expr ctx chan in
      let s, package_type =
        match snd chan' with
        | T_Chan { coeff; typ } -> (coeff, typ)
        | _ -> failwith "Expected channel type"
      in
      (r, (ET.E_ChanReceive chan', T_Marsh { coeff = s; typ = package_type }))
  | E_Abs { param; body } ->
      let param' = type_check_param ctx param in
      let ctx' = Context.add_var ctx param' in
      let r', body' = type_check_expr ctx' body in
      (* We get our coeffect 'r' without the parameter *)
      let r = Coeffect.remove_var r' (fst param') in
      let r_list = List.of_seq (Coeffect.get_vars r) in
      ( r,
        ( ET.E_Abs { param = param'; body = body' },
          (* We capture the coeffect as latent context of this function *)
          ET.T_Func { coeff = r_list; from = snd param'; to_ = snd body' } ) )
  | E_If { cond; if_; else_ } ->
      let r, cond' = type_check_expr ctx cond in
      let () =
        assert_msg
          (snd cond' <= ET.T_Bool)
          (sprintf "Expected type '%s'" (type_to_string ET.T_Bool))
      in
      let s, if_' = type_check_expr ctx if_ in
      let t, else_' = type_check_expr ctx else_ in

      (* One branch can result in a subtype of the other branch, 
         the resulting type will be the type of the most 'generic' *)
      ( r @ s @ t,
        ( ET.E_If { cond = cond'; if_ = if_'; else_ = else_' },
          if snd if_' <= snd else_' then snd else_'
          else if snd else_' <= snd if_' then snd if_'
          else failwith "Branches disagree on resulting type" ) )
  | E_Let { binder; mobility; value; body } ->
      let r, value' = type_check_expr ctx value in
      let s, body' =
        match binder with
        | Some x ->
            let ctx' = Context.add_var ctx (x, snd value') in
            let s', body' = type_check_expr ctx' body in
            (* We remove the binder from the free variable coeffect *)
            let s = Coeffect.remove_var s' x in
            (s, body')
        | None ->
            let s, body' = type_check_expr ctx body in
            (s, body')
      in
      (r @ s, (ET.E_Let { binder; mobility; value = value'; body = body' }, snd body'))
  | E_BinOp (op, e1, e2) ->
      let r, e1' = type_check_expr ctx e1 in
      let s, e2' = type_check_expr ctx e2 in
      let () =
        assert_msg
          (snd e1' <= ET.T_Num && snd e2' <= ET.T_Num)
          (sprintf "Expected type '%s'" (type_to_string ET.T_Num))
      in
      (r @ s, (ET.E_BinOp (op, e1', e2'), ET.T_Num))
  | E_App (callee, arg) -> (
      let r, callee' = type_check_expr ctx callee in
      match snd callee' with
      | T_Func { coeff; from; to_ } ->
          (* Extract the latent coeffect 't' from the function type *)
          let t = Coeffect.from_ident_type_pairs (List.to_seq coeff) in
          let s, arg' = type_check_expr ctx arg in
          let () =
            assert_msg
              (snd arg' <= from)
              (sprintf "Argument cannot be applied. Expected type '%s' but got '%s'"
                 (type_to_string from)
                 (type_to_string (snd arg')))
          in
          (r @ s @ t, (ET.E_App (callee', arg'), to_))
      | _ ->
          failwith
            (sprintf "Argument cannot be applied to type '%s'" (type_to_string (snd callee'))))
  | E_Access (exp, id) -> (
      let r, exp' = type_check_expr ctx exp in
      match snd exp' with
      | T_Rec es ->
          let entry_opt = List.find_opt (fun (id', _) -> id = id') es in
          let id', typ =
            match entry_opt with
            | Some e -> e
            | None -> failwith (sprintf "'%s' not present in '%s'" id (type_to_string (snd exp')))
          in
          (r, (ET.E_Access (exp', id), typ))
      | _ -> failwith (sprintf "Cannot access '%s' from non-record" id))
  | E_Var v -> (
      match Context.get_var ctx v with
      | None -> failwith (sprintf "Unbound variable: '%s'" v)
      | Some t -> (Coeffect.singleton v t, (ET.E_Var v, t)))
  | E_Num n -> (Coeffect.empty, (ET.E_Num n, ET.T_Num))
  | E_Bool v -> (Coeffect.empty, (ET.E_Bool v, ET.T_Bool))
  | E_Unit -> (Coeffect.empty, (ET.E_Unit, T_Unit))
  | E_Rec es ->
      (* Assert that there are no duplicate IDs in the record *)
      let () = assert_no_duplicates es (fun id -> sprintf "Duplicate member '%s' in record" id) in
      let rs, es' =
        List.map_and_foldl
          (fun rs (id, exp) ->
            let r_i, exp' = type_check_expr ctx exp in
            (* Union all of the coeffect contexts *)
            (r_i @ rs, (id, exp')))
          Coeffect.empty es
      in
      let tys = List.map (fun (id, exp) -> (id, snd exp)) es' in
      (rs, (ET.E_Rec es', T_Rec tys))

and type_check_param (ctx : Context.t) ((name, typ) : Ast.param) = (name, type_check_type ctx typ)

and type_check_type (ctx : Context.t) (typ : Ast.typ) =
  let type_check_coeff coeff =
    (* Assert there are no duplicate IDs in coeff *)
    let () = assert_no_duplicates coeff (fun id -> sprintf "Duplicate id '%s' in latent" id) in
    List.map (fun (v, typ) -> (v, type_check_type ctx typ)) coeff
  in
  match typ with
  | T_Num -> ET.T_Num
  | T_Bool -> ET.T_Bool
  | T_Unit -> ET.T_Unit
  | T_Func { coeff; from; to_ } ->
      let from' = type_check_type ctx from in
      let to_' = type_check_type ctx to_ in
      let coeff' = type_check_coeff coeff in
      ET.T_Func { coeff = coeff'; from = from'; to_ = to_' }
  | T_Chan { coeff; typ } ->
      let coeff' = type_check_coeff coeff in
      let typ' = type_check_type ctx typ in
      ET.T_Chan { coeff = coeff'; typ = typ' }
  | T_Marsh { coeff; typ } ->
      let coeff' = type_check_coeff coeff in
      let typ' = type_check_type ctx typ in
      ET.T_Marsh { coeff = coeff'; typ = typ' }
  | T_Rec es ->
      let es' = List.map (fun (id, typ) -> (id, type_check_type ctx typ)) es in
      ET.T_Rec es'

(* Subsumption *)
and ( <= ) (t1 : ET.typ) (t2 : ET.typ) =
  match t1 with
  | T_Func { coeff; from; to_ } -> (
      match t2 with
      (* Note, subsumption on functions is contravariant with respect to input types 
         and covariant with respect to output types *)
      | T_Func { coeff = coeff'; from = from'; to_ = to_' } ->
          let r = Coeffect.from_ident_type_pairs (List.to_seq coeff) in
          let r' = Coeffect.from_ident_type_pairs (List.to_seq coeff') in
          from' <= from && to_ <= to_' && Coeffect.(r <= r')
      | _ -> false)
  | T_Rec es -> (
      (* A record is a subtype of another if it is a superset 
         and its members respect subsumption rules *)
      match t2 with
      | T_Rec es' ->
          List.for_all
            (fun (id', typ') ->
              match List.assoc_opt id' es with None -> false | Some typ -> typ <= typ')
            es'
      | _ -> false)
  | T_Marsh { coeff; typ } -> (
      (* For a marshaled type, it must be covariant with its type
       and subcoeffecting must be satisfied *)
      match t2 with
      | T_Marsh { coeff = coeff'; typ = typ' } ->
          let r = Coeffect.from_ident_type_pairs (List.to_seq coeff) in
          let r' = Coeffect.from_ident_type_pairs (List.to_seq coeff') in
          typ <= typ' && Coeffect.(r <= r')
      | _ -> false)
  | _ -> t1 = t2
