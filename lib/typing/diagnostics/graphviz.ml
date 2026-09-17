open Repr
open Printf
open To_string

(* Nodes in graphviz are represented by their integer ID *)
type node = int

let op_to_string (op : ET.op) = match op with O_Add -> "+" | O_Sub -> "-"

let eval_graphviz out (prog : ET.prog) =
  let outs = Out_channel.output_string out in
  let counter = ref 0 in

  let rec make_node name (children : node list) =
    let () = incr counter in
    let () = outs (sprintf "%d [label=\"%s\"]; " !counter name) in
    let () = children |> List.iter (make_edge !counter) in
    !counter
  and make_leaf name = make_node name []
  and make_edge (parent : node) (child : node) = outs (sprintf "%d -> %d; " parent child) in
  let make_seq name func vs = make_node name (List.map func vs) in
  let make_list func vs = make_seq "[ ... ]" func vs in

  let rec eval_expr (e : ET.expr) =
    let make_coeff coeff = make_list (eval_ident_type_pair "<entry>") coeff in
    let make_rebinds rebinds = make_list (eval_ident_expr_pair "<entry>") rebinds in
    let make_rec entries = make_seq "{ ... }" (eval_ident_expr_pair "<entry>") entries in

    let typ = snd e in
    let name, children =
      match fst e with
      | E_Marshal body -> ("E_Marshal", [ eval_expr body ])
      | E_Unmarshal { rebinds; body } -> ("E_Unmarshal", [ make_rebinds rebinds; eval_expr body ])
      | E_Abs { param; body } -> ("E_Abs", [ eval_param param; eval_expr body ])
      | E_If { cond; if_; else_ } -> ("E_If", [ eval_expr cond; eval_expr if_; eval_expr else_ ])
      | E_Let { binder; value; body } ->
          ( "E_Let",
            [ make_leaf (Option.value binder ~default:"_"); eval_expr value; eval_expr body ] )
      | E_ChanSend { chan; package } ->
          ("E_ChanSend", [ eval_expr chan; make_leaf "!"; eval_expr package ])
      | E_ChanReceive e -> ("E_ChanReceive", [ eval_expr e; make_leaf "?" ])
      | E_NewChan { requirements; typ } -> ("E_NewChan", [ make_coeff requirements; eval_typ typ ])
      | E_BinOp (op, e1, e2) ->
          ("E_BinOp ", [ eval_expr e1; make_leaf (op_to_string op); eval_expr e2 ])
      | E_App (callee, arg) -> ("E_App", [ eval_expr callee; eval_expr arg ])
      | E_Access (e, id) -> ("E_Access", [ eval_id id; eval_expr e ])
      | E_Var v -> (sprintf "E_Var '%s'" v, [])
      | E_Num n -> (sprintf "E_Num '%s'" (Int.to_string n), [])
      | E_Bool b -> (sprintf "E_Bool '%s'" (Bool.to_string b), [])
      | E_Rec es -> ("E_Rec", [ make_rec es ])
      | E_Unit -> ("E_Unit", [])
    in
    make_node (sprintf "%s : %s" name (type_to_string typ)) children
  and eval_typ (t : ET.typ) =
    let make_coeff coeff = make_list (eval_ident_type_pair "<entry>") coeff in
    let make_rec entries = make_seq "{ ... }" (eval_ident_type_pair "<entry>") entries in
    match t with
    | T_Num -> make_leaf "T_Num"
    | T_Unit -> make_leaf "T_Unit"
    | T_Bool -> make_leaf "T_Bool"
    | T_Func { from; to_ } -> make_node "T_Func" [ eval_typ from; make_leaf "->"; eval_typ to_ ]
    | T_Chan { coeff; typ } -> make_node "T_Chan" [ make_coeff coeff; eval_typ typ ]
    | T_Marsh { coeff; typ } -> make_node "T_Marsh" [ make_coeff coeff; eval_typ typ ]
    | T_Rec es -> make_node "T_Rec" [ make_rec es ]
  and eval_param (v : ET.param) =
    eval_ident_type_pair "<param>" (match v with Some name, t -> (name, t) | None, t -> ("_", t))
  and eval_ident_expr_pair name (id, v) = make_node name [ eval_id id; eval_expr v ]
  and eval_ident_type_pair name (id, typ_) = make_node name [ eval_id id; eval_typ typ_ ]
  and eval_id (i : ET.id) = make_leaf (sprintf "ID '%s'" i) in

  let () = outs "digraph { ordering=\"out\" " in
  let _ = eval_expr prog in
  outs " }"
