open Typing.Structs
open Printf

(* Nodes in graphviz are represented by their integer ID *)
type node = int

let op_to_string (op : Ast.op) = match op with O_Add -> "+" | O_Sub -> "-"

let eval_graphviz (out : out_channel) (prog : Ast.prog) =
  let outs = Out_channel.output_string out in
  let counter = ref 0 in

  let rec make_node (name : string) (children : node list) =
    let () = incr counter in
    let () = outs (sprintf "%d [label=\"%s\"]; " !counter name) in
    let () = children |> List.iter (make_edge !counter) in
    !counter
  and make_leaf (name : string) = make_node name []
  and make_edge (parent : node) (child : node) = outs (sprintf "%d -> %d; " parent child) in

  let rec eval_expr (e : Ast.expr) =
    match e with
    | E_Marshal { context; body } ->
        let ctx = make_node "[ ... ]" (List.map eval_id context) in
        make_node "E_Marshal" [ ctx; eval_expr body ]
    | E_Unmarshal { context; body } ->
        let ctx = make_node "[ ... ]" (List.map (eval_ident_expr_pair "<coeff>") context) in
        make_node "E_Unmarshal" [ ctx; eval_expr body ]
    | E_Abs { param; body } -> make_node "E_Abs" [ eval_param param; eval_expr body ]
    | E_If { cond; if_; else_ } ->
        make_node "E_If"
          [
            make_leaf "if";
            eval_expr cond;
            make_leaf "then";
            eval_expr if_;
            make_leaf "else";
            eval_expr else_;
          ]
    | E_Let { binder; mobility; value; body } ->
        make_node "E_Let"
          [
            make_leaf "let";
            eval_mobility mobility;
            make_leaf (Option.value binder ~default:"_");
            make_leaf "=";
            eval_expr value;
            make_leaf "in";
            eval_expr body;
          ]
    | E_ChanSend { chan; package } ->
        make_node "E_ChanSend" [ eval_expr chan; make_leaf "!"; eval_expr package ]
    | E_ChanReceive e -> make_node "E_ChanReceive" [ eval_expr e; make_leaf "?" ]
    | E_BinOp (op, e1, e2) ->
        make_node "E_BinOp" [ eval_expr e1; make_leaf (op_to_string op); eval_expr e2 ]
    | E_App (callee, arg) -> make_node "E_App" [ eval_expr callee; eval_expr arg ]
    | E_Access (e, id) -> make_node "E_Access" [ eval_id id; eval_expr e ]
    | E_Var v -> make_leaf (sprintf "E_Var (%s)" v)
    | E_Num n -> make_leaf (sprintf "E_Num (%s)" (Int.to_string n))
    | E_Bool b -> make_leaf (sprintf "E_Bool (%s)" (Bool.to_string b))
    | E_Rec es -> make_node "E_Rec { ... }" (List.map (eval_ident_expr_pair "<entry>") es)
    | E_Unit -> make_leaf (sprintf "E_Unit")
  and eval_mobility (m : Ast.mobility) =
    match m with M_Mobile -> make_leaf "M_Mobile" | M_iMobile -> make_leaf "M_iMobile"
  and eval_typ (t : Ast.typ) =
    match t with
    | T_Num -> make_leaf "T_Num"
    | T_Unit -> make_leaf "T_Unit"
    | T_Bool -> make_leaf "T_Bool"
    | T_Func { from; to_ } -> make_node "T_Func" [ eval_typ from; make_leaf "->"; eval_typ to_ ]
    | T_Chan { context; typ } ->
        let context' = make_node "[ ... ]" (List.map (eval_ident_type_pair "<coeff>") context) in
        make_node "T_Chan" [ context'; eval_typ typ ]
    | T_Marsh { context; typ } ->
        let context' = make_node "[ ... ]" (List.map (eval_ident_type_pair "<coeff>") context) in
        make_node "T_Marsh" [ context'; eval_typ typ ]
    | T_Rec es -> make_node "T_Rec { ... }" (List.map (eval_ident_type_pair "<entry>") es)
  and eval_param (v : Ast.param) = eval_ident_type_pair "<param>" v
  and eval_ident_expr_pair name (id, v) = make_node name [ eval_id id; eval_expr v ]
  and eval_ident_type_pair name (id, typ_) = make_node name [ eval_id id; eval_typ typ_ ]
  and eval_id (i : Ast.id) = make_leaf (sprintf "ID (%s)" i) in

  let () = outs "digraph { ordering=\"out\" " in
  let _ = eval_expr prog in
  outs " }"
