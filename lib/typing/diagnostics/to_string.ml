open Printf
open Et

let rec type_to_string (typ : typ) =
  (* TODO: Ideally restrict the total width of the string to a fixed amount of characters *)
  let entry_to_string (id, typ) = sprintf "%s: %s" id (type_to_string typ) in
  let entries_to_string format sep es =
    if List.is_empty es then String.empty
    else sprintf format (String.concat sep (List.map entry_to_string es))
  in
  match typ with
  | T_Num -> "num"
  | T_Bool -> "bool"
  | T_Unit -> "unit"
  | T_Func { from; to_ } ->
      let from_str = type_to_string from in
      let to_str = type_to_string to_ in
      sprintf "%s -> %s" from_str to_str
  | T_Chan { coeff; typ } ->
      sprintf "chan%s %s" (entries_to_string " [ %s ]" ", " coeff) (type_to_string typ)
  | T_Marsh { coeff; typ } ->
      sprintf "marsh%s %s" (entries_to_string " [ %s ]" ", " coeff) (type_to_string typ)
  | T_Rec es -> entries_to_string "{ %s }" ", " es
