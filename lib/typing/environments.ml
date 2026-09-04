open Structs.ET
open Printf

let assert_msg cond msg = if not cond then failwith msg

module Context = struct
  module IDMap = Map.Make (String)

  type t = typ IDMap.t

  let empty = IDMap.empty

  (* TODO: Allow identifier shadowing *)
  let _assert_not_contains map id =
    if not (map |> IDMap.mem id) then ()
    else failwith (Printf.sprintf "Already bound identifier '%s'" id)

  let add_var ctx (id, typ) =
    _assert_not_contains ctx id;
    ctx |> IDMap.add id typ

  let remove_var ctx name = ctx |> IDMap.remove name
  let get_var ctx name = ctx |> IDMap.find_opt name
  let get_vars ctx = IDMap.to_seq ctx
end

module Coeffect = struct
  module IDMap = Map.Make (String)

  type t = typ IDMap.t

  let empty = IDMap.empty
  let singleton = IDMap.singleton

  (* Unlike in the theory, the coeffect here will just fail when this happens *)
  let _assert_not_contains map id =
    if not (map |> IDMap.mem id) then ()
    else failwith (Printf.sprintf "Already bound identifier '%s'" id)

  let add_var coeff (id, typ) =
    _assert_not_contains coeff id;
    coeff |> IDMap.add id typ

  let remove_var coeff name = coeff |> IDMap.remove name

  let remove_vars coeff names =
    Seq.fold_left (fun coeff' name -> remove_var coeff' name) coeff names

  let get_var coeff name = coeff |> IDMap.find_opt name
  let get_entries coeff = IDMap.to_seq coeff

  (* Coeffect utilities *)
  let ( <= ) coeff1 coeff2 =
    IDMap.for_all
      (fun key value ->
        match IDMap.find_opt key coeff2 with
        (* Would be interesting to see if we can use subtyping here
           but for now we stick to the theory, strict equality necessary *)
        | Some value' -> value = value'
        | None -> false)
      coeff1

  let merge coeff1 coeff2 =
    IDMap.union
      (fun v typ1 typ2 ->
        if typ1 <> typ2 then failwith (sprintf "Failed to merge coeffects, collision with '%s'" v)
        else Some typ1)
      coeff1 coeff2

  let ( @ ) = merge

  (* Not mentioned in the theory, but useful for creating a coeffect from other types *)
  let from_ident_type_pairs (es : (id * typ) Seq.t) = IDMap.add_seq es empty
end
