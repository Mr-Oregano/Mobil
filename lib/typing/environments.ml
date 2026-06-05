open Structs.ET
open Printf

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

  let add_var ctx (id, typ) =
    _assert_not_contains ctx id;
    ctx |> IDMap.add id typ

  let remove_var ctx name = ctx |> IDMap.remove name
  let get_var ctx name = ctx |> IDMap.find_opt name
  let get_vars ctx = IDMap.to_seq ctx

  (* Coeffect utilities *)
  let ( <= ) ctx1 ctx2 =
    IDMap.for_all
      (fun key value ->
        match IDMap.find_opt key ctx2 with
        (* Would be interesting to see if we can use subtyping here
           but for now we stick to the theory, strict equality necessary *)
        | Some value' -> value = value'
        | None -> false)
      ctx1

  let merge ctx1 ctx2 =
    IDMap.union
      (fun v typ1 typ2 ->
        if typ1 <> typ2 then failwith (sprintf "Failed to merge coeffects, collision with '%s'" v)
        else Some typ1)
      ctx1 ctx2

  let ( @ ) = merge

  (* Not mentioned in the theory, but useful for creating a coeffect from other types *)
  let from_ident_type_pairs (es : (id * typ) Seq.t) = IDMap.add_seq es empty
end
