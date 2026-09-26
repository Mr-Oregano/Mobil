open Rt
open Printf
module IDMap = Map.Make (String)

type t = value IDMap.t

let empty = IDMap.empty
let get_var env id = IDMap.find_opt id env
let add_var env (id, value) = IDMap.add id value env
let rem_var env id = IDMap.remove id env
let to_seq env = IDMap.to_seq env
let of_seq vs = IDMap.add_seq vs empty
