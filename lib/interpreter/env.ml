open Repr
open Repr.RT
open Printf

module Env = struct
  module IDMap = Map.Make (String)

  type t = RT.value IDMap.t

  let empty = IDMap.empty
  let get_var env id = IDMap.find_opt id env
  let add_var env (id, value) = IDMap.add id value env
  let rem_var env id = IDMap.remove id env
  let get_vars env = IDMap.to_seq env
end
