open Structs
open Structs.ET

(* This will be used as the 'Input' of type checking (TOP-DOWN analysis) *)
module Context : sig
  type t

  val empty : t
  val add_var : t -> id * typ -> t
  val remove_var : t -> id -> t
  val get_var : t -> id -> typ option
  val get_vars : t -> (id * typ) Seq.t
end

(* This will be used as the 'Result' of type checking (BOTTOM-UP synthesis) *)
module Coeffect : sig
  type t

  val empty : t
  val singleton : id -> typ -> t
  val add_var : t -> id * typ -> t
  val remove_var : t -> id -> t
  val remove_vars : t -> id Seq.t -> t
  val get_var : t -> id -> typ option
  val get_entries : t -> (id * typ) Seq.t
  val ( <= ) : t -> t -> bool
  val merge : t -> t -> t
  val ( @ ) : t -> t -> t
  val from_ident_type_pairs : (id * typ) Seq.t -> t
end
