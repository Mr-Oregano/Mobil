open Et

(* This will be used as the 'Input' of type checking (TOP-DOWN analysis) *)
module Vars : sig
  type t

  val empty : t
  val get_var : t -> id -> typ option
  val add_var : t -> id * typ -> t
  val rem_var : t -> id -> t
  val to_seq : t -> (id * typ) Seq.t
  val of_seq : (id * typ) Seq.t -> t
end

(* This will be used as the 'Result' of type checking (BOTTOM-UP synthesis) *)
module Coeffect : sig
  type t

  val empty : t
  val singleton : id -> typ -> t
  val get_var : t -> id -> typ option
  val add_var : t -> id * typ -> t
  val remove_var : t -> id -> t
  val remove_vars : t -> id Seq.t -> t
  val ( <= ) : t -> t -> bool
  val merge : t -> t -> t
  val ( @ ) : t -> t -> t
  val to_seq : t -> (id * typ) Seq.t
  val of_seq : (id * typ) Seq.t -> t
end
