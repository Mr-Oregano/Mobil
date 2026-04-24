open Structs.ET

module Context : sig
  type t

  val empty : t
  val add_var : t -> id * typ -> t
  val remove_var : t -> id -> t
  val get_var : t -> id -> typ option
  val get_vars : t -> (id * typ) Seq.t
end
