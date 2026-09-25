open Repr
open Repr.RT
open Typing.Repr.ET
open Printf

type t

val empty : t
val get_var : t -> id -> value option
val add_var : t -> id * value -> t
val rem_var : t -> id -> t
val to_seq : t -> (id * value) Seq.t
val of_seq : (id * value) Seq.t -> t
