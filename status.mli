(**to be in state.ml *)
type status = Poison | Blind | Paralysis | Silence 
(* val change_status: string -> status -> State.t -> State.t

   val get_status: string -> State.t -> State.t  *)

(** to actually be in status.ml *)
(* type t * which will be status and status wont be in state *)

(* val effects_of_Poisson 
   val effects_of_Blind
   val effects_of_Paralysis
   val effects_of_Silence *)