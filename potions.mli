(** 
   Contains all the necessary potions and effects used when the player uses the
   drink command.
*)

(** The abstract type representing a potion. *)
type potion = 
  | Heal
  | Pure

(** Raised when a [Invalid_potion] pot is encountered *)
exception Invalid_potion

val heal_eff: State.t -> State.t

val pure_eff: State.t -> State.t

(** [drink p] is the new [State.t] after potion [pot] has been used.
    Raises: Malformed if the spell is not a *)
val drink: string -> potion