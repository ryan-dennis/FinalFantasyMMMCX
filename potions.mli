(** 
   Contains all the necessary potions and effects used when the player uses the
   drink command.
*)

(** The abstract type representing a potion. *)
type potion = 
  | Heal
  | Pure

(** Raised when a malformed spell is encountered *)
exception Malformed

(** [drink st sp] is the new [State.t] after spell [sp] has been used. *)
val drink: State.t -> string -> State.t