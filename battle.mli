(** 
   Controls combat between player characters and AI enemy.

   This module calculates the effect of a player character's turn and applies
   it to the current battle state. It also controls the AI enemy's turn.
*)

(** The abstract type of values representing battle information of one turn. *)
type t

(** [fight glt st c] is the new state after the character [c] uses its fight
    attack on the enemy and the accompanying descriptor. *)
val fight : Gauntlet.t -> State.t -> Party.t -> t

(** [magic glt st s c tar] is the new state after the spell with name [s] is
    cast by the character [c] on target [tar]. Raises [InvalidSpellTarget] if
    invalid spell target is selected, or [NotEnoughMP] if the character does
    not have enough MP to cast the given spell. *)
val magic : Gauntlet.t -> State.t -> string -> Party.t -> string -> t

(** [boss_turn glt st] is the new state after the boss during [st] from
    gauntlet [glt] takes its turn and the accompanying descriptor. *)
val boss_turn : Gauntlet.t -> State.t -> t


(** [num_hits b] is the number of hits during the battle turn [b]. *)
val num_hits : t -> int

(** [dmg b] is the damage dealt during the battle turn [b]. *)
val dmg : t -> int

(** [target b] is the target of the attack during the battle turn [b]. *)
val target : t -> string

(** [desc b] is the description of the last battle turn [b]. *)
val desc : t -> string

(** [new_st b] is the new state after battle turn [b] has finished. *)
val new_st : t -> State.t