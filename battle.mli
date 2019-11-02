(** 
   Controls combat between player characters and AI enemy.

   This module calculates the effect of a player character's turn and applies
   it to the current battle state. It also controls the AI enemy's turn.
*)

(** [fight st c] is the new state after the character [c] uses its fight
    attack on the enemy. *)
val fight : Gauntlet.t -> State.t -> Party.t -> State.t

val magic : State.t -> State.t

val drink : State.t -> State.t

(** [boss_turn glt st] is the new state after the boss during [st] from
    gauntlet [glt] takes its turn. *)
val boss_turn : Gauntlet.t -> State.t -> State.t