(** The type of cast spell data. *)
type t = {
  dmg : int;
  target : string;
  desc : string;
  new_st : State.t
}

(** The type of magic element. *)
type element = Fire | Ice | Lightning | Poison | Status | None

(** The type of spell. *)
type spell

(** [get_spell s] is the spell with name [s]. *)
val get_spell : string -> spell

(** [is_valid_target sp tar] is whether the target [tar] is a valid target for
    the spell [sp]. *)
val is_valid_target : spell -> string -> bool

(** [cast_spell glt st sp c tar] is the cast spell data after the spell [sp]
    has been cast by character [c] in state [st] on the target [tar]. *)
val cast_spell : Gauntlet.t -> State.t -> spell -> Party.t -> string -> t



(** The type of skill. *)
type skill

(** [get_skill s] is the skill with name [s]. *)
val get_skill : string -> skill

(** [cast_boss_spell glt sp tar st] is the cast spell data after the spell [sp]
    has been cast by the boss from gauntlet [glt] in state [st] on the target
    [tar]. *)
val cast_boss_spell : Gauntlet.t -> spell -> string -> State.t -> t

(** [cast_boss_skill glt sp st] is the cast skill data after the skill [sp]
    has been cast by the boss from gauntlet [glt] in state [st]. *)
val cast_boss_skill : Gauntlet.t -> spell -> State.t -> t