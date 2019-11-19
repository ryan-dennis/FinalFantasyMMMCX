(** The type of cast spell data. *)
type t = {
  m_dmg : int;
  m_target : string;
  m_desc : string;
  m_new_st : State.t
}

(** Exception raised when an invalid target for a spell is chosen *)
exception InvalidSpellTarget

(** Exception raised when a character does not have sufficient MP to cast the
    chosen spell *)
exception NotEnoughMP

(** The type of magic element. *)
type element = Fire | Ice | Lightning | Poison | Status | None

(** The type of spell. *)
type spell

(** [get_spell s] is the spell with name [s]. *)
val get_spell : string -> spell

(** [is_valid_target st sp tar] is whether the target [tar] is a valid target
    for the spell [sp] in state [st]. *)
val is_valid_target : State.t -> spell -> string -> bool

(** [is_enough_mp sp c st] is whether the character [c] has enough MP left to
    cast spell [sp] in state [st]. *)
val is_enough_mp : spell -> Party.t -> State.t -> bool

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

(** [cast_boss_skill glt sp tar st] is the cast skill data after the skill [sp]
    has been cast by the boss from gauntlet [glt] in state [st] on the target
    [tar]. *)
val cast_boss_skill : Gauntlet.t -> skill -> string -> State.t -> t