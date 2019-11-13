type element = Fire | Ice | Lightning | Poison | Status | None

(** Type of the effectivity of a spell *)
type effectivity =
  | Eff of int
  | EStatus of Status.status

(** Type of a spell effect *)
type effect =
  | Damage
  | StatusAilment
  | HitDown
  | HPRec
  | RestoreStatus
  | DefUp
  | AtkUp
  | HitUp
  | AtkAccUp
  | FullHP
  | EvaUp
  | HP300Status

(** Type of a spell *)
type spell = {
  name : string;
  eff : effectivity;
  acc : int;
  el : element;
  effect : effect;
  mp : int
}

(** [spell] is a spell with name [name], effectivity [eff], accuracy [acc]
    element [el], effect [effect], and cost [mp]. *)
let spell name eff acc el effect mp = {
  name = name;
  eff = eff;
  acc = acc;
  el = el;
  effect = effect;
  mp = mp
}

(** List of magic points required for each level of spell *)
let (mp1, mp2, mp3, mp4, mp5, mp6, mp7, mp8) =
  (5, 10, 15, 20, 25, 30, 40, 50)

(** [spell_list] is the list of all available spells. *)
let spell_list = [
  spell "CURE" (Eff 16) 0 None HPRec mp1;
  spell "FOG" (Eff 8) 0 None DefUp mp1;
  spell "RUSE" (Eff 80) 0 None EvaUp mp1;
  spell "FIRE" (Eff 10) 24 Fire Damage mp1;
  spell "LIT" (Eff 10) 24 Lightning Damage mp1;
  spell "LAMP" (EStatus Blinded) 0 None RestoreStatus mp2;
  spell "MUTE" (EStatus Silenced) 64 Status StatusAilment mp2;
  spell "INVS" (Eff 40) 0 None EvaUp mp2;
  spell "ICE" (Eff 20) 24 Ice Damage mp2;
  spell "DARK" (EStatus Blinded) 24 Status StatusAilment mp2;
  spell "TMPR" (Eff 14) 24 None AtkAccUp mp2;
  spell "SLOW" (Eff 0) 64 Status HitDown mp2;
  spell "CUR2" (Eff 33) 0 None HPRec mp3;
  spell "HEAL" (Eff 12) 0 None HPRec mp3;
  spell "FIR2" (Eff 30) 24 Fire Damage mp3;
  spell "HOLD" (EStatus Paralyzed) 64 Status StatusAilment mp3;
  spell "LIT2" (Eff 30) 24 Lightning Damage mp3;
  spell "PURE" (EStatus Poisoned) 0 None RestoreStatus mp4;
  spell "AMUT" (EStatus Silenced) 0 None RestoreStatus mp4;
  spell "FAST" (Eff 0) 0 None HitUp mp4;
  spell "ICE2" (Eff 40) 24 Ice Damage mp4;
  spell "CUR3" (Eff 66) 0 None HPRec mp5;
  spell "HEL2" (Eff 24) 0 None HPRec mp5;
  spell "FIR3" (Eff 50) 24 Fire Damage mp5;
  spell "SLO2" (Eff 0) 64 None HitDown mp5;
  spell "FOG2" (Eff 12) 0 None DefUp mp6;
  spell "INV2" (Eff 40) 0 None EvaUp mp6;
  spell "LIT3" (Eff 60) 24 Lightning Damage mp6;
  spell "STUN" (EStatus Paralyzed) 0 Status HP300Status mp6;
  spell "CUR4" (Eff 0) 0 None FullHP mp7;
  spell "HEL3" (Eff 48) 48 None HPRec mp7;
  spell "ICE3" (Eff 70) 24 Ice Damage mp7;
  spell "BRAK" (EStatus Poisoned) 64 Poison StatusAilment mp7;
  spell "SABR" (Eff 60) 64 None AtkAccUp mp7;
  spell "BLND" (EStatus Blinded) 0 Status HP300Status mp7;
  spell "FADE" (Eff 80) 107 None Damage mp8;
  spell "NUKE" (Eff 100) 107 None Damage mp8
]

(** Type for the number of targets *)
type target = All | Single

(** Type of a skill *)
type skill = {
  name : string;
  eff : effectivity;
  acc : int;
  el : element;
  effect : effect;
  target : target
}

(** [skill] is a skill with name [name], effectivity [eff], accuracy [acc]
    element [el], effect [effect], and target [target]. *)
let skill name eff acc el effect target = {
  name = name;
  eff = eff;
  acc = acc;
  el = el;
  effect = effect;
  target = target
}

(** [skill_list] is the list of all available skills. *)
let skill_list = [
  skill "FROST" (Eff 24) 32 Ice Damage All;
  skill "HEAT" (Eff 12) 32 Fire Damage All;
  skill "GAZE" (EStatus Paralyzed) 0 Status StatusAilment Single;
  skill "FLASH" (EStatus Blinded) 24 Status StatusAilment All;
  skill "SCORCH" (Eff 7) 32 Fire Damage All;
  skill "STARE" (Eff 17) 24 None Damage Single;
  skill "BLIZZARD" (Eff 50) 32 Ice Damage All;
  skill "BLAZE" (Eff 64) 32 Fire Damage All;
  skill "INFERNO" (Eff 96) 32 Fire Damage All;
  skill "CREMATE" (Eff 24) 32 Fire Damage All;
  skill "TRANCE" (EStatus Paralyzed) 0 None StatusAilment All;
  skill "POISON" (Eff 68) 32 Poison Damage All;
  skill "THUNDER" (Eff 76) 32 Lightning Damage All;
  skill "SNORTING" (EStatus Blinded) 24 Status StatusAilment Single;
  skill "NUCLEAR" (Eff 80) 48 None Damage All;
  skill "INK" (EStatus Blinded) 24 Status StatusAilment All;
  skill "STINGER" (EStatus Poisoned) 0 Poison StatusAilment All;
  skill "DAZZLE" (EStatus Paralyzed) 32 Status StatusAilment Single;
  skill "SWIRL" (Eff 64) 32 None Damage All;
  skill "TORNADO" (Eff 64) 32 None Damage All
]

