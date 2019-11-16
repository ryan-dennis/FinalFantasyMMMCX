open State
open Gauntlet
open Status

type t = {
  dmg : int;
  target : string;
  desc : string;
  new_st : State.t
}

(******************************************************************************
   PLAYER CHARACTER MAGIC
 ******************************************************************************)

type element = Fire | Ice | Lightning | Poison | Status | None

type status = Status.t

(** Type of the effectivity of a spell *)
type effectivity =
  | Eff of int
  | EStatus of status

(** Type of a spell effect.
    Damage (Damage): always hits, uses attack magic algorithms
    Status (StatusAilment): if it hits, inflicts the status ailment of EStatus
    Support (HPRec, RestoreStatus, DefUp, AtkUp, HitUp, AtkAccUp, FullHP,
    EvaUp): always hits, positive effects that benefit a player character
    HP300 (HP300Status): always hits if target is not resistant and current HP
    is equal to or less than 300 and always misses otherwise, inflicts the
    status ailment of EStatus if hits
*)
type effect =
  | Damage
  | StatusAilment
  | HPRec
  | FullHP
  | RestoreStatus
  | DefUp
  | StrUp
  | StrHitUp
  | AglUp
  | HP300Status

type spell = {
  sp_name : string;
  sp_eff : effectivity;
  sp_acc : int;
  sp_el : element;
  sp_effect : effect;
  sp_mp : int
}

(** [spell] is a spell with name [name], effectivity [eff], accuracy [acc]
    element [el], effect [effect], and cost [mp]. *)
let spell name eff acc el effect mp = {
  sp_name = name;
  sp_eff = eff;
  sp_acc = acc;
  sp_el = el;
  sp_effect = effect;
  sp_mp = mp
}

(** List of magic points required for each level of spell *)
let (mp1, mp2, mp3, mp4, mp5, mp6, mp7, mp8) =
  (5, 10, 15, 20, 25, 30, 40, 50)

(** [spell_list] is the list of all available spells. *)
let spell_list = [
  spell "CURE" (Eff 16) 0 None HPRec mp1;
  spell "FOG" (Eff 8) 0 None DefUp mp1;
  spell "RUSE" (Eff 80) 0 None AglUp mp1;
  spell "FIRE" (Eff 10) 24 Fire Damage mp1;
  spell "LIT" (Eff 10) 24 Lightning Damage mp1;
  spell "LAMP" (EStatus Blinded) 0 None RestoreStatus mp2;
  spell "MUTE" (EStatus Silenced) 64 Status StatusAilment mp2;
  spell "INVS" (Eff 40) 0 None AglUp mp2;
  spell "ICE" (Eff 20) 24 Ice Damage mp2;
  spell "DARK" (EStatus Blinded) 24 Status StatusAilment mp2;
  spell "TMPR" (Eff 14) 24 None StrHitUp mp2;
  spell "CUR2" (Eff 33) 0 None HPRec mp3;
  spell "HEAL" (Eff 12) 0 None HPRec mp3;
  spell "FIR2" (Eff 30) 24 Fire Damage mp3;
  spell "HOLD" (EStatus Paralyzed) 64 Status StatusAilment mp3;
  spell "LIT2" (Eff 30) 24 Lightning Damage mp3;
  spell "PURE" (EStatus Poisoned) 0 None RestoreStatus mp4;
  spell "AMUT" (EStatus Silenced) 0 None RestoreStatus mp4;
  spell "ICE2" (Eff 40) 24 Ice Damage mp4;
  spell "CUR3" (Eff 66) 0 None HPRec mp5;
  spell "HEL2" (Eff 24) 0 None HPRec mp5;
  spell "FIR3" (Eff 50) 24 Fire Damage mp5;
  spell "FOG2" (Eff 12) 0 None DefUp mp6;
  spell "INV2" (Eff 40) 0 None AglUp mp6;
  spell "LIT3" (Eff 60) 24 Lightning Damage mp6;
  spell "STUN" (EStatus Paralyzed) 0 Status HP300Status mp6;
  spell "CUR4" (Eff 0) 0 None FullHP mp7;
  spell "HEL3" (Eff 48) 48 None HPRec mp7;
  spell "ICE3" (Eff 70) 24 Ice Damage mp7;
  spell "BRAK" (EStatus Poisoned) 64 Poison StatusAilment mp7;
  spell "SABR" (Eff 60) 64 None StrHitUp mp7;
  spell "BLND" (EStatus Blinded) 0 Status HP300Status mp7;
  spell "FADE" (Eff 80) 107 None Damage mp8;
  spell "NUKE" (Eff 100) 107 None Damage mp8
]

(** [string_of_el el] is the element [el] as a string. *)
let string_of_el el =
  match el with
  | Fire -> "Fire"
  | Ice -> "Ice"
  | Lightning -> "Lightning"
  | Poison -> "Poison"
  | Status -> "Status"
  | None -> "None"

(** [string_of_status status] is the status [status] as a string. *)
let string_of_status status =
  match status with
  | Poisoned -> "Poisoned"
  | Blinded -> "Blinded"
  | Paralyzed -> "Paralyzed"
  | Silenced -> "Silenced"

(** [write_spell_desc st tar hit effect] is the description of the last turn,
    if the last turn a player character cast a spell. *)
let write_spell_desc st sp tar hit effect =
  let cur_fighter = get_current_fighter st in
  let spell_desc = match sp.sp_effect with
    | Damage -> ["dealt"; effect; string_of_el sp.sp_el; "damage!"]
    | StatusAilment -> ["inflicted"; effect ^ "!"]
    | HPRec -> ["healed them for"; effect; "HP!"]
    | FullHP -> ["healed them completely!"]
    | RestoreStatus -> ["cured their"; effect ^ "!"]
    | DefUp -> ["raised their defense!"]
    | StrUp -> ["raised their strength!"]
    | StrHitUp -> ["raised their strength and hit rate!"]
    | AglUp -> ["raised their agility!"]
    | HP300Status -> ["inflicted"; effect ^ "!"]
  in
  if hit = false
  then String.concat " " [cur_fighter; "tried to cast"; sp.sp_name; "on"; tar;
                          "but it failed!"]
  else [cur_fighter; "cast"; sp.sp_name; "on"; tar; "and"] @ spell_desc |>
       String.concat " "

let get_spell s =
  List.find (fun x -> x.sp_name = s) spell_list

(** [is_support sp] is whether the spell [sp] is a support spell (used on
    teammates). *)
let is_support sp =
  match sp.sp_effect with
  | HPRec | FullHP | RestoreStatus | DefUp | StrUp | StrHitUp | AglUp
  | HP300Status -> true
  | _ -> false

let is_valid_target sp tar =
  let support = is_support sp in
  try ignore(Party.find_character tar Party.get_characters);
    if support then true else false
  with Party.UnknownCharacter _ ->
    if support then false else true

(** [is_boss_resistant sp b] is whether the boss [b] is resistant to the
    element of spell [sp]. *)
let is_boss_resistant glt sp b =
  List.exists (fun x -> x = string_of_el(sp.sp_el))
    (Gauntlet.boss_stats glt b).resist

(** [is_boss_weak glt sp b] is whether the boss [b] is weak to the element of
    spell [sp]. *)
let is_boss_weak glt sp b =
  List.exists (fun x -> x = string_of_el(sp.sp_el))
    (Gauntlet.boss_stats glt b).weak

(** [dmg_status_hit_roll glt st sp] is whether a Damage or StatusAilment spell
    [sp] hits the boss [b] from gauntlet [glt] in state [st]. If the spell has
    the StatusAilment effect, then it hits; if the spell has the Damage effect,
    then the damage dealt is doubled. *)
let dmg_status_hit_roll glt st sp b =
  let weak = is_boss_weak glt sp b in
  let res = is_boss_resistant glt sp b in
  let base_cth = if weak && res then 40
    else if weak then 188
    else if res then 0
    else 148 in
  let cth = base_cth + sp.sp_acc - (boss_stats glt b).mdef in
  if Random.int 200 <= cth then true
  else false

(** [damage_spell_dmg glt st sp b hit] is the amount of damage the Damage spell
    [sp] cast on the boss [b] from gauntlet [glt] in state [st] deals. *)
let damage_spell_dmg glt st sp b hit =
  let weak = is_boss_weak glt sp b in
  let res = is_boss_resistant glt sp b in
  let eff = match sp.sp_eff with
    | Eff e -> if res then e / 2 else if weak then e * 3 / 2 else e
    | EStatus status -> failwith "Damage spell cannot inflict a status" in
  let res_multiplier = if hit then 2 else 1 in
  Random.int eff * 2 * res_multiplier

(** [cast_damage_spell glt sp c tar st] is the cast spell data after Damage
    spell [sp] is cast on boss [tar] from gauntlet [glt] in state [st]. *)
let cast_damage_spell glt st sp tar =
  let b = get_current_boss st in
  let hit = dmg_status_hit_roll glt st sp b in
  let dmg = damage_spell_dmg glt st sp b hit in
  {
    dmg = dmg;
    target = tar;
    desc = string_of_int dmg |> write_spell_desc st sp tar hit;
    new_st = get_health b st - dmg |> set_health b st
  }

(** [cast_status_spell glt st sp tar] is the cast spell data after
    StatusAilment spell [sp] is cast on boss [tar] from gauntlet [glt] in state
    [st]. *)
let cast_status_spell glt st sp tar =
  let b = get_current_boss st in
  let status = match sp.sp_eff with
    | EStatus status -> status
    | Eff _ -> failwith "not a status spell"
  in
  let hit = dmg_status_hit_roll glt st sp b in
  {
    dmg = 0;
    target = tar;
    desc = string_of_status status |> write_spell_desc st sp tar hit;
    new_st = if hit then status_add tar status st else st
  }

(** [hp300_hit_roll glt st sp] is whether an HP300Status spell [sp] hits the
    boss from gauntlet [glt] in state [st]. *)
let hp300_hit_roll glt st sp =
  let b = get_current_boss st in
  if (is_boss_resistant glt sp b = false) && (get_health b st <= 300)
  then true
  else false

(** [update_mp c sp st] is the new state with the character [c]'s MP updated
    from state [st] after casting spell [sp]. *)
let update_mp c sp st =
  let c_name = Party.get_name c in
  set_magic_points c_name (get_magic_points c_name st - sp.sp_mp) st

let cast_spell glt st sp c tar =
  let spell_data = match sp.sp_effect with
    | Damage -> cast_damage_spell glt st sp tar
    | StatusAilment -> cast_status_spell glt st sp tar
    | HPRec
    | FullHP
    | RestoreStatus
    | DefUp
    | StrUp
    | StrHitUp
    | AglUp
    | HP300Status -> failwith "Unimplemented"
  in
  let st = spell_data.new_st in
  {spell_data with
   new_st = st |> update_mp c sp |> change_turns}


(******************************************************************************
   BOSS MAGIC
 ******************************************************************************)

(** Type for the number of targets of a skill. *)
type target = All | Single

type skill = {
  sk_name : string;
  sk_eff : effectivity;
  sk_acc : int;
  sk_el : element;
  sk_effect : effect;
  sk_target : target
}

(** [skill] is a skill with name [name], effectivity [eff], accuracy [acc]
    element [el], effect [effect], and target [target]. *)
let skill name eff acc el effect target = {
  sk_name = name;
  sk_eff = eff;
  sk_acc = acc;
  sk_el = el;
  sk_effect = effect;
  sk_target = target
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

let get_skill s =
  List.find (fun x -> x.sk_name = s) skill_list

let cast_boss_spell glt sp tar st =
  failwith "Unimplemented"

let cast_boss_skill glt sp st =
  failwith "Unimplemented"