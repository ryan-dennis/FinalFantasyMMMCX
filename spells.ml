open State
open Gauntlet
open Status

type t = {
  m_dmg : int;
  m_target : string;
  m_desc : string;
  m_new_st : State.t
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
    Status (StatusAilment, HP300 Status): if it hits, inflicts the status
    ailment of EStatus
    Heal (HPRec, FullHP): always hits, heals a player character and/or removes
    all statuses
    RestoreStatus (RestoreStatus): removes the status EStatus
    Support (DefUp, StrUp, StrHitUp, AglUp): always hits, stat buffs that are
    applied to a player character
*)
type effect =
  | Damage
  | StatusAilment
  | HP300Status
  | HPRec
  | FullHP
  | RestoreStatus
  | DefUp
  | StrUp
  | StrHitUp
  | AglUp

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

let get_spell s =
  List.find (fun x -> x.sp_name = s) spell_list

(** [is_friendly sp] is whether the spell [sp] is a friendly spell (used on
    teammates). *)
let is_friendly sp =
  match sp.sp_effect with
  | HPRec | FullHP | RestoreStatus | DefUp | StrUp | StrHitUp | AglUp -> true
  | _ -> false

(** [get_char_stats tar] is the stats of the character with the name [tar]. *)
let get_char_stats tar =
  Party.get_characters |>
  Party.find_character tar |>
  Party.get_stats

(**[is_boss st tar] is whether the target [tar] in state [st] is a boss. *)
let is_boss st tar =
  get_current_boss st = tar

let is_valid_target st sp tar =
  let friendly = is_friendly sp in
  let atker = get_current_fighter st in
  if is_boss st atker then true
  else
    try ignore(Party.find_character tar Party.get_characters);
      if friendly then true else false
    with Party.UnknownCharacter _ ->
      (is_boss st tar) && friendly = false

(** [write_spell_desc st tar hit effect] is the description of the last turn,
    if the last turn a player character cast a spell. *)
let write_spell_desc st sp tar hit effect =
  let cur_fighter = get_current_fighter st in
  let spell_desc = match sp.sp_effect with
    | Damage ->
      let dmg_desc = match sp.sp_el with
        | None -> ["dealt"; effect; "damage!"]
        | _ -> ["dealt"; effect; string_of_el sp.sp_el; "damage!"] in
      dmg_desc
    | StatusAilment -> ["inflicted" ; effect ^ "!"]
    | HP300Status -> ["inflicted"; effect ^ "!"]
    | HPRec -> ["healed them for"; effect; "HP!"]
    | FullHP -> ["healed them completely!"]
    | RestoreStatus -> ["cured their"; effect ^ "!"]
    | DefUp -> ["raised their defense!"]
    | StrUp -> ["raised their strength!"]
    | StrHitUp -> ["raised their strength and hit rate!"]
    | AglUp -> ["raised their agility!"]
  in
  let tar = if cur_fighter = tar then "themself" else tar in
  if hit = false
  then String.concat " " [cur_fighter; "tried to cast"; sp.sp_name; "on"; tar;
                          "but it failed!"]
  else [cur_fighter; "cast"; sp.sp_name; "on"; tar; "and"] @ spell_desc |>
       String.concat " "

(** [is_resistant glt st sp tar] is whether the target [tar] in state [st] is
    resistant to the element of spell [sp]. *)
let is_resistant glt st sp tar =
  if is_boss st tar
  then List.exists (fun x -> x = string_of_el(sp.sp_el))
      (Gauntlet.boss_stats glt tar).resist
  else false

(** [is_weak glt st sp tar] is whether the target [tar] in state [st] is weak
    to the element of spell [sp]. *)
let is_weak glt st sp tar =
  if is_boss st tar
  then List.exists (fun x -> x = string_of_el(sp.sp_el))
      (Gauntlet.boss_stats glt tar).weak
  else false

(** [dmg_status_hit_roll glt st sp b] is whether a Damage or StatusAilment spell
    [sp] hits the target [tar] from gauntlet [glt] in state [st]. If the spell
    has the StatusAilment effect, then it hits; if the spell has the Damage
    effect, then the damage dealt will be doubled. *)
let dmg_status_hit_roll glt st sp tar =
  let weak = is_weak glt st sp tar in
  let res = is_resistant glt st sp tar in
  let base_cth = if weak && res then 40
    else if weak then 188
    else if res then 0
    else 148 in
  let mdef = if is_boss st tar then (boss_stats glt tar).mdef
    else (get_char_stats tar).m_def in
  let cth =
    base_cth + sp.sp_acc - mdef in
  if Random.int 200 <= cth then true
  else false

(** [damage_spell_dmg glt st sp tar doubled] is the amount of damage the Damage
    spell [sp] cast on the target [tar] in state [st] deals. [doubled] is
    whether the damage is doubled or not. *)
let damage_spell_dmg glt st sp tar doubled =
  let weak = is_weak glt st sp tar in
  let res = is_resistant glt st sp tar in
  let eff = match sp.sp_eff with
    | Eff e -> if res then e / 2 else if weak then e * 3 / 2 else e
    | EStatus _ -> failwith "Not a damage spell" in
  let res_multiplier = if doubled then 2 else 1 in
  ((Random.int eff) + eff) * res_multiplier

(** [cast_damage_spell glt sp c tar st] is the cast spell data after Damage
    spell [sp] is cast on boss [tar] from gauntlet [glt] in state [st]. Damage
    spells will always hit. *)
let cast_damage_spell glt st sp tar =
  let doubled = dmg_status_hit_roll glt st sp tar in
  let dmg = damage_spell_dmg glt st sp tar doubled in
  let new_st = get_health tar st - dmg |> set_health glt tar st in
  {
    m_dmg = dmg;
    m_target = tar;
    m_desc = string_of_int dmg |> write_spell_desc st sp tar true;
    m_new_st = new_st
  }

(** [hp300_hit_roll glt st sp tar] is whether an HP300Status spell [sp] hits the
    target [tar] from gauntlet [glt] in state [st]. *)
let hp300_hit_roll glt st sp tar =
  if (is_resistant glt st sp tar = false) && (get_health tar st <= 300)
  then true else false

(** [cast_status_spell glt st sp tar hit] is the cast spell data after
    a status spell [sp] is cast on boss [tar] from gauntlet [glt] in state
    [st]. *)
let cast_status_spell glt st sp tar =
  let hit = match sp.sp_effect with
    | StatusAilment -> dmg_status_hit_roll glt st sp tar
    | HP300Status -> hp300_hit_roll glt st sp tar
    | _ -> failwith "not a status spell"
  in
  let status = match sp.sp_eff with
    | EStatus status -> status
    | Eff _ -> failwith "not a status spell"
  in
  {
    m_dmg = 0;
    m_target = tar;
    m_desc = string_of_status status |> write_spell_desc st sp tar hit;
    m_new_st = if hit then status_add tar status st else st
  }

(** [hp_healed] is the amount of HP healed by a HPRec spell [sp]. *)
let hp_healed st sp tar =
  let eff = match sp.sp_eff with
    | Eff e -> e
    | EStatus _ -> failwith "not a heal spell" in
  (Random.int eff) + eff

(** [cast_heal_spell st sp tar] is the cast spell data after a heal spell [sp]
    is cast on a target [tar] in state [st]. *)
let cast_heal_spell glt st sp tar =
  let old_hp = get_health tar st in
  let new_st = match sp.sp_effect with
    | HPRec -> old_hp + hp_healed st sp tar |>
               set_health glt tar st
    | FullHP -> cure4_status tar st
    | _ -> failwith "not a heal spell"
  in
  let hp_rec = get_health tar new_st - old_hp in
  {
    m_dmg = hp_rec;
    m_target = tar;
    m_desc = string_of_int hp_rec |> write_spell_desc st sp tar true;
    m_new_st = new_st
  }

(** [cast_restore_status_spell st sp tar] is the cast spell data after
    RestoreStatus spell [sp] is cast on player character [tar] in state
    [st]. *)
let cast_restore_status_spell st sp tar =
  let status = match sp.sp_eff with
    | EStatus status -> status
    | Eff e -> failwith "not a restore status spell"
  in
  {
    m_dmg = 0;
    m_target = tar;
    m_desc = string_of_status status |> write_spell_desc st sp tar true;
    m_new_st = status_remove tar status st
  }

(** [cast_support_spell st sp tar] is the cast spell data after a support
    spell [sp] is cast on a player character [tar] in state [st]. *)
let cast_support_spell st sp tar =
  let eff = match sp.sp_eff with
    | Eff e -> e
    | EStatus _ -> failwith "not a support spell" in
  {
    m_dmg = 0;
    m_target = tar;
    m_desc = write_spell_desc st sp tar true "";
    m_new_st = match sp.sp_effect with
      | DefUp -> get_fight_def tar st + eff |> set_fight_def tar st
      | StrUp -> get_strength tar st + eff |> set_strength tar st
      | StrHitUp -> get_hit_per tar st + sp.sp_acc |> set_hit_percent tar
                      (get_strength tar st + eff |> set_strength tar st)
      | AglUp -> get_agil tar st + eff |> set_agil tar st
      | _ -> failwith "not a support spell"
  }

(** [update_mp c sp st] is the new state with the character [c]'s MP updated
    from state [st] after casting spell [sp]. *)
let update_mp c sp st =
  let c_name = Party.get_name c in
  set_magic_points c_name (get_magic_points c_name st - sp.sp_mp) st

let is_enough_mp sp c st =
  sp.sp_mp <= get_magic_points (Party.get_name c) st

let cast_spell glt st sp c tar =
  let spell_data = match sp.sp_effect with
    | Damage -> cast_damage_spell glt st sp tar
    | StatusAilment | HP300Status -> cast_status_spell glt st sp tar
    | HPRec | FullHP -> cast_heal_spell glt st sp tar
    | RestoreStatus -> cast_restore_status_spell st sp tar
    | DefUp | StrUp | StrHitUp | AglUp -> cast_support_spell st sp tar
  in
  let st = spell_data.m_new_st in
  {spell_data with
   m_new_st = st |> update_mp c sp}


(******************************************************************************
   BOSS MAGIC
 ******************************************************************************)

(** Type for the number of targets of a skill. *)
type target = All | Single

type skill = {
  sk_spell : spell;
  sk_target : target
}

(** [skill] is a skill with name [name], effectivity [eff], accuracy [acc]
    element [el], effect [effect], and target [target]. *)
let skill name eff acc el effect target = {
  sk_spell = spell name eff acc el effect 0;
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
  List.find (fun x -> x.sk_spell.sp_name = s) skill_list

let cast_boss_spell glt sp tar st =
  let spell_data = match sp.sp_effect with
    | Damage -> cast_damage_spell glt st sp tar
    | StatusAilment | HP300Status -> cast_status_spell glt st sp tar
    | HPRec | FullHP -> cast_heal_spell glt st sp tar
    | RestoreStatus -> cast_restore_status_spell st sp tar
    | DefUp | StrUp | StrHitUp | AglUp -> cast_support_spell st sp tar
  in
  {spell_data with m_new_st = spell_data.m_new_st}

(** [get_st t] is the new state after a spell/skill is cast. *)
let get_st t =
  t.m_new_st

(** [atk_all glt sp party st acc] is the new spell data after the spell [sp] is
    cast on every member of [party] by the current boss from gauntlet [glt]. *)
let rec atk_all glt sp party st acc =
  match party with
  | [] -> acc
  | h::t -> let spell_data = cast_boss_spell glt sp h st in
    atk_all glt sp t (get_st spell_data) spell_data

(** [t_init st] is an empty record of type t. *)
let t_init st = {
  m_dmg = 0;
  m_target = "";
  m_desc = "";
  m_new_st = st
}

let cast_boss_skill glt sk tar st =
  let party = get_party st in
  let sp = sk.sk_spell in
  let spell_data = match sk.sk_target with
    | All -> t_init st |> atk_all glt sp party st 
    | Single -> cast_boss_spell glt sp tar st
  in
  let b = get_current_boss st in
  let desc = match sk.sk_target with
    | All -> String.concat " " [b; "cast"; sp.sp_name; "on the party!"]
    | Single -> spell_data.m_desc
  in
  {
    m_dmg = 0;
    m_target = tar;
    m_desc = desc;
    m_new_st = spell_data.m_new_st
  }