(** Notes:
    - Implement Blinded (attacker's base chance to hit drops by 40%, and boss's
      cth against that specific character increases by 40%)
*)

open State
open Spells

type t = {
  hits : int;
  dmg : int;
  target : string;
  desc : string;
  new_st : State.t
}

(******************************************************************************
   PLAYER CHARACTER TURN
 ******************************************************************************)

(** [write_fight_desc st target hits dmg] is the description of the last turn,
    if the last turn was a fight attack. *)
let write_fight_desc st target hits dmg =
  String.concat " " [get_current_fighter st; "attacked"; target;
                     string_of_int hits; "times for"; string_of_int dmg;
                     "damage!"]

(** [get_atk atker st] is the attack rating of [atker] in [st]. *)
let get_atk atker st =
  let boss = get_current_boss st in
  let str_bonus = get_strength atker st / 2 in
  let weapon = if atker = boss then 0 else
      match atker with
      | "fighter" -> 45
      | "thief" -> 33
      | "black belt" -> 80 - str_bonus
      | "red mage" -> 32
      | "white mage" -> 18
      | "black mage" -> 22
      | c -> failwith "invalid character" in
  str_bonus + weapon

(** [hit_roll st atker tar] is whether the attacker [atker] hits the target
    [tar] in the state [st]. *)
let hit_roll st atker tar =
  let base_cth = if is_blinded atker st then 148
    else if is_blinded tar st then 208
    else 168 in
  let cth = base_cth + (get_hit_per atker st) - (48 + (get_agil tar st)) in
  if Random.int 200 <= cth then true
  else false

(** [num_of_hits glt atker st] is the number of hits that the attacker [atker]
    in state [st] gets. *)
let num_of_hits glt atker st =
  let boss = get_current_boss st in
  let base_hit = 1 + ((get_hit_per atker st) / 32) in
  if atker = boss then Gauntlet.boss_num_of_hits glt boss
  else if atker = "black belt" then 2 * base_hit
  else match atker with
    | "black belt" -> 2 * base_hit
    | char -> base_hit

(** [fight_dmg st a_hit d_agl d_def atk] is how much damage the attacker
    [atker] deals to target [tar] in state [st]. *)
let fight_dmg st atker tar =
  let atk = get_atk atker st in
  if hit_roll st atker tar = false then 0
  else let dmg = (Random.int atk) + atk - (get_fight_def tar st) in
    if dmg <= 0 then 1
    else dmg

(** [total_hit_dmg st atker tar n dmg] is how much damage an attacker [atker]
    does to a target [tar] in state [st] over the course of [hits] hits. *)
let rec total_hit_dmg st atker tar hits dmg =
  if hits = 0 then dmg
  else (dmg + fight_dmg st atker tar) |>
       total_hit_dmg st atker tar (hits-1)

(** [fight_attack glt st tar] is the new state after the current fighter
    executes their fight attack on [tar] in [st]. *)
let fight_attack glt st tar =
  let atker = get_current_fighter st in
  let hits = num_of_hits glt atker st in
  let dmg = total_hit_dmg st atker tar hits 0 in
  let new_st = get_health tar st - dmg |> set_health glt tar st in
  {hits = hits;
   dmg = dmg;
   target = tar;
   desc = write_fight_desc st tar hits dmg;
   new_st = new_st}

let fight glt st c =
  let data = get_current_boss st |> fight_attack glt st in
  {data with new_st = change_turns glt data.new_st}

(** [magic_to_battle_data spell_data] is the magic data [t] in the form of
    battle data. *)
let magic_to_battle_data t =
  {
    hits = 1;
    dmg = t.m_dmg;
    target = t.m_target;
    desc = t.m_desc;
    new_st = t.m_new_st
  }

let magic glt st s c tar =
  let data = cast_spell glt st (get_spell s) c tar |> magic_to_battle_data in
  {data with new_st = change_turns glt data.new_st}

(******************************************************************************
   BOSS AI
 ******************************************************************************)

type boss_attack =
  | Spell of spell
  | Skill of skill
  | Fight
  | Status of Status.t

(** [boss_spell_num] is what spell in the list the boss will cast. *)
let boss_spell_num = ref 0

(** [boss_skill_num] is what skill in the list the boss will cast. *)
let boss_skill_num = ref 0

(** [boss_spell glt b] is the next spell for boss [b] in [glt]. *)
let boss_spell glt b =
  let spells = Gauntlet.boss_spells glt b in
  let sp = !boss_spell_num mod (List.length spells) |>
           List.nth spells |>
           get_spell in
  boss_spell_num := !boss_spell_num + 1; Spell sp

(** [boss_skill glt b] is the next skill for boss [b] in [glt]. *)
let boss_skill glt b =
  let skills = Gauntlet.boss_skills glt b in
  let sk = !boss_skill_num mod (List.length skills) |>
           List.nth skills |>
           get_skill in
  boss_skill_num := !boss_skill_num + 1; Skill sk

(** [boss_action glt st] is the boss_attack that the boss chooses on its next
    turn. *)
let boss_action glt st =
  let b = get_current_boss st in
  if is_paralyzed b st then Status Paralyzed
  else if is_silenced b st then Status Silenced
  else if (Random.int 128) + 1 <= Gauntlet.boss_spell_chance glt b
  then boss_spell glt b
  else if (Random.int 128) + 1 <= Gauntlet.boss_skill_chance glt b
  then boss_skill glt b
  else Fight

(** [boss_target glt st] is the character in [glt] who is targeted by the
    current boss in [st]. If the character chosen is dead, then a new target
    will be chosen until a valid target is selected. *)
let rec boss_target glt st =
  let party = get_party st in
  let r = Random.int 7 in
  let target = 
    if r < 4 then List.nth party 0
    else if r < 6 then List.nth party 1
    else List.nth party 2 in
  if is_dead st target then boss_target glt st
  else target

(** [rm_dead new_hp st] is the state [st] with characters in [party] removed
    from the turn order if they are dead. *)
let rec rm_dead party st =
  match party with
  | [] -> st
  | h::t -> if is_dead st h then remove_from_t h st |> rm_dead t
    else rm_dead t st

let boss_turn glt st =
  let tar = boss_target glt st in
  let new_data = match boss_action glt st with
    | Spell sp -> cast_boss_spell glt sp tar st |> magic_to_battle_data
    | Skill sk -> cast_boss_skill glt sk tar st |> magic_to_battle_data
    | Fight -> fight_attack glt st tar
    | Status status ->
      match status with
      | Paralyzed -> {
          hits = 0;
          dmg = 0;
          target = tar;
          desc = get_current_boss st ^ "is paralyzed and cannot attack!";
          new_st = empty_state
        }
      | Silenced -> fight_attack glt st tar
      | _ -> failwith "not a valid boss attack"
  in
  {new_data with new_st = new_data.new_st |>
                          rm_dead (get_party st) |>
                          change_turns glt}

let num_hits b =
  b.hits

let dmg b = 
  b.dmg

let target b =
  b.target

let desc b =
  b.desc

let new_st b =
  b.new_st