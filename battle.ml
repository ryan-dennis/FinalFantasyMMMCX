open State

(** To-do: # of hits/who takes damage/amount of damage should be available *)

let weapon st =
  match get_current_fighter st with
  | "fighter" -> 32
  | "thief" -> 19
  | "black belt" -> 0
  | "red mage" -> 32
  | "white mage" -> 12
  | "black mage" -> 12
  | boss -> 0

(** [char_stats c] is the stats of character [c]. *)
let char_stats c = Party.get_stats c

(** [cur_boss_stats st] is the stats of the current boss in [st]. *)
let cur_boss_stats glt st =
  st |> get_current_boss |> Gauntlet.boss_stats glt

(** [hit_roll hit agl] is whether the attacker with a hit % of [hit] hits a
    target with an agility of [agl]. *)
let hit_roll hit agl =
  let cth = 168 + hit - (48 + agl) in
  if Random.int 200 <= cth then true
  else false

(** [num_of_hits c hit] is the number of hits that the character [c] with a
    hit rate of [hit] gets. *)
let num_of_hits c hit =
  match c with
  | "black belt" -> 2 * (1 + (hit/32))
  | char -> 1 + (hit/32)

(** [fight_dmg st a_hit a_str d_agl d_def] is how much damage an attacker with
    a hit percent of [a_hit] and strength of [a_str] does to a defender with an
    agility of [d_agl] and defense of [d_def]. *)
let fight_dmg st a_hit a_str d_agl d_def =
  let atk = (a_str / 2) + (weapon st) in
  if hit_roll a_hit d_agl = false then 0
  else let dmg = (Random.int atk) + atk - d_def in
    if dmg <= 0 then 1
    else dmg

(** [total_hit_dmg st a_hit a_str d_agl d_def acc] is how much damage an
    attacker with a hit percent of [a_hit] and strength of [a_str] does to a
    defender with an agility of [d_agl] and defense of [d_def], over the course
    of [n] hits. *)
let rec total_hit_dmg st a_hit a_str d_agl d_def n acc =
  match n with
  | 0 -> acc
  | n -> acc + fight_dmg st a_hit a_str d_agl d_def |>
         total_hit_dmg st a_hit a_str d_agl d_def (n-1)

let fight glt st c =
  let b = get_current_boss st in
  let char = char_stats c in
  let c_name = Party.get_name c in
  let boss = cur_boss_stats glt st in
  let n = num_of_hits c_name char.hit_percent in
  let dmg = total_hit_dmg st char.hit_percent char.str boss.agl boss.def n 0 in
  let new_st = set_health b (get_health b st - dmg) st |> change_turns in
  (n, dmg, new_st)

(** [boss_target glt st] is the character in [glt] who is targeted by the
    current boss in [st]. *)
let boss_target glt st =
  let party = get_party st in
  let r = Random.int 7 in
  if r < 5 then List.nth party 0
  else if r < 7 then List.nth party 1
  else List.nth party 3

(** [rm_dead new_hp st c] is the state [st] with character [c] removed from
    the turn order if they are dead ([new_hp] <= 0). *)
let rm_dead new_hp st c =
  if new_hp > 0 then st
  else remove_from_t c st

let boss_turn glt st =
  let c = boss_target glt st in
  let char = Party.find_character c Party.get_characters |> char_stats in
  let boss = cur_boss_stats glt st in
  let n = num_of_hits c boss.hit in
  let dmg = total_hit_dmg st boss.hit boss.str char.agl char.fight_def n 0 in
  let new_hp = get_health c st - dmg in
  let new_dmged_st = set_health c new_hp st in
  let new_st = rm_dead new_hp new_dmged_st c |> change_turns in
  (n, dmg, c, new_st)