open State

type t = {
  hits : int;
  dmg : int;
  target : string;
  new_st : State.t
}

(** [char_atk st] is the attack rating of the current fighter in [st]. *)
let char_atk st =
  let c = get_current_fighter st in
  let char = Party.find_character c Party.get_characters in
  let str_bonus = (Party.get_stats char).str / 2 in
  let weapon = match c with
    | "fighter" -> 32
    | "thief" -> 19
    | "black belt" -> 100 - str_bonus
    | "red mage" -> 32
    | "white mage" -> 12
    | "black mage" -> 12
    | c -> failwith "invalid character" in
  str_bonus + weapon

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

(** [fight_dmg st a_hit d_agl d_def atk] is how much damage an attacker with
    a hit percent of [a_hit] and base attack of [atk] does to a defender with
    an agility of [d_agl] and defense of [d_def]. *)
let fight_dmg st (char : Party.stat) (boss : Gauntlet.stats) atk =
  if hit_roll char.hit_percent boss.agl = false then 0
  else let dmg = (Random.int atk) + atk - boss.def in
    if dmg <= 0 then 1
    else dmg

(** [total_hit_dmg st a_hit d_agl d_def n atk acc] is how much damage a
    character with a hit percent of [a_hit] and base attack of [atk] does to a
    boss with an agility of [d_agl] and defense of [d_def], over the course
    of [n] hits. *)
let rec total_hit_dmg st char boss n atk acc =
  match n with
  | 0 -> acc
  | n -> acc + fight_dmg st char boss atk |>
         total_hit_dmg st char boss (n-1) atk

let fight glt st c =
  let b = get_current_boss st in
  let char = char_stats c in
  let c_name = Party.get_name c in
  let boss = cur_boss_stats glt st in
  let n = num_of_hits c_name char.hit_percent in
  let dmg = total_hit_dmg st char boss n (char_atk st) 0 in
  let new_st = set_health b (get_health b st - dmg) st |> change_turns in
  {hits = n; dmg = dmg; target = b; new_st = new_st}

(** [boss_hit_roll hit agl] is whether the boss with a hit % of [hit] hits
    a character with an agility of [agl]. *)
let boss_hit_roll hit agl =
  let cth = 168 + hit - agl in
  if Random.int 200 <= cth then true
  else false

(** [boss_fight_dmg st a_hit d_agl d_def atk] is how much damage a boss
    [boss] in [st] with a base attack of [atk] can do to [char]. *)
let boss_fight_dmg st (boss : Gauntlet.stats) (char : Party.stat) atk =
  if boss_hit_roll boss.hit char.agl = false then 0
  else let dmg = (Random.int atk) + atk - char.fight_def in
    if dmg <= 0 then 1
    else dmg

(** [total_boss_hit_dmg st a_hit d_agl d_def n atk acc] is how much damage
    a boss [boss] in [st] with a base attack of [atk] can do to [char] over
    the course of [n] hits. *)
let rec total_boss_hit_dmg st boss char n atk acc =
  match n with
  | 0 -> acc
  | n -> acc + boss_fight_dmg st boss char atk |>
         total_boss_hit_dmg st boss char (n-1) atk

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
  let dmg = total_boss_hit_dmg st boss char n boss.str 0 in
  let dmged_hp = get_health c st - dmg in
  let new_hp = if dmged_hp > 0 then dmged_hp else 0 in
  let new_dmged_st = set_health c new_hp st in
  let new_st = rm_dead new_hp new_dmged_st c |> change_turns in
  {hits = n; dmg = dmg; target = c; new_st = new_st}

let num_hits b =
  b.hits

let dmg b = 
  b.dmg

let target b =
  b.target

let new_st b =
  b.new_st