open State

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

(** [fight_dmg a_hit a_str d_agl d_def] is how much damage an attacker with a
    hit percent of [a_hit] and strength of [a_str] does to a defender with an
    agility of [d_agl] and defense of [d_def]. *)
let fight_dmg a_hit a_str d_agl d_def =
  if hit_roll a_hit d_agl = false then 0
  else let atk = a_str / 2 in (Random.int atk) + atk - d_def

let fight glt st c =
  let b = get_current_boss st in
  let char = char_stats c in
  let boss = cur_boss_stats glt st in
  set_health b (get_health b st -
                fight_dmg char.hit_percent char.str boss.agl boss.def) st

(** [boss_target glt st] is the character in [glt] who is targeted by the
    current boss in [st]. *)
let boss_target glt st =
  let party = get_party st in
  let r = Random.int 7 in
  if r < 5 then List.nth party 0
  else if r < 7 then List.nth party 1
  else List.nth party 3

let boss_turn glt st =
  let c = boss_target glt st in
  let char = Party.find_character c Party.get_characters |> char_stats in
  let boss = cur_boss_stats glt st in
  let new_st = set_health c (get_health c st - fight_dmg boss.hit boss.str
                               char.agl char.fight_def) in
  if get_health c new_st > 0 then new_st
  else remove_from_t c new_st