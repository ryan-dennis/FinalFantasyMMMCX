open State

let weapon st =
  let c = get_current_fighter st in
  if c = "fighter" then 100 (**32*)
  else if c = "thief" then 75 (**19*)
  else if c = "black belt" then 100 (**30*)
  else if c = "red mage" then 100 (**32*)
  else if c = "white mage" then 50 (**12*)
  else if c = "black mage" then 50 (**12*)
  else failwith "nonexistent character"

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
let fight_dmg st a_hit a_str d_agl d_def =
  let atk = (a_str / 2) + (weapon st) in
  if hit_roll a_hit d_agl = false then 0
  else let dmg = (Random.int atk) + atk - d_def in
    if dmg <= 0 then 1
    else dmg

let fight glt st c =
  let b = get_current_boss st in
  let char = char_stats c in
  let boss = cur_boss_stats glt st in
  set_health b (get_health b st -
                fight_dmg st char.hit_percent char.str boss.agl boss.def) st |>
  change_turns

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
  let new_hp = get_health c st - fight_dmg st boss.hit boss.str
                 char.agl char.fight_def in
  let new_st = set_health c new_hp st in
  if new_hp > 0 then new_st |> change_turns
  else remove_from_t c new_st |> change_turns