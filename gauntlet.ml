open Yojson.Basic.Util

type boss_id = string
type sprite = string list

exception UnknownBoss of boss_id

exception InvalidSprite of sprite

type stats = {
  hp : int;
  agl : int;
  def : int;
  mdef : int;
  str : int;
  hit : int;
  hits_per : int;
  weak : string list;
  resist : string list
}

(** The type of magic. *)
type magic = {
  spell_chance : int;
  spell_list : string list
}

(** The type of special. *)
type special = {
  skill_chance : int;
  skill_list : string list
}

(** The type of boss. *)
type boss = {
  id : boss_id;
  stats : stats;
  sprite : string list;
  magic : magic;
  special : special;
  boss_dlg : string;
  next : string
}

(** The type of starting information. *)
type start = {
  start_id : boss_id;
  start_dlg : string
}

type t = {
  start : start;
  final : boss_id;
  bosses : boss list
}

(** [start_of_json j] is the starting information in the gauntlet [j]
    represents. *)
let start_of_json j = {
  start_id = j |> member "id" |> to_string;
  start_dlg = j |> member "dialogue" |> to_string
}

(** [stats_of_json j] is the stats of a boss in the gauntlet [j] represents. *)
let stats_of_json j = {
  hp = j |> member "hp" |> to_int;
  agl = j |> member "agility" |> to_int;
  def = j |> member "defense" |> to_int;
  mdef = j |> member "magic defense" |> to_int;
  str = j |> member "strength" |> to_int;
  hit = j |> member "hit rate" |> to_int;
  hits_per = j |> member "hits per turn" |> to_int;
  weak = j |> member "weaknesses" |> to_list |> List.map to_string;
  resist = j |> member "resistances" |> to_list |> List.map to_string;
}

(** [magic_of_json j] is the magic abilities of a boss in the gauntlet [j]
    represents. *)
let magic_of_json j = {
  spell_chance = j |> member "spell chance" |> to_int;
  spell_list = j |> member "spell list" |> to_list |> List.map to_string
}

(** [special_of_json j] is the special abilities of a boss in the gauntlet [j]
    represents. *)
let special_of_json j = {
  skill_chance = j |> member "skill chance" |> to_int;
  skill_list = j |> member "skill list" |> to_list |> List.map to_string
}

(** [check_sprite sprite] is [sprite] if it is a valid sprite (each line is
    30 characters) and raises InvalidSprite otherwise. *)
let check_sprite sprite =
  let rec valid = function
    | [] -> true
    | h::t -> if String.length h = 30 then valid t
      else false
  in
  if valid sprite then sprite
  else raise (InvalidSprite sprite)

(** [boss_of_json j] is a boss in the gauntlet [j] represents. *)
let boss_of_json j = {
  id = j |> member "id" |> to_string;
  stats = j |> member "stats" |> stats_of_json;
  sprite = j |> member "sprite" |> to_list |> List.map to_string;
  magic = j |> member "magic" |> magic_of_json;
  special = j |> member "special" |> special_of_json;
  boss_dlg = j |> member "dialogue" |> to_string;
  next = j |> member "next boss" |> to_string
}

let from_json json = {
  start = json |> member "start" |> start_of_json;
  final = json |> member "final" |> to_string;
  bosses = json |> member "bosses" |> to_list |> List.map boss_of_json
}

let start_boss glt =
  glt.start.start_id

let start_dialogue glt =
  glt.start.start_dlg

let final glt =
  glt.final

(** [find_boss glt b] is the boss in [glt] with the identifier [b]. *)
let find_boss glt b =
  match List.filter (fun x -> x.id = b) glt.bosses with
  | [] -> raise (UnknownBoss b)
  | h::t -> h

let boss_stats glt b =
  (find_boss glt b).stats

let boss_sprite glt b =
  (find_boss glt b).sprite

let boss_spell_chance glt b =
  (find_boss glt b).magic.spell_chance

let boss_spells glt b =
  (find_boss glt b).magic.spell_list

let boss_skill_chance glt b =
  (find_boss glt b).special.skill_chance

let boss_skills glt b =
  (find_boss glt b).special.skill_list

let next glt b =
  (find_boss glt b).next

let dialogue glt b =
  (find_boss glt b).boss_dlg
