open Yojson.Basic.Util

type boss_id = string

type element = Fire | Ice | Lightning | Poison | Status

exception Unknown_Boss of boss_id
exception Unknown_Element of string

type stats = {
  hp : int;
  ag : int;
  def : int;
  str : int;
  hit : int;
  weak : element list;
  resist : element list
}

(** The type of boss. *)
type boss = {
  id : boss_id;
  stats : stats;
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
  battles : boss list
}

(** [to_element e] is the element of a boss's weaknesses or resistances as
    type element. *)
let to_element = function
  | "fire" -> Fire
  | "ice" -> Ice
  | "lightning" -> Lightning
  | "poison" -> Poison
  | "status" -> Status
  | el -> raise (Unknown_Element el)

(** [start_of_json j] is the starting information in the gauntlet [j]
    represents. *)
let start_of_json j = {
  start_id = j |> member "id" |> to_string;
  start_dlg = j |> member "dialogue" |> to_string
}

(** [element_of_json j] is a magic element in the gauntlet [j] represents. *)
let element_of_json j =
  j |> member "magic" |> to_string |> to_element

(** [stats_of_json j] is the stats of a boss in the gauntlet [j] represents. *)
let stats_of_json j = {
  hp = j |> member "hp" |> to_int;
  ag = j |> member "ag" |> to_int;
  def = j |> member "def" |> to_int;
  str = j |> member "str" |> to_int;
  hit = j |> member "hit" |> to_int;
  weak = j |> member "weak" |> to_list |> List.map element_of_json;
  resist = j |> member "resist" |> to_list |> List.map element_of_json;
}

(** [boss_of_json j] is a boss in the gauntlet [j] represents. *)
let boss_of_json j = {
  id = j |> member "id" |> to_string;
  stats = j |> member "stats" |> stats_of_json;
  boss_dlg = j |> member "dialogue" |> to_string;
  next = j |> member "next" |> to_string
}

let from_json json = {
  start = json |> member "start" |> start_of_json;
  battles = json |> member "battles" |> to_list |> List.map boss_of_json
}


let start_battle glt =
  match glt with
  | [] -> failwith "empty gauntlet"
  | h::t -> h.id

let next_battle glt b =
  failwith ""