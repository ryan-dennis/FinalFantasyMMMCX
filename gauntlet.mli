(** The abstract type of values representing gauntlets. *)
type t

(** The type of boss. *)
type boss_id = string

(** The type of sprite. *)
type sprite = string list

(** The type of boss stats. *)
type stats = {
  hp : int;
  ag : int;
  def : int;
  str : int;
  hit : int;
  weak : string list;
  resist : string list
}

(** Raised when an unknown boss is encountered. *)
exception UnknownBoss of boss_id

(** Raised when an invalid sprite is encountered. *)
exception InvalidSprite of sprite

(** [from_json j] is the gauntlet that [j] represents. *)
val from_json : Yojson.Basic.t -> t

(** [start_boss g] is the identifier of the starting boss in gauntlet
    [g]. *)
val start_boss : t -> boss_id

(** [start_dlg g] is the opening dialogue in gauntlet [g]. *)
val start_dialogue : t -> string

(** [boss_stats g b] is the stats of boss [b] in gauntlet [g]. *)
val boss_stats : t -> boss_id -> stats

(** [boss_sprite g b] is the sprite of boss [b] in gauntlet [g]. *)
val boss_sprite : t -> boss_id -> string list

(** [boss_spells g b] is the list of spells of boss [b] in gauntlet [g]. *)
val boss_spells : t -> boss_id -> string list

(** [next g b] is the next boss after [b] in gauntlet [g]. *)
val next : t -> boss_id -> boss_id

(** [get_dlg g b] is the victory dialogue for boss [b] in gauntlet [g]. *)
val dialogue : t -> boss_id -> string