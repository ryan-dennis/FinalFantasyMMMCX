(** The abstract type of values representing gauntlets. *)
type t

(** The type of boss. *)
type boss_id = string

(** The type of boss stats. *)
type stats

(** The type of magic element. *)
type element

(** Raised when an unknown boss is encountered. *)
exception UnknownBoss of boss_id

(** Raised when an unknown magic element is encountered. *)
exception UnknownElement of string

(** [from_json json] is the gauntlet that [json] represents. *)
val from_json : Yojson.Basic.t -> t

(** [start_battle glt] is the identifier of the starting battle in gauntlet
    [glt]. *)
val start_battle : t -> boss_id

val next_battle : t -> boss_id -> boss_id

val get_desc : t -> boss_id -> string

val win_msg : t -> string

val get_boss_name : t -> boss_id -> string

val get_boss_stats : t -> boss_id -> stats

val get_boss_sprite : t -> boss_id -> string list list

val get_boss_spells : t -> boss_id -> (string * int) list