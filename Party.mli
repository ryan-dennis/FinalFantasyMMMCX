(** Contains the characters of the game including their stats, spells, and 
    weapons. Also include the functions for adding 3 charcters
    to a party. *)

(** The abtract type representing a charcacter. (record) *)
type t 

(** Represents the type of the charcater's spells.
    Each character gets up to 3 (magic character 3 else 0)(variant) *)
type spell 

(** Represents the type of the charcater's stats (record)*)
type stats

(** Represents the type of the characters sprite to be displeys (string list list)*)
type sprite 

(** [get_spells t] is the list of spells of [t] *)
val get_spells : t -> spell list  

(** [get_stats t] is the list of stats of charcater [t] *)
val get_stats : t -> stats list

(** [get_characters] is the character list of all possible characters *)
val get_characters : t list 

(** [get_weapon] is the weapon name the characater [t] has *)
val get_weapon: t -> string

(** [get_sprite t] is the sprite of character [t]*)
val get_sprite: t -> sprite 

(** [add lst] if the character list of the charcaters with names in 
    [lst] *)
val add : string list -> t list



