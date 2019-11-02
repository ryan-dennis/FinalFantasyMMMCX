(** Contains the characters of the game including their stats, spells, and 
    weapons. Also include the functions for adding 3 charcters
    to a party. *)

(** The abtract type representing a charcacter. (record) *)
type t

(** Represents the type of the charcater's spells.
    Each character gets up to 3 *)
type spell = string

(** Represents the type of the charcater's stats (record)*)
type stat

(** Represents the type of the characters sprite to be displeys (string list list)*)
type sprite = string list

(** Exception of an unknown character name  *)
exception UnknownCharacter of string 

(** [get_spells t] is the list of spells of [t] *)
val get_spells : t -> spell list  

(** [get_stats t] is the list of stats of charcater [t] *)
val get_stats : t -> stat

(** [get_characters] is the character list of all possible characters *)
val get_characters : t list 

(** [get_weapon] is the weapon name the characater [t] has *)
val get_weapon: t -> string

(** [get_sprite t] is the sprite of character [t]*)
val get_sprite: t -> sprite 

(** [get_name t] is the name of [t] *)
val get_name: t -> string 

(** [add lst acc] is the character list [acc] of the charcaters with names in 
    [lst]. Raises UnknownCharcter if the name in the [lst] is not a valid
    charcter name *)
val add : string list -> t list -> t list



