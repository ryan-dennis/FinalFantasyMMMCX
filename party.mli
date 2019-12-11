(** Contains the characters of the game including their stats, spells, and 
    weapons. Also include the functions for adding 3 charcters
    to a party. *)

(** The abtract type representing a charcacter. *)
type t

(** Represents the type of the charcater's spells.*)
type spell = string

(** Represents the type of the charcater's stats *)
type stat = {
  str : int; 
  agl : int; 
  int : int;
  vit : int; 
  mp : int;
  hit_percent : int; 
  m_def : int;
  fight_def : int;  
}

(** Represents the type of the characters sprite to be displeys
    (string list list)*)
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

(** [find_character n lst] is the character with name [n] in [lst]. 
    Raises UnknownCharacter if [n] is not a valid name of a character *)
val find_character: string -> t list -> t

(** [add lst acc] is the character list [acc] of the charcaters with names in 
    [lst]. Raises UnknownCharcter if the name in the [lst] is not a valid
    charcter name *)
val add : string list -> t list -> t list

(** [char_og_health t] returns the health of [t] *)
val char_og_health : t -> int 

(** [has_spell t spell] returns [true] if [t] has [spell] and [false] otherwise *)
val has_spell: t -> spell -> bool 

(** [get_test c] is the colored sprite of the character [c] *)
val get_test: t -> (string list * string list) list

(** [skull] is the colored sprite representing when a character is at 0 vitality *)
val skull : (string list * string list) list