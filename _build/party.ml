(** Party.ml contains the hardcoded charcaters; getter methods for their stats,
    weapons, spells, and sprite. As well as a method to get all characters and adding 
    for adding into a party. *)

type spell = string 

type sprite = (string list) list 

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

type character = {
  name : string; 
  stats : stat; 
  weapon: string; 
  spells : spell list; 
  sprite : sprite;
}

type t = character

exception UnknownCharacter of string

(** [get_spells c] is the spell list of characater[c] *)
let get_spells {name;stats;weapon;spells;sprite} = spells

(** [get_stats c] is the stats of character [c] *)
let get_stats {name;stats;weapon;spells;sprite} = stats

(** [get_weapon c] is the weapon of character [c] *)
let get_weapon {name;stats;weapon;spells;sprite} = weapon 

(** [get_sprite c] is the sprite of character [c] *)
let get_sprite {name;stats;weapon;spells;sprite} = sprite

(** Charcter fighter *)
let fighter = {
  name = "fighter"; 
  stats = {str = 20; agl = 5; int = 1; vit = 45; mp = 0; hit_percent = 10; m_def = 15; fight_def = 20};
  weapon = "Sun sword";
  spells = [];
  sprite = [[]];
}

(** Charcter Thief *)
let thief = {
  name = "thief"; 
  stats = {str = 5; agl = 10; int = 5; vit = 35; mp = 15; hit_percent = 5; m_def = 15; fight_def=15};
  weapon = "Dragon sword";
  spells = ["FIRE"; "ICE"];
  sprite = [[]];
}

(** Charcter Black Belt *)
let black_belt = {
  name = "black belt"; 
  stats = {str = 5; agl = 5; int = 5; vit = 53; mp = 0; hit_percent = 5; m_def = 10; fight_def = 25};
  weapon = "Fists";
  spells = [];
  sprite = [[]];
}


(** Charcter Red Mage *)    
let red_mage = {
  name = "red mage"; 
  stats = {str = 10; agl = 10; int = 10; vit = 35; mp = 20; hit_percent = 7; m_def = 20; fight_def = 15};
  weapon = "Fists";
  spells = ["FIR2";"ICE";"LIT2"];
  sprite = [[]];
}

(** Charcter White Mage *)
let white_mage = {
  name = "white mage"; 
  stats = {str = 5; agl = 5; int = 15; vit = 38; mp = 25; hit_percent = 5; m_def = 20; fight_def = 15};
  weapon = "Silver Hammer";
  spells = ["LIT2";"ICE3";"LIT"];
  sprite = [[]];
}

(** Charcter Black Mage *)
let black_mage = {
  name = "black mage"; 
  stats = {str = 1; agl = 10; int = 20; vit = 26; mp = 25; hit_percent = 5; m_def = 20; fight_def = 15};
  weapon = "Mage staff";
  spells = ["ICE3";"FIR2";"FIRE"];
  sprite = [[]];
}

(** [get_characters] is the character list of all characters of the game *)
let get_characters = [fighter;thief; black_belt; red_mage; white_mage; black_mage]

(** [find_character n lst] is the character in [lst] with name [n] or 
    raises exception UnknownCharcter of [n] if no charcaters have name [n]*)
let rec find_character n lst = 
  match lst with 
  | [] -> raise (UnknownCharacter n)
  | {name;stats;weapon;spells;sprite}::t -> if name = n 
    then {name;stats;weapon;spells;sprite} else find_character n t

(** [add lst acc] is the character list [acc] with characters whose names are 
    in [lst]*)
let rec add lst acc  = 
  match lst with 
  | [] -> List.rev acc
  | x::t -> add t ((find_character x get_characters)::acc)
