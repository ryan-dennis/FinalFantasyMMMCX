(** Party.ml contains the hardcoded charcaters; getter methods for their stats,
    weapons, spells, and sprite. As well as a method to get all characters and adding 
    for adding into a party. *)

(** Question: Is vitality suppose to act like HP and be a constant start or should 
    I have both Vitality and HP? 
    Question: Does magoc points vary with each spell or are we doing general for all 
    spells you can use them until you run out of MP*)

type spell = string 

type sprite = (string list) list 

type stat = {
  str : int; 
  agl : int; 
  int : int;
  vit : int; (** Treating like just HP for right now. Doing HP plus one Vit *)
  mp : int;
  hit_percent : int; (** float or int?*)
  (* absorb : Not in the website.. what is this?*)
}

(** Question: Are we only doing the weapon or also armor and shield?*)
type character = {
  name : string; 
  stats : stat; 
  weapon: string; 
  spells : spell list; 
  sprite : sprite;
}

type t = character

exception UnknownCharcter of string

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
  stats = {str = 20; agl = 5; int = 1; vit = 45; mp = 0; hit_percent = 10};
  weapon = "Sun sword";
  spells = [];
  sprite = [[]];
}

(** Charcter Thief *)
let thief = {
  name = "thief"; 
  stats = {str = 5; agl = 10; int = 5; vit = 35; mp = 0; hit_percent = 5};
  weapon = "Dragon sword";
  spells = [];
  sprite = [[]];
}

(** Charcter Black Belt *)
let black_belt = {
  name = "black belt"; 
  stats = {str = 5; agl = 5; int = 5; vit = 53; mp = 0; hit_percent = 5};
  weapon = "Fists";
  spells = [];
  sprite = [[]];
}

(**Question: Does not say how many MP they start with so I just made up a number.. *)
(** I gave them all fire ice and lightning spells for now because those 
    are the 3 spells we wanted done by MS2. I will add poison and Status and change 
    the three spells for each character around accordingly in MS3 *)

(** Charcter Red Mage *)    
let red_mage = {
  name = "red mage"; 
  stats = {str = 10; agl = 10; int = 10; vit = 35; mp = 20; hit_percent = 7};
  weapon = "Fists";
  spells = ["fire";"ice";"lightning"];
  sprite = [[]];
}

(** Charcter White Mage *)
let white_mage = {
  name = "white mage"; 
  stats = {str = 5; agl = 5; int = 15; vit = 38; mp = 25; hit_percent = 5};
  weapon = "Silver Hammer";
  spells = ["ice";"fire";"lightning"];
  sprite = [[]];
}

(** Charcter Black Mage *)
let balck_mage = {
  name = "black mage"; 
  stats = {str = 1; agl = 10; int = 20; vit = 26; mp = 25; hit_percent = 5};
  weapon = "Mage staff";
  spells = ["lightning";"fire";"ice"];
  sprite = [[]];
}

(** [get_characters] is the character list of all characters of the game *)
let get_characters = [fighter;thief; black_belt; red_mage; white_mage; balck_mage]

(** [find_character n lst] is the character in [lst] with name [n] or 
    raises exception UnknownCharcter of [n] if no charcaters have name [n]*)
let rec find_character n lst = 
  match lst with 
  | [] -> raise (UnknownCharcter n)
  | {name;stats;weapon;spells;sprite}::t -> if name = n 
    then {name;stats;weapon;spells;sprite} else find_character n t

(** [add lst acc] is the character list [acc] with characters whose names are 
    in [lst]*)
let rec add lst acc  = 
  match lst with 
  | [] -> acc
  | x::t -> add t ((find_character x get_characters)::acc)
