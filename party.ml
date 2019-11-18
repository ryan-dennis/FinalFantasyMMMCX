(** Party.ml contains the hardcoded charcaters; getter methods for their stats,
    weapons, spells, and sprite. As well as a method to get all characters and adding 
    for adding into a party. *)

type spell = string 

type sprite = string list 

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
let get_spells c = c.spells

(** [get_stats c] is the stats of character [c] *)
let get_stats c = c.stats

(** [get_weapon c] is the weapon of character [c] *)
let get_weapon c = c.weapon 

(** [get_sprite c] is the sprite of character [c] *)
let get_sprite c = c.sprite

(** [get_name c] is the name of character [c] *)
let get_name (c:character) = 
  match c with 
  | {name;_} -> name 

(** Charcter fighter *)
let fighter = {
  name = "fighter"; 
  stats = {str = 69; agl = 43; int = 23; vit = 993; mp = 0; hit_percent = 187; m_def = 162; fight_def = 60};
  weapon = "Sun Sword";
  spells = ["CURE";"FOG";"RUSE";"LAMP";"INVS";"PURE";];
  sprite = [
    "                       ████     "; 
    "                  ████     ███  ";
    "                ███        ███  ";
    "            ████          ██    ";
    "   ██████████           ██      ";
    " ██                      ██     ";
    "    ██████                 ██   "; 
    "       ██████████           ██  ";
    "     ████   ███████           █ ";
    "    █████   █████   ██         █";
    "    █████████████   ███████████ ";
    "    ███████████████████████████ ";
    "██    ██████████████████    ████";
    "████             ███████      █ ";
  ]
}

(** Charcter Thief *)
let thief = {
  name = "thief"; 
  stats = {str = 42; agl = 46; int = 30; vit = 660; mp = 15; hit_percent = 118; m_def = 113; fight_def=51};
  weapon = "Dragon Sword";
  spells = ["CURE";"FOG";"FIRE";"LIT";"MUTE";"ICE";"DARK";"TMPR";"CUR2";"FIR2";
            "HOLD";"LIT2";"ICE2";];
  sprite = [];
}

(** Charcter Black Belt *)
let black_belt = {
  name = "black belt"; 
  stats = {str = 35; agl = 36; int = 35; vit = 999; mp = 0; hit_percent = 152; m_def = 206; fight_def = 43};
  weapon = "Fists";
  spells = ["CURE";"RUSE";"MUTE";"CUR2";"FIR3";];
  sprite = [
    "██████████";
    "█        █";
    "█        █";
    "█        █";
    "██████████";
  ];
}


(** Charcter Red Mage *)    
let red_mage = {
  name = "red mage"; 
  stats = {str = 41; agl = 35; int = 40; vit = 591; mp = 20; hit_percent = 135; m_def = 118; fight_def = 51};
  weapon = "Sun Sword";
  spells = ["CURE";"FOG";"RUSE";"FIRE";"LIT";"LAMP";"MUTE";"INVS";"ICE";"DARK";
            "TMPR";"CUR2";"FIR2";"HOLD";"LIT2";"PURE";"AMUT";"ICE2";"CUR3";"FIR3";
            "FOG2";"INV2";"LIT3";"ICE3";];
  sprite = [];
}

(** Charcter White Mage *)
let white_mage = {
  name = "white mage"; 
  stats = {str = 30; agl = 30; int = 49; vit = 676; mp = 25; hit_percent = 59; m_def = 118; fight_def = 51};
  weapon = "Silver Hammer";
  spells = ["CURE";"FOG";"RUSE";"FIRE";"LIT";"LAMP";"MUTE";"INVS";"ICE";"DARK";
            "TMPR";"CUR2";"HEAL";"FIR2";"HOLD";"LIT2";"PURE";"AMUT";"ICE2";"CUR3";
            "HEL2";"FIR3";"FOG2";"INV2";"LIT3";"STUN";"HEL3";"ICE3";"BLND";];
  sprite = [];
}

(** Charcter Black Mage *)
let black_mage = {
  name = "black mage"; 
  stats = {str = 23; agl = 32; int = 69; vit = 470; mp = 25; hit_percent = 64; m_def = 118; fight_def = 51};
  weapon = "Mage staff";
  spells = ["CURE";"FOG";"RUSE";"FIRE";"LIT";"LAMP";"MUTE";"INVS";"ICE";"DARK";
            "TMPR";"CUR2";"HEAL";"FIR2";"HOLD";"LIT2";"PURE";"AMUT";"ICE2";"CUR3";
            "HEL2";"FIR3";"FOG2";"INV2";"LIT3";"STUN";"CUR4";"HEL3";"ICE3";"BRAK";
            "BLND";"SABR";"NUKE";"FADE";];
  sprite = [];
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

(** [char_og_health char] is the health of [char] *)
let char_og_health char = 
  char.stats.vit   

(** [has_spell char spell] is [true] if [char] has [spell] in spell list else 
    [false] *)
let has_spell char spell = 
  List.mem spell char.spells   
