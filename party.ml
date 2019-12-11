(** Party.ml contains the hardcoded charcaters; getter methods for their stats,
    weapons, spells, and sprite. As well as a method to get all characters and 
    adding for adding into a party. *)

(** represents the type of spells the characters have *)
type spell = string 

(** the type of a sprite of a character *)
type sprite = string list 

(** represents the stats every character comes with *)
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

(** Represents the type of character. Conatins all of the information needed 
    for each character of the game. *)
type character = {
  name : string; 
  stats : stat; 
  weapon: string; 
  spells : spell list; 
  sprite : sprite;
  test: (string list * string list) list
}

(** The type of a character in our game *)
type t = character

(** Throw exception when character is not one of the characters in Party *)
exception UnknownCharacter of string

(** [get_spells c] is the spell list of characater[c] *)
let get_spells c = c.spells

(** [get_stats c] is the stats of character [c] *)
let get_stats c = c.stats

(** [get_weapon c] is the weapon of character [c] *)
let get_weapon c = c.weapon 

(** [get_sprite c] is the sprite of character [c] *)
let get_sprite c = c.sprite

(** [get_test c] is the colored sprite of the character [c] used in display *)
let get_test c = c.test

(** [get_name c] is the name of character [c] *)
let get_name (c:character) = 
  match c with 
  | {name;_} -> name 

(** Charcter fighter *)
let fighter = {
  name = "fighter"; 
  stats = {str = 69; agl = 43; int = 23; vit = 993; mp = 70; hit_percent = 187;
           m_def = 60; fight_def = 60};
  weapon = "Sun Sword";
  spells = ["CURE";"FOG";"RUSE";"LAMP";"INVS";"PURE";];
  sprite = [
    "  ▄█▄█▄██████▄  ";
    " ██████████████▀";
    " ▄█████████████ ";
    " ████▄██▀██████▀";
    "  ▀ █ █   ▀█  █ ";
    "  ▄▄▄      ▄██▀ ";
    " ▀████▄▄▄███████";
    " █ ▀████▀▀▄▄ ▀█ ";
    "▄▀▀▀▄██   ▄██▄█ ";
    "▀▄▄▄▀▀████▀▀▀▀  ";
    "      █████     ";
    "     ▀████▀     ";
  ];
  test = [
    ([" ";" ";"▄";"█";"▄";"█";"▄";"█";"█";"█";"█";"█";"█";"▄";" ";" "],
     ["red";"red";"red";"red";"red";"red"; "red";"red";
      "red";"red";"red";"red";"red";"red";"red";"red"]);
    ([" ";"█";"█";"█";"█";"█";"█";"█";"█";"█";"█";"█";"█";"█";"█";"▀"],
     ["red";"red";"red";"red";"red";"red"; "red";"red";
      "red";"red";"red";"red";"red";"red";"red";"red"]);
    ([" ";"▄";"█";"█";"█";"█";"█";"█";"█";"█";"█";"█";"█";"█";"█";" "],
     ["red";"red";"red";"red";"red";"red"; "red";"red";
      "red";"red";"red";"red";"red";"red";"red";"red"]);
    ([" ";"█";"█";"█";"█";"▄";"█";"█";"▀";"█";"█";"█";"█";"█";"█";"▀"],
     ["red";"red";"red";"red";"red";"red"; "red";"red";
      "red";"red";"red";"red";"red";"red";"red";"red"]);
    ([" ";" ";"▀";" ";"█";" ";"█";" ";" ";" ";"▀";"█";" ";" ";"█ "],
     ["red";"red";"red";"red";"red";"red"; "white";"red";
      "red";"red";"red";"red";"red";"red";"red";"red"]);
    ([" ";" ";"▄";"▄";"▄";" ";" ";" ";" ";" ";" ";"▄";"█";"█";"▀";" "],
     ["white";"white";"white";"white";"white";"white"; "white";"white";
      "white";"white";"white";"white";"white";"white";"white";"white"]);
    ([" ";"▀";"█";"█";"█";"█";"▄";"▄";"▄";"█";"█";"█";"█";"█";"█";"█"],
     ["red";"red";"red";"red";"red";"red"; "red";"red";
      "red";"red";"red";"red";"red";"red";"red";"red"]);
    ([" ";"█";" ";"▀";"█";"█";"█";"█";"▀";"▀";"▄";"▄";" ";"▀";"█";" "],
     ["red";"red";"red";"red";"red";"red"; "red";"red";
      "red";"red";"yellow";"yellow";"yellow";"yellow";"yellow";"yellow"]);
    (["▄";"▀";"▀";"▀";"▄";"█";"█";" ";" ";" ";"▄";"█";"█";"▄";"█";" "],
     ["yellow";"yellow";"yellow";"yellow";"yellow";"red"; "red";"yellow";
      "yellow";"yellow";"yellow";"yellow";"yellow";"yellow";"yellow";"yellow"]);
    (["▀";"▄";"▄";"▄";"▀";"▀";"█";"█";"█";"█";"▀";"▀";"▀";"▀";" ";" "],
     ["yellow";"yellow";"yellow";"yellow";"yellow";"red"; "white";"white";
      "white";"white";"red";"red";"red";"red";"red";"red"]);
    ([" ";" ";" ";" ";" ";" ";"█";"█";"█";"█";"█";" ";" ";" ";" ";" "],
     ["red";"red";"red";"red";"red";"red"; "red";"red";
      "red";"red";"red";"red";"red";"red";"red";"red"]);
    ([" ";" ";" ";" ";" ";"▀";"█";"█";"█";"█";"▀";" ";" ";" ";" ";" "],
     ["red";"red";"red";"red";"red";"red"; "red";"red";
      "red";"red";"red";"red";"red";"red";"red";"red"]);            
  ];
}

(** Charcter Thief *)
let thief = {
  name = "thief"; 
  stats = {str = 42; agl = 46; int = 30; vit = 660; mp = 120; hit_percent = 118;
           m_def = 75; fight_def=51};
  weapon = "Dragon Sword";
  spells = ["FOG";"MUTE";"ICE";"DARK";"TMPR";"CUR2";"FIR2";"HOLD";"LIT2"];
  sprite = [
    "  ▄████▄████▄▄  ";
    " ████▀█████████▄";
    "  ▀██▄▄▄▀█▀  ███";
    "   ▄█ █  █  ▄█▀ ";
    " ▄███    ▄█████▄";
    "▄▀▄▄▀█▄▄███▀   █";
    "█▀▀▀██▀  ██    █";
    "█   ██   ██▄  █ ";
    " ▀▀▀ ███████▄▀  ";
    "     ▄█▄▄▄▄██▄  ";
    "      ██████▀   ";
    "     ▀█████▀    "
  ];
  test = [
    ([" ";" ";"▄";"█";"█";"█";"█";"▄";"█";"█";"█";"█";"▄";"▄";" ";" "],
     ["cyan";"cyan";"cyan";"cyan";"cyan";"cyan"; "cyan";"cyan";
      "cyan";"cyan";"cyan";"cyan";"cyan";"cyan";"cyan";"cyan"]);
    ([" ";"█";"█";"█";"█";"▀";"█";"█";"█";"█";"█";"█";"█";"█";"█";"▄"],
     ["cyan";"cyan";"cyan";"cyan";"cyan";"cyan"; "cyan";"cyan";
      "cyan";"cyan";"cyan";"cyan";"cyan";"cyan";"cyan";"cyan"]);
    ([" ";" ";"▀";"█";"█";"▄";"▄";"▄";"▀";"█";"▀";" ";" ";"█";"█";"█"],
     ["cyan";"cyan";"cyan";"cyan";"cyan";"cyan"; "cyan";"cyan";
      "cyan";"cyan";"cyan";"cyan";"cyan";"cyan";"cyan";"cyan"]);
    ([" ";" ";" ";"▄";"█";" ";"█";" ";" ";"█";" ";" ";"▄";"█";"▀";" "],
     ["cyan";"cyan";"cyan";"cyan";"cyan";"cyan"; "yellow";"cyan";
      "cyan";"cyan";"cyan";"cyan";"cyan";"cyan";"cyan";"cyan"]); 
    ([" ";"▄";"█";"█";"█";" ";" ";" ";" ";"▄";"█";"█";"█";"█";"█";"▄"],
     ["white";"white";"white";"white";"white";"white";"white";"white";
      "white";"white";"yellow";"yellow";"yellow";"yellow";"yellow";"yellow"]);
    (["▄";"▀";"▄";"▄";"▀";"█";"▄";"▄";"█";"█";"█";"▀";" ";" ";" ";"█"],
     ["white";"white";"white";"white";"white";"white";"white";"white";
      "white";"white";"yellow";"yellow";"yellow";"yellow";"yellow";"yellow"]);
    (["█";"▀";"▀";"▀";"█";"█";"▀";" ";" ";"█";"█";" ";" ";" ";" ";"█"],
     ["yellow";"yellow";"yellow";"yellow";"yellow";"white";"white";"white";
      "white";"white";"yellow";"yellow";"yellow";"yellow";"yellow";"yellow"]);
    (["█";" ";" ";" ";"█";"█";" ";" ";" ";"█";"█";"▄";" ";" ";"█";" "],
     ["yellow";"yellow";"yellow";"yellow";"yellow";"white";"white";"white";
      "white";"white";"yellow";"yellow";"yellow";"yellow";"yellow";"yellow"]);
    ([" ";"▀";"▀";"▀";" ";"█";"█";"█";"█";"█";"█";"█";"▄";"▀";" ";" "],
     ["yellow";"yellow";"yellow";"yellow";"yellow";"white";"white";"white";
      "white";"white";"white";"yellow";"yellow";"yellow";"yellow";"yellow"]);
    ([" ";" ";" ";" ";" ";"▄";"█";"▄";"▄";"▄";"▄";"█";"█";"▄";" ";" "],
     ["yellow";"yellow";"yellow";"yellow";"yellow";"yellow";"yellow";"yellow";
      "yellow";"yellow";"yellow";"yellow";"yellow";"yellow";"yellow";"yellow"]);
    ([" ";" ";" ";" ";" ";" ";"█";"█";"█";"█";"█";"█";"▀";" ";" ";" "],
     ["white";"white";"white";"white";"white";"white";"white";"white";
      "white";"white";"white";"white";"white";"white";"white";"white"]);
    ([" ";" ";" ";" ";" ";"▀";"█";"█";"█";"█";"█";"▀";" ";" ";" ";" "],
     ["white";"white";"white";"white";"white";"white";"white";"white";
      "white";"white";"white";"white";"white";"white";"white";"white"]);                 
  ];
}

(** Charcter Black Belt *)
let black_belt = {
  name = "black belt"; 
  stats = {str = 35; agl = 36; int = 35; vit = 999; mp = 45; hit_percent = 152;
           m_def = 59; fight_def = 43};
  weapon = "Fists";
  spells = ["CURE";"RUSE";"MUTE";"CUR2";"FIR3";];
  sprite = [
    "     ▄█████▄    ";
    "   ▄█████████▄  ";
    "  ██▀██████████ ";
    "  █████▀███████ ";
    " ▄▄█ █  ██████▀ ";
    "█▀▀█    ▄▀█▀▀█▄ ";
    "█▄▄██▄▄▀▄██▄▄  █";
    " ▀▀▀█▄▀ ▀█▄  ▀▄█";
    "    ▀█▄ ▄█▀ ▄█▀ ";
    "    ████████▀   ";
    "     ▀█████▀    ";
    "     ▀████▀     ";
  ];
  test = [
    ([" ";" ";" ";" ";" ";"▄";"█";"█";"█";"█";"█";"▄";" ";" ";" ";" "],
     ["yellow";"yellow";"yellow";"yellow";"yellow";"yellow";"yellow";"yellow";
      "yellow";"yellow";"yellow";"yellow";"yellow";"yellow";"yellow";"yellow"]);
    ([" ";" ";" ";"▄";"█";"█";"█";"█";"█";"█";"█";"█";"█";"▄";" ";" "],
     ["yellow";"yellow";"yellow";"yellow";"yellow";"yellow";"yellow";"yellow";
      "yellow";"yellow";"yellow";"yellow";"yellow";"yellow";"yellow";"yellow"]);
    ([" ";" ";"█";"█";"▀";"█";"█";"█";"█";"█";"█";"█";"█";"█";"█";" "],
     ["yellow";"yellow";"yellow";"yellow";"yellow";"yellow";"yellow";"yellow";
      "yellow";"yellow";"yellow";"yellow";"yellow";"yellow";"yellow";"yellow"]);
    ([" ";" ";"█";"█";"█";"█";"█";"▀";"█";"█";"█";"█";"█";"█";"█";" "],
     ["yellow";"yellow";"yellow";"yellow";"cyan";"cyan";"cyan";"yellow";
      "yellow";"yellow";"cyan";"cyan";"cyan";"yellow";"yellow";"yellow"]);
    ([" ";"▄";"▄";"█";" ";"█";" ";" ";"█";"█";"█";"█";"█";"█";"▀";" "],
     ["yellow";"cyan";"cyan";"cyan";"yellow";"yellow";"yellow";"yellow";
      "yellow";"yellow";"yellow";"yellow";"yellow";"yellow";"yellow";"yellow"]);
    (["█";"▀";"▀";"█";" ";" ";" ";" ";"▄";"▀";"█";"▀";"▀";"█";"▄";" "],
     ["yellow";"yellow";"yellow";"yellow";"yellow";"yellow";"yellow";"yellow";
      "cyan";"cyan";"cyan";"cyan";"cyan";"cyan";"cyan";"cyan"]);
    (["█";"▄";"▄";"█";"█";"▄";"▄";"▀";"▄";"█";"█";"▄";"▄";" ";" ";"█"],
     ["yellow";"yellow";"yellow";"yellow";"cyan";"cyan"; "cyan";"cyan";
      "cyan";"cyan";"cyan";"cyan";"cyan";"cyan";"cyan";"cyan"]); 
    ([" ";"▀";"▀";"▀";"█";"▄";"▀";" ";"▀";"█";"▄";" ";" ";"▀";"▄";"█"],
     ["cyan";"cyan";"cyan";"cyan";"cyan";"yellow"; "yellow";"yellow";
      "yellow";"yellow";"cyan";"cyan";"cyan";"cyan";"cyan";"cyan"]);
    ([" ";" ";" ";" ";"▀";"█";"▄";" ";"▄";"█";"▀";" ";"▄";"█";"▀";" "],
     ["cyan";"cyan";"cyan";"cyan";"cyan";"yellow"; "yellow";"yellow";
      "yellow";"yellow";"cyan";"cyan";"cyan";"cyan";"cyan";"cyan"]);
    ([" ";" ";" ";" ";"█";"█";"█";"█";"█";"█";"█";"█";"▀";" ";" ";" "],
     ["cyan";"cyan";"cyan"; "cyan";"cyan";"cyan"; "cyan";"cyan";
      "cyan";"cyan";"cyan";"cyan";"cyan";"cyan";"cyan";"cyan"]);
    ([" ";" ";" ";" ";" ";"▀";"█";"█";"█";"█";"█";"▀";" ";" ";" ";" "],
     ["cyan";"cyan";"cyan";"cyan";"cyan";"cyan"; "cyan";"cyan";
      "cyan";"cyan";"cyan";"cyan";"cyan";"cyan";"cyan";"cyan"]); 
    ([" ";" ";" ";" ";" ";"▀";"█";"█";"█";"█";"▀";" ";" ";" ";" ";" "],
     ["cyan";"cyan";"cyan";"cyan";"cyan";"cyan"; "cyan";"cyan";
      "cyan";"cyan";"cyan";"cyan";"cyan";"cyan";"cyan";"cyan"]);            
  ];
}


(** Charcter Red Mage *)    
let red_mage = {
  name = "red mage"; 
  stats = {str = 41; agl = 35; int = 40; vit = 591; mp = 150; hit_percent = 135; 
           m_def = 81; fight_def = 51};
  weapon = "Sun Sword";
  spells = ["CURE";"RUSE";"LIT";"LAMP";"TMPR";"HOLD";"PURE";"ICE2";"CUR3";"FIR3"
           ;"FOG2";"INV2";];
  sprite = [
    "    ▄▄████▄█▀▀  ";
    "    ████████   ▄";
    "   ▄██████████▀ ";
    "▄▄████▀█▀▀▀▄    ";
    "    █  █    ▀▄  ";
    " ▄█ ▄▄▄█ ▄█▄ ▄█ ";
    "▀  ▀ ▀▀▀▄███▄██ ";
    "███ ██ ██▄▀████▄";
    "▀█▀ ▀█ ▀▀▀█▄▀███";
    " █   ▀█████   ██";
    " █▄▄▄ █████ ▄▄▄█";
    "     ▀▀▀▀▀▀    ▀";
  ];
  test =[
    ([" ";" ";" ";" ";"▄";"▄";"█";"█";"█";"█";"▄";"█";"▀";"▀";" ";" "],
     ["black";"black";"black";"black";"red";"red";"red";"red";"red";
      "red";"red";"white";"white";"white";"white";"black"]);
    ([" ";" ";" ";" ";"█";"█";"█";"█";"█";"█";"█";"█";" ";" ";"▄";"▄"],
     ["black";"black";"black";"black";"red";"red";"red";"red";"red";
      "red";"white";"red";"white";"white";"red";"red"]);
    ([" ";" ";" ";"▄";"█";"█";"█";"█";"█";"█";"█";"█";"█";"█";"▀";" "],
     ["black";"black";"black";"red";"red";"red";"red";"red";"red";
      "red";"red";"red";"red";"red";"red";"black"]);
    (["▄";"▄";"█";"█";"█";"█";"▀";"█";"▀";"▀";"▀";"▄";" ";" ";" ";" "],
     ["red";"red";"red";"red";"red";"red";"red";"red";"red";
      "red";"red";"white";"red";"red";"red";"black"]);
    ([" ";" ";" ";" ";"█";" ";" ";"█";" ";" ";" ";" ";"▀";"▄";" ";" "],
     ["red";"red";"red";"red";"yellow";"red";"red";"white";"red";
      "red";"red";"white";"white";"white";"white";"black"]);
    ([" ";"▄";"█";" ";"▄";"▄";"▄";"█";" ";"▄";"█";"▄";" ";"▄";"█";" "],
     ["red";"red";"red";"red";"red";"red";"red";"white";"white";
      "red";"red";"red";"white";"white";"white";"black"]);
    (["▀";" ";" ";"▀";" ";"▀";"▀";"▀";"▄";"█";"█";"█";"▄";"█";"█";" "],
     ["red";"red";"red";"red";"red";"red";"red";"red";"white";
      "red";"red";"red";"white";"red";"red";"black"]); 
    (["█";"█";"█";" ";"█";"█";" ";"█";"█";"▄";"▀";"█";"█";"█";"█";"▄"],
     ["yellow";"yellow";"yellow";"red";"red";"red";"red";"yellow";"yellow";
      "yellow";"red";"red";"red";"red";"red";"red"]);
    (["▀";"█";"▀";" ";"▀";"█";" ";"▀";"▀";"▀";"█";"▄";"▀";"█";"█";"█"],
     ["red";"red";"red";"red";"red";"red";"red";"red";"red";
      "red";"red";"red";"red";"red";"red";"red"]);
    ([" ";"█";" ";" ";" ";"▀";"█";"█";"█";"█";"█";" ";" ";" ";"█";"█"],
     ["red";"red";"red";"red";"red";"red";"red";"red";"red";
      "red";"red";"red";"red";"red";"red";"red"]);
    ([" ";"█";"▄";"▄";"▄";" ";"█";"█";"█";"█";"█";" ";"▄";"▄";"▄";"█"],
     ["red";"red";"red";"red";"red";"red";"red";"red";"red";
      "red";"red";"red";"red";"red";"red";"red"]);
    ([" ";" ";" ";" ";" ";"▀";"▀";"▀";"▀";"▀";"▀";" ";" ";" ";" ";"▀"],
     ["red";"red";"red";"red";"red";"red";"red";"red";"red";
      "red";"red";"red";"red";"red";"red";"red"])  
  ];
}

(** Charcter White Mage *)
let white_mage = {
  name = "white mage"; 
  stats = {str = 30; agl = 30; int = 49; vit = 676; mp = 180; hit_percent = 59; 
           m_def = 81; fight_def = 51};
  weapon = "Silver Hammer";
  spells = ["MUTE";"INVS";"DARK";"CUR2";"HOLD";"AMUT";"FOG2";"LIT3";"STUN";
            "HEL3";"CUR4";"BLND";];
  sprite = [
    "    ▄▄▄▄▄▀▀▀▀▀▄ ";
    "  ▄▀        ▄ █ ";
    "▄▀▄█����████▄   ▀▄ ";
    " ▀▄ ▀▀▀  ▀█   █ ";
    "   █ ▀   █  ▄▀ ";
    " ▄██  ▄▄███   █ ";
    "█▄▄▄█▀▀▀▀▀▀▄▀ ▀▄";
    "█▀  ██▀ ��   █  █";
    "█▀  ██▄�� ��     █";
    "█▀  █▀█  ▄     █";
    " █   █▄▀██ ▄█ ▄█";
    "  ▀█████▀▀█████▀";
  ];
  test = [
    ([" ";" ";" ";" ";"▄";"▄";"▄";"▄";"▄";"▀";"▀";"▀";"▀";"▀";"▄";" "],
     ["black";"black";"black";"black";"white";"white";"white";"white";"white";
      "white";"white";"white";"white";"white";"white";"black";]);
    ([" ";" ";"▄";"▀";" ";" ";" ";" ";" ";" ";" ";" ";"▄";" ";"█";" "],
     ["black";"black";"white";"white";"white";"white";"white";"white";"white";
      "white";"white";"white";"white";"white";"white";"black";]);
    (["▄";"▀";"▄";"█";"█";"█";"█";"█";"█";"▄";" ";" ";" ";"▀";"▄";" "],
     ["white";"white";"white";"white";"white";"white";"white";"white";"white";
      "white";"white";"white";"white";"white";"white";"black";]);
    ([" ";"▀";"▄";" ";"▀";"▀";"▀";" ";" ";"▀";"█";" ";" ";" ";"█";" "],
     ["black";"white";"white";"black";"red";"red";"red";"yellow";"white";
      "white";"white";"white";"white";"white";"white";"black";]);
    ([" ";" ";" ";"█";" ";"▀";" ";" ";" ";" ";"█";" ";" ";"▄";"▀";" "],
     ["black";"white";"white";"white";"black";"yellow";"black";"white";"white";
      "white";"white";"white";"white";"white";"white";"black";]);
    ([" ";"▄";"█";"█";" ";" ";"▄";"▄";"█";"█";"█";" ";" ";" ";"█";" "],
     ["white";"white";"white";"white";"white";"white";"white";"white";"white";
      "white";"white";"white";"white";"white";"white";"black";]);
    (["█";"▄";"▄";"▄";"█";"▀";"▀";"▀";"▀";"▀";"▀";"▄";"▀";" ";"▀";"▄"],
     ["red";"white";"white";"white";"white";"white";"white";"white";"white";
      "white";"white";"white";"white";"white";"white";"white";]);
    (["█";"▀";" ";" ";"█";"█";"▀";" ";" ";" ";" ";" ";"█";" ";" ";"█"],
     ["red";"red";"white";"white";"red";"white";"white";"white";"white";
      "white";"white";"white";"white";"white";"white";"white";]);
    (["█";"▀";" ";" ";"█";"█";"▄";"▄";" ";" ";" ";" ";" ";" ";" ";"█"],
     ["red";"red";"white";"white";"red";"red";"white";"white";"white";
      "white";"white";"white";"white";"white";"white";"white";]);
    (["█";"▀";" ";" ";"█";"▀";"█";" ";" ";"▄";" ";" ";" ";" ";" ";"█"],
     ["red";"red";"white";"white";"red";"red";"white";"white";"white";
      "white";"white";"white";"white";"white";"white";"white";]);
    ([" ";"█";" ";" ";" ";"█";"▄";"▀";"█";"█";" ";"▄";"█";" ";"▄";"█"],
     ["red";"red";"white";"white";"red";"red";"red";"white";"white";
      "white";"white";"red";"red";"white";"white";"white";]);
    ([" ";" ";"▀";"█";"█";"█";"█";"█";"▀";"▀";"█";"█";"█";"█";"█";"▀"],
     ["red";"red";"red";"red";"red";"red";"red";"red";"red";
      "red";"red";"red";"red";"red";"red";"red";])      
  ];
}

(** Charcter Black Mage *)
let black_mage = {
  name = "black mage"; 
  stats = {str = 23; agl = 32; int = 69; vit = 470; mp = 215; hit_percent = 64;
           m_def = 81; fight_def = 51};
  weapon = "Mage staff";
  spells = ["MUTE";"INVS";"HOLD";"LIT2";"HEL2";"FIR3";"STUN";"ICE3";"SABR";
            "NUKE";"FADE";"BRAK";];
  sprite = [
    "           ▄▄██ "; 
    "        ▄▄████  ";
    "▄▄▄▄▄▄███████   ";
    " ▀▀▀████████▀   ";
    "     ▀▀▀▀▀███▄  ";
    " ▄▄ █  █   █▀█▄ ";
    " █▄▄   ▄▄▄▄▄▄█  "; 
    "▄▄▀███▀▀▄▄▄██▄  ";
    "▀▀▄██▄▄ ██████  ";
    " █ ██▀▀ █████▀  ";
    " ██▄▀███ ████   ";
    "▄████▄▄██▄▀▀▄█▄ ";
  ];
  test = [
    ([" ";" ";" ";" ";" ";" ";" ";" ";" ";" ";" ";"▄";"▄";"█";"█";" "], 
     ["black"; "black"; "black"; "black";"black"; 
      "black"; "black"; "black";"black"; "black"; 
      "black"; "yellow";"yellow"; "yellow"; "yellow"; "black";]);
    ([" ";" ";" ";" ";" ";" ";" ";" ";"▄";"▄";"█";"█";"█";"█";" ";" "], 
     ["black"; "black"; "black"; "black";"black"; 
      "black"; "black"; "black";"yellow"; "yellow"; 
      "yellow"; "yellow";"yellow"; "yellow"; "black"; "black";]);
    (["▄";"▄";"▄";"▄";"▄";"▄";"█";"█";"█";"█";"█";"█";"█";" ";" ";" "], 
     ["yellow"; "yellow"; "yellow"; "yellow";"yellow";"yellow"; 
      "yellow"; "yellow"; "yellow";"yellow";"yellow"; 
      "yellow"; "yellow"; "black";"black";"black"]);
    ([" ";"▀";"▀";"▀";"█";"█";"█";"█";"█";"█";"█";"█";"▀";" ";" ";" "],
     ["black"; "yellow"; "yellow"; "yellow"; "yellow"; "yellow"; "yellow"; 
      "yellow";"yellow"; "yellow"; "yellow"; 
      "yellow";"yellow"; "black"; "black"; "black";]);
    ([" ";" ";" ";" ";" ";"▀";"▀";"▀";"▀";"▀";"█";"█";"█";"▄";" ";" "],
     ["black"; "black"; "black"; "black"; "black"; "yellow"; "yellow"; 
      "yellow";"yellow"; "yellow"; "yellow"; 
      "yellow";"yellow"; "yellow"; "black"; "black";]);
    ([" ";"▄";"▄";" ";"█";" ";" ";"█";" ";" ";" ";"█";"▀";"█";"▄";" "],
     ["black"; "cyan"; "cyan"; "black"; "yellow"; "black"; "black"; 
      "yellow";"black"; "black"; "black"; 
      "yellow";"yellow"; "yellow"; "yellow"; "black";]);
    ([" ";"█";"▄";"▄";" ";" ";" ";"▄";"▄";"▄";"▄";"▄";"▄";"█";" ";" "],
     ["black"; "cyan"; "cyan"; "cyan"; "yellow"; "black"; "black"; 
      "cyan";"cyan"; "cyan"; "cyan"; 
      "cyan";"cyan"; "cyan"; "yellow"; "black";]); 
    (["▄";"▄";"▀";"█";"█";"█";"▀";"▀";"▄";"▄";"▄";"█";"█";"▄";" ";" "],
     ["yellow"; "yellow"; "cyan"; "cyan"; "cyan"; "cyan"; "cyan"; 
      "cyan";"cyan"; "cyan"; "cyan"; 
      "cyan";"cyan"; "cyan"; "black"; "black";]);
    (["▀";"▀";"▄";"█";"█";"▄";"▄";" ";"█";"█";"█";"█";"█";"█";" ";" "], 
     ["yellow"; "yellow"; "cyan"; "cyan"; "cyan"; "yellow"; "yellow"; 
      "cyan";"cyan"; "cyan"; "cyan"; 
      "cyan";"cyan"; "cyan"; "black"; "black";]);
    ([" ";"█";" ";"█";"█";"▀";"▀";" ";"█";"█";"█";"█";"█";"▀";" ";" "], 
     ["yellow"; "cyan"; "cyan"; "cyan"; "cyan"; "yellow"; "yellow"; 
      "cyan";"cyan"; "cyan"; "cyan"; 
      "cyan";"cyan"; "cyan"; "black"; "black";]);
    ([" ";"█";"█";"▄";"▀";"█";"█";"█";" ";"█";"█";"█";"█";" ";" ";" "],
     ["cyan"; "cyan"; "cyan"; "cyan"; "cyan"; "cyan"; "cyan"; 
      "cyan";"cyan"; "cyan"; "cyan"; 
      "cyan";"cyan"; "cyan"; "black"; "black";]);
    (["▄";"█";"█";"█";"█";"▄";"▄";"█";"█";"▄";"▀";"▀";"▄";"█";"▄";" "], 
     ["cyan"; "cyan"; "cyan"; "cyan"; "cyan"; "cyan"; "cyan"; 
      "cyan";"cyan"; "cyan"; "cyan"; 
      "cyan";"cyan"; "cyan"; "cyan"; "cyan";])                                               
  ];
}

(** [lump] is the sprite used when a character has been defeated and is at
    0 HP. *)
let lump = [
  "                ";
  "                ";
  "                ";
  "                ";
  "                ";
  "                ";
  "                "; 
  " ▄▄    ▄▄▄  ▄▄  ";
  "████▄▄███████ ▀▄";
  "███████████▀█ ▄▀";
  "███▀▀▀█████  █▀ ";
  "▀▄▄▄▄██████▄▀   ";
]

(** [skull] is a colored sprite of a skull to represent the character's when 
    dead. This occurs when the vit is 0 *)
let skull = [
  ([" ";" ";" ";"█";"█";"█";"█";"█";"█";"█";"█";"█";" ";" ";" ";" "],
   ["white";"white";"white";"white";"white";"white";"white";"white";"white";
    "white";"white";"white";"white";"white";"white";"white";]);
  ([" ";"█";"█";"█";"█";"█";"█";"█";"█";"█";"█";"█";"█";"█";" ";" "],
   ["white";"white";"white";"white";"white";"white";"white";"white";"white";
    "white";"white";"white";"white";"white";"white";"white";]);
  ([" ";"█";"█";" ";" ";" ";"█";"█";"█";" ";" ";" ";"█";"█";" ";" "],
   ["white";"white";"white";"white";"white";"white";"white";"white";"white";
    "white";"white";"white";"white";"white";"white";"white";]);
  ([" ";"█";"█";" ";" ";" ";"█";"█";"█";" ";" ";" ";"█";"█";" ";" "],
   ["white";"white";"white";"white";"white";"white";"white";"white";"white";
    "white";"white";"white";"white";"white";"white";"white";]);
  ([" ";"█";"█";"█";"█";"█";"█";"█";"█";"█";"█";"█";"█";"█";" ";" "],
   ["white";"white";"white";"white";"white";"white";"white";"white";"white";
    "white";"white";"white";"white";"white";"white";"white";]);
  ([" ";" ";"█";"█";"▀";"█";"▀";"█";"▀";"█";"▀";"█";"█";" ";" ";" "],
   ["white";"white";"white";"white";"white";"white";"white";"white";"white";
    "white";"white";"white";"white";"white";"white";"white";]);
  ([" ";" ";" ";"█";"▄";"█";"▄";"█";"▄";"█";"▄";"█";" ";" ";" ";" "],
   ["white";"white";"white";"white";"white";"white";"white";"white";"white";
    "white";"white";"white";"white";"white";"white";"white";]);
  ([" ";"█";"█";"█";" ";" ";" ";" ";" ";" ";" ";"█";"█";"█";" ";" "],
   ["white";"white";"white";"white";"white";"white";"white";"white";"white";
    "white";"white";"white";"white";"white";"white";"white";]);
  ([" ";" ";" ";" ";"█";"█";"█";" ";" ";"█";"█";"█";" ";" ";" ";" "],
   ["white";"white";"white";"white";"white";"white";"white";"white";"white";
    "white";"white";"white";"white";"white";"white";"white";]);
  ([" ";" ";" ";" ";" ";" ";"█";"█";"█";"█";" ";" ";" ";" ";" ";" "],
   ["white";"white";"white";"white";"white";"white";"white";"white";"white";
    "white";"white";"white";"white";"white";"white";"white";]);
  ([" ";" ";" ";" ";"█";"█";"█";" ";" ";"█";"█";"█";" ";" ";" ";" "],
   ["white";"white";"white";"white";"white";"white";"white";"white";"white";
    "white";"white";"white";"white";"white";"white";"white";]);
  ([" ";"█";"█";"█";" ";" ";" ";" ";" ";" ";" ";"█";"█";"█";" ";" "],
   ["white";"white";"white";"white";"white";"white";"white";"white";"white";
    "white";"white";"white";"white";"white";"white";"white";])                  
]


(* let color_helper h = 
   match h with 
   | "cyan" -> [ANSITerminal.cyan]
   | "yellow" -> [ANSITerminal.yellow]
   | "black" -> [ANSITerminal.black]
   | "white" -> [ANSITerminal.white]
   | "red" -> [ANSITerminal.red]
   | "blue" -> [ANSITerminal.blue]
   | "magenta" -> [ANSITerminal.magenta]
   | "green" -> [ANSITerminal.green]
   | "background white" -> [Background White]
   | _ -> [ANSITerminal.default]

   let rec helper string color int = 
   match string with 
   | [] -> print_endline ""
   | h:: t -> ANSITerminal.(print_string (color_helper (List.nth color int)) h);
    helper t color (int+1)

   let rec pr s = 
   match s with 
   | [] -> ()
   | (s1,lst)::t -> helper s1 lst 0; pr t  *)

(** [get_characters] is the character list of all characters of the game *)
let get_characters = [fighter;thief; black_belt; red_mage; white_mage;
                      black_mage]

(** [find_character n lst] is the character in [lst] with name [n] or 
    raises exception UnknownCharcter of [n] if no charcaters have name [n]*)
let rec find_character n lst = 
  match lst with 
  | [] -> raise (UnknownCharacter n)
  | {name;stats;weapon;spells;sprite;test}::t -> if name = n 
    then {name;stats;weapon;spells;sprite;test} else find_character n t

(** [add lst acc] is the character list [acc] with characters whose names are 
    in [lst]*)
let rec add lst acc  = 
  match lst with 
  | [] -> List.rev acc
  | x::t -> add t ((find_character x get_characters)::acc)

(** [char_og_health char] is the original health of [char] *)
let char_og_health char = 
  char.stats.vit   

(** [has_spell char spell] is [true] if [char] has [spell] in spell list else 
    [false] *)
let has_spell char spell = 
  List.mem spell char.spells   
