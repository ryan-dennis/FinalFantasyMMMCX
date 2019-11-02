(** State contains all of the temporary and changing information regarding the 
    party members and the enemy. *)

open Party 
open Command 
open Gauntlet 

(** Represents the state of the game beinf played *)
type t = {
  health : (string*int) list; 
  magic_points : (string*int) list; 
  turnorder : string list;
  current_boss: string;
  next_boss : string; 
}

(** [get_party_health gtl party acc] is [acc] with the (name,vit) of every
    character in [party]  *)
let rec get_party_health party acc = 
  match party with 
  | [] -> acc 
  | x::t -> get_party_health t ((get_name x,(get_stats x).vit)::acc) 

(** [get_party_health gtl party acc] is [acc] with the (name,mp) of every
    character in [party]  *)
let rec get_party_mp party acc = 
  match party with 
  | [] -> acc 
  | x::t -> get_party_health t ((get_name x,(get_stats x).mp)::acc) 

(** [get_bs gtl boss] is the health of [boss] in [gtl] *)
let get_bs gtl boss = (boss_stats gtl boss).hp 

(** [init state gtl party] initializes the first state of the game. Every 
    Party memeber has full health and magic points as well as the boss. The
    turnover is the official turnover including all 4 players. Current boss is the 
    boss currently beinf played against and next boss is the boss to be played next 
    if the party succeeds. *)
let init_state gtl party = 
  let boss = start_boss gtl in
  {health = (boss, get_bs gtl boss)::(get_party_health party []);
   magic_points = get_party_mp party [] ;
   turnorder = []; current_boss =boss; next_boss = next gtl boss}

(** [helper name lst] is the health of [name] of the character in 
    [lst] or raises UnknownCharacter if not in [lst] *)
let rec helper name lst = 
  match lst with 
  | [] -> raise (UnknownCharacter name)
  | (n,i)::t -> if n=name then i else helper name t 

(** [get_health name t] is the health of chracter with name [name] in 
    state [t]  *)
let get_health name t = helper name t.health  

let get_magic_points name t = helper name t.magic_points

let get_current_boss t = t.current_boss

let get_turnorder t = t.turnorder

let get_next_boss t = t.next_boss




