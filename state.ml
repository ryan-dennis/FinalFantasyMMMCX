(** State contains all of the temporary and changing information regarding the 
    party members and the enemy. *)

open Party 
open Command 
open Gauntlet 

type t = {
  health : (string*int) list; (**boss and party *)
  magic_points : (string*int) list; (** party  *)
  turnorder : string list;
  current_boss: string;
  next_boss : string; 
}

let rec get_party_health gtl party acc = 
  match party with 
  | [] -> acc 
  | x::t -> get_party_health gtl t ((get_name x,(get_stats x).vit)::acc) 

let rec get_party_mp gtl party acc = 
  match party with 
  | [] -> acc 
  | x::t -> get_party_health gtl t ((get_name x,(get_stats x).mp)::acc) 

let get_bs gtl boss = (boss_stats gtl boss).hp 


let init_state gtl party boss = 
  {health = (boss, get_bs gtl boss)::(get_party_health gtl party []);
   magic_points = get_party_mp gtl party [] ;
   turnorder = []; current_boss =boss; next_boss = next gtl boss}

let rec health_helper name lst = 
  match lst with 
  | [] -> raise (UnknownCharacter name)
  | (n,i)::t -> if n=name then i else health_helper name t 

let get_health name t = health_helper name t.health  



