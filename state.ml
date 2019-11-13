(** State contains all of the temporary and changing information regarding the 
    party members and the enemy. *)
open Party 
open Gauntlet 
open Status
(** Represents the state of the game beinf played *)
type t = {
  health : (string*int) list; 
  magic_points : (string*int) list; 
  status's : (string *(Status.t list)) list;
  turnorder : string list;
  party : string list;
  current_boss: string;
  next_boss : string; 
  current_fighter: string;
  next_fighter: string
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

(** [party_helper lst acc] is [acc] with the name of every character in 
    [lst] *)
let rec party_helper (lst: Party.t list) acc = 
  match lst with 
  | [] -> acc
  | x::t -> party_helper t (get_name x::acc)

(** [shuffle lst] generates a random permutation of [lst] *)
let shuffle lst  =
  QCheck.Gen.(generate1 (shuffle_l lst))

(** [generate_turnorder party name] generates a random list including [party] and [name] *)
let generate_turnorder party name = 
  let lst = name::party in 
  shuffle lst 

let rec status_helper acc (lst: string list) = 
  match lst with  
  | [] -> acc
  | x::t -> status_helper ((x,[])::acc) t 

(** [init state gtl party] initializes the first state of the game. Every 
    Party memeber has full health and magic points as well as the boss. The
    turnover is the official turnover including all 4 players. Current boss is the 
    boss currently beinf played against and next boss is the boss to be played next 
    if the party succeeds. *)
let init_state gtl party = 
  let boss = start_boss gtl in
  let t_order = generate_turnorder (party_helper party []) boss in 
  {health = (boss, get_bs gtl boss)::(get_party_health party []);
   magic_points = get_party_mp party [] ; party = party_helper party [] ;
   turnorder = t_order; current_boss =boss; next_boss = next gtl boss; 
   current_fighter = List.nth t_order 0; next_fighter = List.nth t_order 1;
   status's = status_helper [] (boss::(party_helper party []))}

(** [helper name lst] is the health of [name] of the character in 
    [lst] or raises UnknownCharacter if not in [lst] *)
let rec helper name lst = 
  match lst with 
  | [] -> raise (UnknownCharacter name)
  | (n,i)::t -> if n=name then i else helper name t 

(**[helper2 name num lst acc] is [acc] with each element of [lst] but with 
   [num] of [name] changed. *)
let rec helper2 name num lst acc = 
  match lst with 
  | [] -> acc
  | (n,i)::t -> if n = name then helper2 name num t ((name,num)::acc)
    else helper2 name num t ((n,i)::acc)     

(** [set_health name num t] is [t] with the health of [name] set to [num] *)
let set_health name num t = 
  {health = List.rev (helper2 name num t.health []);magic_points = t.magic_points;
   turnorder= t.turnorder; party = t.party;current_boss=t.current_boss;next_boss=t.next_boss;
   current_fighter = t.current_fighter; next_fighter = t.next_fighter; status's = t.status's}

(** [set_magic_points name num t] is [t] with the mp of [name] set to [num] *)
let set_magic_points name num t = 
  {health = t.health;magic_points = List.rev (helper2 name num t.magic_points []);
   turnorder= t.turnorder; party=t.party;current_boss=t.current_boss;next_boss=t.next_boss;
   current_fighter = t.current_fighter; next_fighter= t.next_fighter; status's = t.status's}


(** [get_next name lst head] is [head] if [name] is the last element in [lst] else 
    it is the element following [name] in [lst]  *)
let rec get_next name lst head = 
  match lst with 
  | [] -> raise (UnknownCharacter name)
  | x::y::t -> if name = x then y else get_next name (y::t) head
  | x::t -> if name = x then head else get_next name t head    

(** [remove_from_t name lst acc] is [acc] with each element of [lst] in order 
    except with [name] removed.*)
let rec remove_helper name lst acc = 
  match lst with 
  | [] -> acc 
  | x::t -> if x=name then remove_helper name t acc else 
      x::(remove_helper name t acc)  

(** [check before name t] is [name] if [name] is still in [t] else is the next
    element in [t] after [before] *)
let check before name t =  
  if (List.mem name t) then name else 
    get_next before t (List.nth t 0)

(**[remove_from_t name state] is [state] with [name] removed from the 
   turnorder in [state] *)
let remove_from_t name state = 
  let t = remove_helper name state.turnorder [] in 
  {health = state.health; magic_points = state.magic_points; 
   turnorder = t; party = state.party; current_boss = state.current_boss; 
   next_boss= state.next_boss; current_fighter = state.current_fighter;
   next_fighter= (check state.current_fighter state.next_fighter t); status's = state.status's}      

(** [get_health name t] is the health of chracter with name [name] in 
    state [t]  *)
let get_health name t = helper name t.health 

(** [get_magic_points name t] is the magic point of [name] in 
    state [t] *)
let get_magic_points name t = helper name t.magic_points

(** [get_current_boss t] is the current boss played against in state [t] *)
let get_current_boss t = t.current_boss

(** [get_turnorder t] is the turnorder being played in state [t] *)
let get_turnorder t = t.turnorder

(** [get_party t] is the party being played in state [t] *)
let get_party t = t.party

(** [get_next_boss t] is the next_boss to be played in state [t]  *)
let get_next_boss t = t.next_boss

(** [get_current_fighter t] is the name of the character or boss whose turn it 
    currently is *)
let get_current_fighter t = t.current_fighter

(** [get_next_fighter t] is the name of the character or boss whose turn is next *)
let get_next_fighter t = t.next_fighter

(** [get_status name state] is the status effects of [name] in [state]*)
let get_status name state = helper name state.status's

(** [alive_helper lst bool boss] is [bool] if the at least one member of [lst] 
    excluding [boss] has a health > 0 *)
let rec alive_helper lst (bool:bool) boss = 
  match lst with 
  | [] -> bool 
  | (n,i)::t -> if n = boss then alive_helper t bool boss 
    else if i > 0 then alive_helper t true boss else alive_helper t bool boss 

(** [check_alive t] is true if at least one member of the party in [t] has a health
    above 0  *)
let check_alive t = 
  alive_helper t.health false t.current_boss

(**[is_dead t name] is true if [name] has a health in [t] less than or equal to 
   0 *)
let is_dead t name = 
  if (helper name t.health) <= 0 then true else false    

(**  [change_turns t] is [t] wiht the current fighter becoming the next fighter of 
     [t] and the next_fighter is the next person in the turnorder after [t.next_fighter]
     If the t has an empty turnorder this method will raise an exception. If the 
     turnoder is of size one it current and next fighter will always be the sanme*)
let change_turns t = 
  {health = t.health; magic_points = t.magic_points; turnorder = t.turnorder;
   party = t.party; current_boss = t.current_boss; next_boss = t.next_boss; 
   current_fighter = t.next_fighter; 
   next_fighter = get_next t.next_fighter t.turnorder (List.nth t.turnorder 0); 
   status's = t.status's}  

(** [find_char lst acc] is the character list [acc] with the characters whose 
    names are in [lst] in the order they are in [lst] *)
let rec find_char lst acc = 
  match lst with 
  | [] -> acc
  | x::t -> find_character x get_characters :: find_char t acc

(** [reset_state gtl l] resets the state of all the players and the new battle 
    with the next boss in [gtl] *)
let reset_state gtl t = 
  let boss = t.next_boss in
  let party = find_char t.party [] in
  let t_order = generate_turnorder t.party boss in 
  {health = (boss, get_bs gtl boss)::(get_party_health party []);
   magic_points = get_party_mp party [] ; party = t.party ;
   turnorder = t_order; current_boss = boss; next_boss = next gtl boss; 
   current_fighter = List.nth t_order 0; next_fighter = List.nth t_order 1;
   status's = status_helper [] (boss:: (party_helper party []))}  

(** [status_add name status state] is [state] with the status effect [status]
    added to [name]'s status effects *)
let status_add name status state = 
  let stats = List.sort_uniq compare (status::(get_status name state)) in 
  {health = state.health; magic_points = state.magic_points; turnorder = state.turnorder;
   party = state.party; current_boss = state.current_boss; next_boss = state.next_boss; 
   current_fighter = state.current_fighter; next_fighter = state.next_fighter; 
   status's = helper2 name stats state.status's []} 


(** [status_remove name status state] is [state] with the status effect [status]  
    removed from [name]'s status effects *)
let status_remove name status state = 
  let s = get_status name state in 
  let stats = if List.mem status s then 
      (List.filter (fun x -> x <> status) s) else s in 
  {health = state.health; magic_points = state.magic_points; turnorder = state.turnorder;
   party = state.party; current_boss = state.current_boss; next_boss = state.next_boss; 
   current_fighter = state.current_fighter; next_fighter = state.next_fighter; 
   status's = helper2 name stats state.status's []}   

let pure_status name state = 
  {health = state.health; magic_points = state.magic_points; turnorder = state.turnorder;
   party = state.party; current_boss = state.current_boss; next_boss = state.next_boss; 
   current_fighter = state.current_fighter; next_fighter = state.next_fighter; 
   status's = helper2 name [] state.status's []}  