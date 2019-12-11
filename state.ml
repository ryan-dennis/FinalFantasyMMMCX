(** State contains all of the temporary and changing information regarding the 
    party members and the enemy. *)
open Party 
open Gauntlet 
open Status
open Command

(** Represents the state of the game being played *)
type t = {
  health : (string*int) list; 
  magic_points : (string*int) list; 
  status's : (string *(Status.t list)) list;
  turnorder : string list;
  party : string list;
  current_boss: string;
  next_boss : string; 
  current_fighter: string;
  next_fighter: string; 
  strength : (string*int) list;
  agility: (string*int) list;
  hit_percent: (string*int) list;
  fight_defense : (string*int) list;
  pure: (string*bool) list;
  heal: (string*bool) list;
}

(** [get_party_health gtl party acc] is [acc] with the (name,vit) of every
    character in [party]  *)
let rec get_party_health party acc = 
  match party with 
  | [] -> acc 
  | x::t -> get_party_health t ((get_name x,(get_stats x).vit)::acc) 

(** [get_party_str gtl party acc] is [acc] with the (name,str) of every
    character in [party]  *)
let rec get_party_str party acc = 
  match party with 
  | [] -> acc 
  | x::t -> get_party_str t ((get_name x,(get_stats x).str)::acc) 

(** [get_party_agility gtl party acc] is [acc] with the (name,agl) of every
    character in [party]  *)
let rec get_party_agility party acc = 
  match party with 
  | [] -> acc 
  | x::t -> get_party_agility t ((get_name x,(get_stats x).agl)::acc) 

(** [get_party_hit_per gtl party acc] is [acc] with the (name,hit_percent) 
    of every character in [party]  *)
let rec get_party_hit_per party acc = 
  match party with 
  | [] -> acc 
  | x::t -> get_party_hit_per t ((get_name x,(get_stats x).hit_percent)::acc) 

(** [get_party_def gtl party acc] is [acc] with the (name,fight_def) of every
    character in [party]  *)
let rec get_party_def party acc = 
  match party with 
  | [] -> acc 
  | x::t -> get_party_def t ((get_name x,(get_stats x).fight_def)::acc) 

(** [get_party_health gtl party acc] is [acc] with the (name,mp) of every
    character in [party]  *)
let rec get_party_mp party acc = 
  match party with 
  | [] -> acc 
  | x::t -> get_party_mp t ((get_name x,(get_stats x).mp)::acc) 

(** [get_bs gtl boss] is the health of [boss] in [gtl] *)
let get_bs gtl boss = (boss_stats gtl boss).hp 

(** [get_bagl gtl boss] is the agility of [boss] in [gtl] *)
let get_bagl gtl boss = (boss_stats gtl boss).agl 

(** [get_bdef gtl boss] is the defense of [boss] in [gtl] *)
let get_bdef gtl boss = (boss_stats gtl boss).def 

(** [get_bstr gtl boss] is the strength of [boss] in [gtl] *)
let get_bstr gtl boss = (boss_stats gtl boss).str 

(** [get_bhits_per gtl boss] is the hit_percent of [boss] in [gtl] *)
let get_bhits_per gtl boss = (boss_stats gtl boss).hits_per 

(** [party_helper lst acc] is [acc] with the name of every character in 
    [lst] *)
let rec party_helper (lst: Party.t list) acc = 
  match lst with 
  | [] -> acc
  | x::t -> party_helper t (get_name x::acc)

(** [shuffle lst] generates a random permutation of [lst] *)
let shuffle lst  =
  QCheck.Gen.(generate1 (shuffle_l lst))

(** [generate_turnorder party name] generates a random list including
    [party] and [name] *)
let generate_turnorder party name = 
  let lst = name::party in 
  shuffle lst 

(** [status_helper acc lst] creates the intial status  *)
let rec status_helper acc (lst: string list) = 
  match lst with  
  | [] -> acc
  | x::t -> status_helper ((x,[])::acc) t 

(** [init_pot lst] is an assoc list with each name in [lst] paired with true *)
let rec init_pot = function
  | [] -> []
  | h::t -> (h,true):: init_pot t

(** [init state gtl party] initializes the first state of the game. Every 
    Party memeber has full health and magic points as well as the boss. The
    turnover is the official turnover including all 4 players. Current boss is
    the boss currently beinf played against and next boss is the boss to be 
    played next if the party succeeds. *)
let init_state gtl party = 
  let boss = start_boss gtl in
  let t_order = generate_turnorder (party_helper party []) boss in 
  {health = (boss, get_bs gtl boss)::(get_party_health party []);
   magic_points = get_party_mp party [] ; party = party_helper party [] ;
   turnorder = t_order; current_boss =boss; next_boss = next gtl boss; 
   current_fighter = List.nth t_order 0; next_fighter = List.nth t_order 1;
   status's = status_helper [] (boss::(party_helper party []));
   strength = (boss, get_bstr gtl boss)::(get_party_str party []);
   agility = (boss, get_bagl gtl boss)::(get_party_agility party []);
   hit_percent = (boss, get_bhits_per gtl boss)::(get_party_hit_per party []);
   fight_defense = (boss, get_bdef gtl boss)::(get_party_def party []);
   pure = init_pot (party_helper party []);
   heal = init_pot (party_helper party []);
  }

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

(** [max_hp name t gtl] is the maximal hp [name] can have.(the starting health)*)
let max_hp name t gtl = 
  if name = t.current_boss then get_bs gtl name
  else find_character name get_characters |> char_og_health           

(** [set_health name num t] is [t] with the health of [name] set to [num] *)
let set_health gtl name t num = 
  let hp = max_hp name t gtl in 
  {health = List.rev (helper2 name (if num< 0 then 0 else if 
                                      num > hp then hp else  num) t.health []);
   magic_points = t.magic_points;
   turnorder= t.turnorder; party = t.party;current_boss=t.current_boss;
   next_boss=t.next_boss;current_fighter = t.current_fighter; 
   next_fighter = t.next_fighter; status's = t.status's;strength = t.strength;
   agility = t.agility;hit_percent = t.hit_percent; 
   fight_defense = t.fight_defense; pure = t.pure; heal = t.heal}

(** [set_magic_points name num t] is [t] with the mp of [name] set to [num] *)
let set_magic_points name num t = 
  {health = t.health;
   magic_points = List.rev (helper2 name (if num< 0 
                                          then 0 else num) t.magic_points []); 
   turnorder= t.turnorder; party=t.party;
   current_boss=t.current_boss;next_boss=t.next_boss;
   current_fighter = t.current_fighter; next_fighter= t.next_fighter; 
   status's = t.status's;strength = t.strength; agility = t.agility; 
   hit_percent = t.hit_percent; fight_defense = t.fight_defense; pure=t.pure; 
   heal = t.heal}

(** [set_strength name num t] is [t] with the strength of [name] set to [num] *)
let set_strength name t num = 
  {health = t.health;magic_points = t.magic_points;
   turnorder= t.turnorder; party = t.party;current_boss=t.current_boss;
   next_boss=t.next_boss;current_fighter = t.current_fighter; 
   next_fighter = t.next_fighter; status's = t.status's;
   strength =  List.rev (helper2 name (if num< 0 then 0 else num) t.strength []);
   agility = t.agility;hit_percent = t.hit_percent; 
   fight_defense = t.fight_defense; pure=t.pure; heal = t.heal}

(** [set_agil name num t] is [t] with the agil of [name] set to [num] *)
let set_agil name t num = 
  {health = t.health; magic_points = t.magic_points;
   turnorder= t.turnorder; party = t.party;current_boss=t.current_boss;
   next_boss=t.next_boss;current_fighter = t.current_fighter; 
   next_fighter = t.next_fighter; status's = t.status's;
   strength = t.strength; 
   agility =  List.rev (helper2 name (if num< 0 then 0 else num) t.agility []);
   hit_percent = t.hit_percent; fight_defense = t.fight_defense; pure=t.pure; 
   heal = t.heal}

(** [set_hip_percent name num t] is [t] with the hit_percent of [name] set 
    to [num] *)
let set_hit_percent name t num = 
  {health = t.health;magic_points = t.magic_points;
   turnorder= t.turnorder; party = t.party;current_boss=t.current_boss;
   next_boss=t.next_boss;current_fighter = t.current_fighter; 
   next_fighter = t.next_fighter; status's = t.status's;
   strength = t.strength; agility = t.agility;
   hit_percent = List.rev (helper2 name (if num< 0 
                                         then 0 else num) t.hit_percent []);
   fight_defense = t.fight_defense;pure=t.pure; 
   heal = t.heal}

(** [set_fight_def name num t] is [t] with the fight_defense of [name] set to [num] *)
let set_fight_def name t num = 
  {health = t.health ;magic_points = t.magic_points;
   turnorder= t.turnorder; party = t.party;current_boss=t.current_boss;
   next_boss=t.next_boss;current_fighter = t.current_fighter; 
   next_fighter = t.next_fighter; status's = t.status's;
   strength = t.strength; agility = t.agility;
   hit_percent = t.hit_percent; 
   fight_defense =  List.rev (helper2 name (if num< 0 
                                            then 0 else num) t.fight_defense []);
   pure=t.pure; heal = t.heal
  }         

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
let check current next t old =  
  if (List.mem next t) then next else 
  if (List.mem current t) then get_next current t (List.nth t 0) else
    get_next current old (List.nth t 0)

(**[remove_from_t name state] is [state] with [name] removed from the 
   turnorder in [state] *)
let remove_from_t name state = 
  let t = remove_helper name state.turnorder [] in 
  {health = state.health; magic_points = state.magic_points; 
   turnorder = t; party = state.party; current_boss = state.current_boss; 
   next_boss= state.next_boss; current_fighter = state.current_fighter;
   next_fighter= (check state.current_fighter state.next_fighter t state.turnorder); 
   status's = state.status's;strength = state.strength; agility = state.agility; 
   hit_percent = state.hit_percent; fight_defense = state.fight_defense; 
   pure=state.pure; heal = state.heal}      

(** [get_health name t] is the health of chracter with name [name] in 
    state [t]  *)
let get_health name t = helper name t.health 

(** [get_magic_points name t] is the magic point of [name] in 
    state [t] *)
let get_magic_points name t = helper name t.magic_points

(** [get_strength name t] is the strength of chracter with name [name] in 
    state [t]  *)
let get_strength name t = helper name t.strength

(** [get_agil name t] is the agility of chracter with name [name] in 
    state [t]  *)
let get_agil name t = helper name t.agility

(** [get_hit_per name t] is the hit_percent of chracter with name [name] in 
    state [t]  *)
let get_hit_per name t = helper name t.hit_percent

(** [get_fight_def name t] is the fight defense of chracter with name [name] in 
    state [t]  *)
let get_fight_def name t = helper name t.fight_defense 

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

(** [get_next_fighter t] is the name of the character or boss whose turn is 
    next *)
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

(** [check_alive t] is true if at least one member of the party in [t] has a 
    health above 0  *)
let check_alive t = 
  alive_helper t.health false t.current_boss

(**[is_dead t name] is true if [name] has a health in [t] less than or equal to 
   0 *)
let is_dead t name = 
  if (helper name t.health) <= 0 then true else false 

(** [status_remove name status state] is [state] with the status effect [status]  
    removed from [name]'s status effects *)
let status_remove name status state = 
  let s = get_status name state in 
  let stats = if List.mem status s then 
      (List.filter (fun x -> x <> status) s) else s in 
  {health = state.health; magic_points = state.magic_points; 
   turnorder = state.turnorder;party = state.party; 
   current_boss = state.current_boss; next_boss = state.next_boss; 
   current_fighter = state.current_fighter; next_fighter = state.next_fighter; 
   status's = helper2 name stats state.status's [];strength = state.strength; 
   agility = state.agility; hit_percent = state.hit_percent; 
   fight_defense = state.fight_defense; pure= state.pure; heal = state.heal}     

(** [is_poisoned name t] is true if [name] has status effect Poisoned in 
    state [t] *)
let is_poisoned name t = 
  List.mem Poisoned (get_status name t) 

(** [is_paralysed name t] is true if [name] has status effect Paralyzed in
    state [t] *)
let is_paralyzed name t = 
  List.mem Paralyzed (get_status name t)      

(** [is_blinded name t] is true if [name] has status effect Blinded in 
    state [t] *)
let is_blinded name t = 
  List.mem Blinded (get_status name t)  

(** [is_silenced name t] is true if [name] has status effect Silenced in
    state [t] *)
let is_silenced name t = 
  List.mem Silenced (get_status name t)  

(** [cure_para name t] is [t] with Paralyzed remove by random chance. if [name]
    is a PC then 25% chance and if [name] is an enemy then 10% chance *)
let cure_para name t = 
  let num = Random.int 101 in 
  if name = t.current_boss then 
    (if num <= 10 then status_remove name Paralyzed t else t)
  else if num > 25 && num <= 50 then status_remove name Paralyzed t 
  else t 

(** [status_effects name t] is [t] with the status effects of [name] taken care 
    of (Poison subtracts 20 Hp and Paralyzed removed by random) *)
let status_effects gtl name t = 
  if is_poisoned name t && is_paralyzed name t then 
    (get_health name t)-20 |> set_health gtl name t |> cure_para name 
  else if is_poisoned name t then set_health gtl name t ((get_health name t)-20) 
  else if is_paralyzed name t then cure_para name t else t  

(**  [change_turns t] is [t] wiht the current fighter becoming the next fighter 
     of [t] and the next_fighter is the next person in the turnorder after 
     [t.next_fighter] If the t has an empty turnorder this method will raise 
     an exception. If the turnoder is of size one it current and next fighter 
     will always be the sanme*)
let change_turns gtl t = 
  let n = status_effects gtl t.next_fighter t in 
  {health = n.health; magic_points = n.magic_points; turnorder = n.turnorder;
   party = n.party; current_boss = n.current_boss; next_boss = n.next_boss; 
   current_fighter = n.next_fighter; 
   next_fighter = get_next n.next_fighter n.turnorder (List.nth n.turnorder 0); 
   status's = n.status's;strength = n.strength;  agility = n.agility; 
   hit_percent = n.hit_percent; fight_defense = n.fight_defense; pure=t.pure; 
   heal = t.heal}  

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
   status's = status_helper [] (boss:: (party_helper party []));
   strength = (boss, get_bstr gtl boss)::(get_party_str party []);
   agility = (boss, get_bagl gtl boss)::(get_party_agility party []);
   hit_percent = (boss, get_bhits_per gtl boss)::(get_party_hit_per party []);
   fight_defense = (boss, get_bdef gtl boss)::(get_party_def party []);
   heal = init_pot t.party; pure = init_pot t.party}  

(** [status_add name status state] is [state] with the status effect [status]
    added to [name]'s status effects *)
let status_add name status state = 
  let stats = List.sort_uniq compare (status::(get_status name state)) in 
  {health = state.health; magic_points = state.magic_points; 
   turnorder = state.turnorder;party = state.party; 
   current_boss = state.current_boss; next_boss = state.next_boss; 
   current_fighter = state.current_fighter; next_fighter = state.next_fighter; 
   status's = helper2 name stats state.status's []; strength = state.strength; 
   agility = state.agility; hit_percent = state.hit_percent;
   fight_defense = state.fight_defense;pure = state.pure; heal = state.heal} 

(** [pure_status nmae state] is [state] but with [name] has no status effects *)
let pure_status name state = 
  {health = state.health; magic_points = state.magic_points; 
   turnorder = state.turnorder;party = state.party; 
   current_boss = state.current_boss; next_boss = state.next_boss; 
   current_fighter = state.current_fighter; next_fighter = state.next_fighter; 
   status's = helper2 name [] state.status's []; strength = state.strength; 
   agility = state.agility; hit_percent = state.hit_percent; 
   fight_defense = state.fight_defense; pure = state.pure; heal = state.heal}

(** [cure4_status name state] is [state] after [name]'s health has been revived 
    and status effects have been removed. *)
let cure4_status name state gtl  = 
  let h = max_hp name state gtl in   
  {health = List.rev (helper2 name h state.health []); 
   magic_points = state.magic_points; turnorder = state.turnorder;
   party = state.party; current_boss = state.current_boss; 
   next_boss = state.next_boss; current_fighter = state.current_fighter; 
   next_fighter = state.next_fighter; 
   status's = helper2 name [] state.status's []; strength = state.strength; 
   agility = state.agility; hit_percent = state.hit_percent; 
   fight_defense = state.fight_defense;
   pure = state.pure; heal = state.heal}

(** [is_valid_com name t com] is true if [name] does not have a status effect
    preventing them to perform [com], false otherwise. *)
let is_valid_com name t com = 
  let st = get_status name t in 
  match com with 
  | Fight -> if is_paralyzed name t then false else true 
  | Magic n -> if is_paralyzed name t then false else if 
      List.mem Silenced st then false else true 
  | Drink n -> if is_paralyzed name t then false else if 
      List.mem Silenced st then false else true
  | _ -> true    

(**[used_heal name t] is [t] with field heal of [name] set to [false]*)
let used_heal name t = 
  {health = t.health; magic_points = t.magic_points;
   turnorder= t.turnorder; party = t.party;current_boss=t.current_boss;
   next_boss=t.next_boss;current_fighter = t.current_fighter; 
   next_fighter = t.next_fighter; status's = t.status's;
   strength = t.strength; agility = t.agility;
   hit_percent = t.hit_percent; fight_defense = t.fight_defense; pure=t.pure; 
   heal = helper2 name false t.heal []}

(**[used_pure name t] is [t] with field pure of [name] set to [false]*)
let used_pure name t = 
  {health = t.health; magic_points = t.magic_points;
   turnorder= t.turnorder; party = t.party;current_boss=t.current_boss;
   next_boss=t.next_boss;current_fighter = t.current_fighter; 
   next_fighter = t.next_fighter; status's = t.status's;
   strength = t.strength; agility = t.agility; hit_percent = t.hit_percent; 
   fight_defense = t.fight_defense; pure= helper2 name false t.pure []; 
   heal = t.heal} 

(** [has_heal name t] is [true] if [name] has potion heal*)
let has_heal name t = 
  List.assoc name t.heal

(** [has_pure name t] is [true] if [name] has potion pure *)
let has_pure name t = 
  List.assoc name t.pure        

(** [empty_state] is an empty state *)
let empty_state = 
  {health = []; magic_points = []; turnorder= []; party = [];
   current_boss= "";next_boss= "";current_fighter = ""; next_fighter = ""; 
   status's = []; strength = []; agility = []; hit_percent = []; 
   fight_defense = []; pure= []; heal = []} 