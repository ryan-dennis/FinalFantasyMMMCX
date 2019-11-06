open Gauntlet
(** State maintains all of the information changing within the game. 
    State must know which Boss of the gauntlet the game is being played in as well as
    each indiviual party members stats including health as well as the enemy's 
    Eventually State will also need to maintain information regarding status changes 
    to the enemy and party memebers but those will be implemented later. 
    State also will hold what the turn order is for the party memebers and enemy *)

type t

(** [init_state] returns the initial state of the game *)
val init_state : Gauntlet.t -> Party.t list -> t  

(** [get_health s t] is the health of [s] in [t] *)
val get_health : string -> t -> int  

(** [get_magic_points s t] is the mp of [s] in [t] *)
val get_magic_points : string -> t -> int 

(** [get_current_boss t] is the boss being fought against in [t] *)
val get_current_boss : t -> Gauntlet.boss_id

(** [get_turnovert] is the turnorder in [t] *)
val get_turnorder : t -> string list

(** [get_next_boss t] is the next_boss to be played of [t] *)
val get_next_boss: t -> Gauntlet.boss_id 

(**[get_party t] is the character names of the character's in the party [t] *)
val get_party: t -> string list

(** [get_next_fighter t] is the next_fighter of state [t] *)
val get_next_fighter : t -> string 

(** [get_current_fighter t] is the current fighter of state [t] *)
val get_current_fighter: t -> string 

(** [set_health name num t] returns [t] with new health for [name] with
    value [num] *)
val set_health: string-> int-> t -> t

(** [set_magic_points name num t] returns [t] with new mp for [name] with
    value [num] *)
val set_magic_points: string-> int -> t->t

(** [remove_from_t name t ] is [t] with [name] removed from turnorder *)
val remove_from_t: string -> t -> t 

(** [check_alive t] returns true if at least one character in the party in 
    state [t] is alive *)
val check_alive: t -> bool 

(** [is_dead] returns true if the character's health is less than or equal to zero *)
val is_dead: t->string-> bool 

(** [change_turns t] is [t] with the current_fighter and next_fighter updated *)
val change_turns : t -> t

(** [reset_state gtl t] resets the state from the previous battle to revive all 
    health and magic points; change the current boss to the next boss in the gauntlet
    and create a new turnorder *)
val reset_state: Gauntlet.t -> t -> t 


