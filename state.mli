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


