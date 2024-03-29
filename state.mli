open Gauntlet
open Status
open Command
(** State maintains all of the information changing within the game. 
    State must know which Boss of the gauntlet the game is being played in as well as
    each indiviual party members stats including health as well as the enemy's 
    Eventually State will also need to maintain information regarding status changes 
    to the enemy and party memebers but those will be implemented later. 
    State also will hold what the turn order is for the party memebers and enemy *)

(** Represents the type of state *)
type t

(** [init_state gtl party] returns the initial state of the game with 
    gauntlet [gtl] and characters of [party] *)
val init_state : Gauntlet.t -> Party.t list -> t  

(** [get_health s t] is the health of [s] in [t] *)
val get_health : string -> t -> int  

(** [get_strength s t] is the strength of [s] in [t] *)
val get_strength : string -> t -> int  

(** [get_agil s t] is the agility of [s] in [t] *)
val get_agil : string -> t -> int  

(** [get_hit_per s t] is the hit percent of [s] in [t] *)
val get_hit_per : string -> t -> int  

(** [get_fight_def s t] is the fight defense of [s] in [t] *)
val get_fight_def : string -> t -> int  

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
val set_health: Gauntlet.t->string-> t-> int -> t

(** [set_magic_points name num t] returns [t] with new mp for [name] with
    value [num] *)
val set_magic_points: string-> int -> t->t

(** [set_strength name num t] returns [t] with new strength for [name] with
    value [num] *)
val set_strength: string-> t-> int -> t

(** [set_agil name num t] returns [t] with new agility for [name] with
    value [num] *)
val set_agil: string-> t-> int -> t

(** [set_hip_percent name num t] returns [t] with new hit_percent for [name]
    with value [num] *)
val set_hit_percent: string-> t-> int -> t

(** [set_fight_def name num t] returns [t] with new fight_def for [name] with
    value [num] *)
val set_fight_def: string-> t-> int -> t

(** [remove_from_t name t ] is [t] with [name] removed from turnorder *)
val remove_from_t: string -> t -> t 

(** [check_alive t] returns true if at least one character in the party in 
    state [t] is alive *)
val check_alive: t -> bool 

(** [is_dead t name] returns true if the character's [name] health
    is less than or equal to zero in state [t]*)
val is_dead: t->string-> bool 

(** [change_turns glt t] is [t] with the current_fighter and 
    next_fighter updated with gauntlet [glt]*)
val change_turns : Gauntlet.t -> t -> t

(** [reset_state gtl t] resets the state [t] from the previous battle to revive 
    all health and magic points; change the current boss to the next boss in the 
    gauntlet [gtl] and create a new turnorder *)
val reset_state: Gauntlet.t -> t -> t 

(** [status_add name status state] is [state] with [name] adding 
    [status] to its list of current status effects. If [name] already has 
    [status] as a current status effect, change nothing in [state] *)
val status_add: string -> Status.t -> t -> t

(** [status_remove name status state] is [state] with [name] no longer 
    having [status] as a status effect. If [name] never had [status] to begin 
    with then nothing is changed  *)
val status_remove: string -> Status.t -> t -> t

(** [get_status name t] is the current status effects of the given player [name] 
    at state [t] *)
val get_status: string -> t -> Status.t list 

(** [pure_status name state] removes all status effects from [name] and returns
    [state] *)
val pure_status: string -> t -> t

(* * [is_valid_com name t com] is true if [com] is a valid command for 
   [name] to perform. *)
val is_valid_com: string->t -> Command.command -> bool

(** [cure4_status name t] is [t] with [name] having full health and no status
    effects *)
val cure4_status: string -> t -> Gauntlet.t -> t

(**[is_poisoned name t] is true if the character [name] is Poisoned in [t] *)
val is_poisoned: string -> t -> bool

(**[is_paralyzed name t] is true if the character [name] is Paralyzed in [t] *)
val is_paralyzed: string -> t -> bool

(**[is_blinded name t] is true if the character [name] is Blinded [t] *)
val is_blinded: string -> t -> bool

(**[is_silenced name t] is true if the character [name] is Silenced in [t] *)
val is_silenced: string -> t -> bool

(**[used_heal name t] is [t] with the character's [name] heal field set to 
   false*)
val used_heal: string -> t -> t

(**[used_pure name t] is [t] with the character's [name] pure field set to 
   false*)
val used_pure: string -> t -> t

(** [has_heal name t] is true if the character [name] has potion heal left in 
    [t] *)
val has_heal: string -> t -> bool

(** [has_pure name t] is true if the character [name] has potion pure left in 
    [t] *)
val has_pure: string -> t -> bool

(** [empty_state] returns an empty state with nothing in it. All fields set 
    to empty list. *)
val empty_state: t