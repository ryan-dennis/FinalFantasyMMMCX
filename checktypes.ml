module type PartySig = sig 
  type t 
  type sprite = string list
  type spell = string
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
  exception UnknownCharacter of string

  val get_spells : t -> spell list 
  val get_stats : t -> stat
  val get_characters : t list
  val get_weapon: t -> string
  val add : string list -> t list -> t list
  val get_sprite: t -> sprite 
  val get_name: t -> string 
  val find_character: string -> t list -> t

end 

module PartyCheck : PartySig = Party

module type CommandSig = sig 
  type command
  type object_phrase = string
  exception Empty
  exception Malformed 
  val parse: string -> command 
end 

module CommandCheck : CommandSig = Command


module type StateSig = sig 
  type t 
  val init_state : Gauntlet.t -> Party.t list -> t 
  val get_health : string -> t -> int 
  val get_magic_points : string -> t -> int 
  val get_current_boss : t -> Gauntlet.boss_id 
  val get_turnorder : t -> string list
  val get_next_boss: t -> Gauntlet.boss_id
  val set_health: string-> t -> int -> t
  val set_magic_points: string-> int -> t->t
  val check_alive: t -> bool
  val get_party: t -> string list
  val is_dead: t->string-> bool 
  val remove_from_t: string -> t -> t 
  val change_turns : t -> t
  val get_current_fighter: t -> string 
  val get_next_fighter : t -> string 
  val reset_state: Gauntlet.t -> t -> t 

end 

module StateCheck : StateSig = State

module type GauntletSig = sig 
  type t
  type boss_id = string
  type sprite = string list
  type stats = {
    hp : int;
    agl : int;
    def : int;
    mdef : int;
    str : int;
    hit : int;
    hits_per : int;
    weak : string list;
    resist : string list
  }
  exception UnknownBoss of boss_id
  exception InvalidSprite of sprite
  val from_json : Yojson.Basic.t -> t
  val start_boss : t -> boss_id
  val start_dialogue : t -> string
  val final : t -> boss_id
  val boss_stats : t -> boss_id -> stats
  val boss_sprite : t -> boss_id -> string list
  val boss_spell_chance : t -> boss_id -> int
  val boss_spells : t -> boss_id -> string list
  val boss_skill_chance : t -> boss_id -> int
  val boss_skills : t -> boss_id -> string list
  val next : t -> boss_id -> boss_id
  val dialogue : t -> boss_id -> string
end 

module GuantletCheck : GauntletSig = Gauntlet

module type BattleSig = sig 
  type t
  val fight : Gauntlet.t -> State.t -> Party.t -> t
  val boss_turn : Gauntlet.t -> State.t -> t
  val num_hits : t -> int
  val dmg : t -> int
  val target : t -> string
  val new_st : t -> State.t
end 

module BattleCheck : BattleSig = Battle 

