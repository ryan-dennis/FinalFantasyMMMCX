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
  type object_phrase 
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
  val set_health: string-> int-> t -> t
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


