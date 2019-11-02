module type PartySig = sig 
  type t 
  type sprite = (string list) list
  type spell = string
  type stat
  exception UnknownCharacter of string

  val get_spells : t -> spell list 
  val get_stats : t -> stat
  val get_characters : t list
  val get_weapon: t -> string
  val add : string list -> t list -> t list
  val get_sprite: t -> sprite 
  val get_name: t -> string 

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


