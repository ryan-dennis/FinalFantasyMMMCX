(** Module status hold the variant of type of the four different possible
    status effects. It also includes the functions of the different effects of
    what the added status effect will have on the character and its state. It 
    also contains the algorithms to see if the status has been removed or not 
    by chance. 
*)
(** type t represents the possible types of status effects a character can have *)
type t =  Poisoned | Blinded | Paralyzed | Silenced 

(**  *)
val effects_of_Poisoned: string -> t
val effects_of_Blinded: string -> t
val effects_of_Paralyzed: string -> t

val effects_of_Silenced: string -> t