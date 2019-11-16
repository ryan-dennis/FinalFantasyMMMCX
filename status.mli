(** Module status hold the variant of type of the four different possible
    status effects. It also contains the algorithms to see if the status 
    has been removed or not by chance. 
*)

(** type t represents the possible types of status effects a character can have *)
type t =  Poisoned | Blinded | Paralyzed | Silenced 

