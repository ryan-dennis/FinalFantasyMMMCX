open State

(** [potion b] represents the potion with bool [b] as true if it can be used, 
    or false if it has already been used (i.e. only one use per potion per 
    character) *)
type potion = 
  | Heal
  | Pure

(** [Invalid_potion] is the exception thrown when the potion does not exist. *)
exception Invalid_potion

(** [heal_eff g st] is the state resulting from using a heal potion in 
    state [st] and gauntlet [g]. *)
let heal_eff g (st : State.t) =
  let c = get_current_fighter st in
  let amt = 150 - Random.int 51 in
  set_health g c st (get_health c st + amt) |> change_turns g

(** [pure_eff g st] is the state resulting from using a pure potion in 
    state [st] and gauntlet [g]. *)
let pure_eff g (st : State.t) =
  let c = get_current_fighter st in
  pure_status c st |> change_turns g

(** [drink p] is the [potion] type corresponding to [p]. 
    Raises: Invalid_potion if [p] is not a [potion] type. *)
let drink p = 
  match p with
  | "heal" -> Heal 
  | "pure" -> Pure
  | _ -> raise Invalid_potion