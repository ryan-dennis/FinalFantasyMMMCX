open State

(** *)
type potion = 
  | Heal
  | Pure

(** *)
exception Invalid_potion

let heal_eff (st : State.t) =
  let c = get_current_fighter st in
  let amt = 32 - Random.int 15 in
  set_health c st (get_health c st + amt) |> change_turns

let pure_eff (st : State.t) =
  let c = get_current_fighter st in
  pure_status c st |> change_turns

(** *)
let drink p = 
  match p with
  | "heal" -> Heal
  | "pure" -> Pure
  | _ -> raise Invalid_potion