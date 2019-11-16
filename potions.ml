open State

(** *)
type potion = 
  | Heal
  | Pure

(** *)
exception Invalid_potion

let heal_eff g (st : State.t) =
  let c = get_current_fighter st in
  let amt = 32 - Random.int 15 in
  set_health g c st (get_health c st + amt) |> change_turns g

let pure_eff g (st : State.t) =
  let c = get_current_fighter st in
  pure_status c st |> change_turns g

(** *)
let drink p = 
  match p with
  | "heal" -> Heal
  | "pure" -> Pure
  | _ -> raise Invalid_potion