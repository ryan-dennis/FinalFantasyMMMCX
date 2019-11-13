open State

(** *)
type potion = 
  | Heal
  | Pure

(** *)
exception Malformed

let heal_eff (st : State.t) =
  let c = get_current_fighter st in
  let amt = 32 - Random.int 15 in
  let new_st = set_health c (get_health c st + amt) st |> change_turns in
  new_st

let pure_eff (st : State.t) =
  let c = get_current_fighter st in
  let amt = 32 - Random.int 15 in
  let new_st = set_health c (get_health c st + amt) st |> change_turns in
  new_st

(** *)
let drink st sp = 
  match sp with
  | "heal" -> heal_eff st
  | "pure" -> pure_eff st
  | _ -> raise Malformed