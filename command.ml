type object_phrase = string

type command = 
  | Fight
  | Magic of object_phrase
  | Drink of object_phrase
  | Show
  | Quit
  | Pass

exception Malformed

exception Empty

let rec string_of_list acc = function
  | [] -> acc
  | [e] -> acc ^ e
  | h :: t -> string_of_list (h ^ ", " ^ acc) t

let is_empty = function 
  | [] -> true
  | _::_ -> false

let comm_help c t = 
  match c with
  | "fight" -> if is_empty t then Fight else raise Malformed
  | "cast" -> if is_empty t then raise Malformed else Magic (List.nth t 0)
  | "drink" -> if is_empty t then raise Malformed else Drink (List.nth t 0)
  | "magic" -> if is_empty t then Show else raise Malformed
  | "quit" -> if is_empty t then Quit else raise Malformed
  | "pass" -> if is_empty t then Pass else raise Malformed
  | _ -> raise Malformed

let get_command clst = 
  let flst = List.filter (fun x -> x <> "") clst in
  match flst with
  | [] -> raise Empty
  | c :: t -> comm_help c t

let parse str = 
  String.split_on_char ' ' str |> get_command |> function
  | Fight -> Fight
  | Magic s -> Magic s
  | Drink p -> Drink p
  | Show -> Show
  | Pass -> Pass
  | Quit -> Quit