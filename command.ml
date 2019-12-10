type object_phrase = string

type command = 
  | Fight
  | Magic of object_phrase
  | Drink of object_phrase
  | Show
  | Quit
  | Pass

(** [Malformed] is the exception thrown when the command does not exist. *)
exception Malformed

(** [Empty] is the exception thrown when the input command is empty. *)
exception Empty

(** [is_empty lst] is true if the list [lst] has no elements. False otherwise.*)
let is_empty = function 
  | [] -> true
  | _::_ -> false

(** [comm_help c t] is the [command] type corresponding to [c]. *)
let comm_help c t = 
  match c with
  | "fight" -> if is_empty t then Fight else raise Malformed
  | "cast" -> if is_empty t then raise Malformed else Magic (List.nth t 0)
  | "drink" -> if is_empty t then raise Malformed else Drink (List.nth t 0)
  | "magic" -> if is_empty t then Show else raise Malformed
  | "quit" -> if is_empty t then Quit else raise Malformed
  | "pass" -> if is_empty t then Pass else raise Malformed
  | _ -> raise Malformed

(** [get_command clst] is the command resulting from the first non-empty string
    in the list [clst]. 
    Raises: Empty exception if [clst] is empty or contains only spaces.*)
let get_command clst = 
  let flst = List.filter (fun x -> x <> "") clst in
  match flst with
  | [] -> raise Empty
  | c :: t -> comm_help c t

(** [parse str] is the command resulting from the input string [str]. 
    Raises: Empty if [str] is the empty string or contains only spaces. 
    Raises: Malformed if if the command is malformed. A command
    is {i malformed} if the input is not a [command] type.*)
let parse str = 
  String.split_on_char ' ' str |> get_command |> function
  | Fight -> Fight
  | Magic s -> Magic s
  | Drink p -> Drink p
  | Show -> Show
  | Pass -> Pass
  | Quit -> Quit