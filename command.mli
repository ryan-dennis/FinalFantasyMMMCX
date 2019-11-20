(** Command module takes in a command from the user and parses the command 
    resulting in either a exception or returns a command.
    edit more :) TODO *)

(** TODO Ryan you can do this *)
type object_phrase = string

(** type command represents the valid commands the user can call *)
type command =
  | Fight
  | Magic of object_phrase
  | Drink of object_phrase
  | Show
  | Quit
  | Pass

(** Raised when a malformed command is encountered *)
exception Malformed 

(** Raised when an empty command is parsed *)
exception Empty

(** [parse s] takes in a string that is either a validly formed command or an 
    invalid command. A command can be invalid in two ways. If the string is empty or 
    a list of spaces it will raise Empty. If the command is a string with characters 
    but is malformed then parse will raise exception Malformed. If the string is a 
    valid command, then the valid command will be returned *)
val parse: string -> command 