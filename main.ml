open Party
open Command
open Gauntlet
open State
open Battle

let rec string_of_list acc = function
  | [] -> acc
  | [e] -> acc ^ e
  | h :: t -> string_of_list (h ^ ", " ^ acc) t

(** [char_names] is the list of names of all characters in the game. *)
let char_names = List.map (get_name) get_characters

(** [reject msg] prints a response to the terminal indicating that there is an
    invalid property [msg]. *)
let reject msg = ANSITerminal.(print_string [red] ("Invalid " ^ msg ^ ". Pick another.\n"))

(** [select_party acc] is the list of character names selected according to user
    input. *)
let rec select_party (acc : string list) : string list = 
  ANSITerminal.(print_string [magenta] ("Current party: " ^ (string_of_list "" acc) ^ "\n"));
  match List.length acc with
  | 3 -> acc
  | _ -> print_string "> "; 
    let try_char = read_line () in
    if List.mem try_char char_names && not (List.mem try_char acc) 
    then select_party (try_char::acc)
    else let _ = reject "character" in select_party acc

(** [stat_str plst s acc] is the list string concatenation of [name: hp] values
    in the current state s.
    Requires: plst is a list of valid character names. *)
let rec stat_str (plst : string list) s (acc : string) : string =
  let curr_name = List.nth plst 0 in
  let hp = get_health curr_name s |> string_of_int in
  let str = curr_name ^ ": " ^ hp in
  match plst with
  | [] -> acc
  | [_] -> acc ^ str
  | _ :: t -> stat_str t s (acc ^ str ^ " • ")

let attack_response (b:Battle.t) (curr:string) : string =
  let damage = string_of_int (dmg b) in
  let hits = string_of_int (num_hits b) in 
  let targ = target b in 
  curr ^ " attacked " ^ targ ^ " " ^ hits ^ " times for " ^ damage ^ " damage.\n\n"

(** *)
let rec repl g s = 
  (* print current game display *)
  if not (check_alive s) then 
    (ANSITerminal.(print_string [red] "\nAll party members are dead, you lost!\n\n"); 
     exit 0)
  else
    let boss = get_current_boss s in 
    if is_dead s boss && boss = final g then 
      (ANSITerminal.(print_string [Blink; green] ("\n" ^ dialogue g boss ^ "\n\n")); 
       exit 0)
    else if is_dead s boss then 
      (ANSITerminal.(print_string [Blink; green] ("\n" ^ dialogue g boss ^ "\n\n")); 
       reset_state g s |> repl g) 
    else
      (* ignore(Sys.command "clear"); *)
      let curr = get_current_fighter s in
      let state_party = get_party s in
      let state_hp = stat_str state_party s "" in
      ANSITerminal.(print_string [Underlined; cyan] "Party:\n");
      ANSITerminal.(print_string [cyan] (state_hp ^ "\n\n"));
      ANSITerminal.(print_string [yellow] (boss ^ " health: " ^ string_of_int (get_health boss s) ^ "\n\n"));
      if List.mem curr char_names then
        let curr_char = find_character curr get_characters in
        let _ = print_string ("Enter a command for " ^ curr ^ ": ") in
        match parse (read_line ()) with
        | Fight -> let b = fight g s curr_char in
          let s' = new_st b in 
          ANSITerminal.(print_string [green] ("\n" ^ attack_response b curr)); 
          repl g s'
        | Magic spell -> let s' = fight g s curr_char |> new_st in 
          ANSITerminal.(print_string [green] ("\nThe " ^ curr ^ " cast a spell!\n\n")); 
          repl g s'
        | Drink pot -> ANSITerminal.(print_string [green] ("\nThe " ^ curr ^ " healed.\n\n")); 
          repl g s
        | Show -> let spell_str = get_spells curr_char |> string_of_list "" in 
          ANSITerminal.(print_string [green] ("\nThe " ^ curr ^ "'s spells: " ^ spell_str ^ "\n\n")); repl g s
        | Quit -> ANSITerminal.(print_string [red] "\nQuiting game...\n\n"); 
          (* ignore(Sys.command "printf '\\e[8;24;80t'");  *)
          exit 0
        | exception Malformed -> ANSITerminal.(print_string [red] "\nInvalid command. Try another.\n\n"); 
          repl g s
        | exception Empty -> ANSITerminal.(print_string [red] "Please type a command.\n\n"); 
          repl g s
          (* else (ignore(Sys.command "clear");  *)
      else (
        (* ANSITerminal.(print_string [yellow] ("" ^ curr ^ " attacked.\n\n")); *)
        let b = boss_turn g s in 
        let s' = new_st b in 
        ANSITerminal.(print_string [yellow] (attack_response b boss));
        repl g s')

let rec game_start f = 
  try let gaunt = f |> Yojson.Basic.from_file |> from_json in 
    ignore(Sys.command "clear");
    ANSITerminal.(print_string [green] "\nEnter three of the following characters that you wish to add to your party:\n");
    ANSITerminal.(print_string [green] (string_of_list "" char_names));
    print_endline "\n";
    let game_party_names = select_party [] in
    let game_party_chars = List.map (fun x -> find_character x get_characters) game_party_names in
    let start = init_state gaunt game_party_chars in 
    ANSITerminal.(print_string [cyan] ("\n" ^ start_dialogue gaunt ^ "\n\n"));
    repl gaunt start 
  with Sys_error m -> reject "file"; print_string "> "; game_start (read_line ())

let main () = 
  (* ignore(Sys.command "printf '\\e[8;68;200t'"); *)
  ignore(Sys.command "clear");
  ANSITerminal.(print_string [Bold; red] "\n\nFINAL FANTASY MMMCX\n");
  print_endline "Enter a gauntlet file name to play.\n";
  print_string "> ";
  match read_line () with 
  | exception End_of_file -> ()
  | file_name when file_name = "quit" -> exit 0
  | file_name -> game_start file_name

let () = main ()