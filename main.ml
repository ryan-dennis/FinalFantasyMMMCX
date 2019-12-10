open Party
open Command
open Gauntlet
open State
open Battle
open Potions
open Status
open Spells
open Display

let boss_name_pos = 43

let color_helper h = 
  match h with 
  | "cyan" -> [ANSITerminal.cyan]
  | "yellow" -> [ANSITerminal.yellow]
  | "black" -> [ANSITerminal.black]
  | "white" -> [ANSITerminal.white]
  | "red" -> [ANSITerminal.red]
  | "blue" -> [ANSITerminal.blue]
  | "background white" -> [Background White]
  | _ -> [ANSITerminal.default]

let rec helper string color int x y= 
  match string with 
  | [] -> ()
  | h:: t -> ANSITerminal.set_cursor x y; 
    ANSITerminal.(print_string (color_helper (List.nth color int)) h);
    helper t color (int+1) (x+1) y

let rec pr s x y= 
  match s with 
  | [] -> ()
  | (s1,lst)::t -> helper s1 lst 0 x y; pr t x (y+1)

let rec line_print s slst y = 
  ANSITerminal.set_cursor 31 y;
  if (String.length s) <= 46 then ANSITerminal.(print_string slst s)
  else (ANSITerminal.(print_string slst (String.sub s 0 46)); 
        line_print (String.sub s 46 ((String.length s) - 46)) slst) (y + 1)

(** *) 
let minisleep (sec: float) s =
  print_endline s;
  ignore(Sys.command "tput civis");
  let tios = Unix.tcgetattr Unix.stdin in 
  flush stdout;
  tios.c_echo <- false;
  Unix.tcsetattr Unix.stdin TCSANOW tios;
  let tios = Unix.tcgetattr Unix.stdin in
  Unix.tcflush Unix.stdout TCIOFLUSH;
  ignore (Unix.select [] [] [] sec);
  Unix.tcflush Unix.stdout TCIOFLUSH;
  ANSITerminal.set_cursor 31 46;
  print_endline "";
  tios.c_echo <- true;
  Unix.tcsetattr Unix.stdin TCSANOW tios;
  ignore(Sys.command "tput cnorm")

(** *)
let wait_no_cursor () = 
  ignore(Sys.command "tput civis");
  let tios = Unix.tcgetattr Unix.stdin in 
  flush stdout;
  tios.c_echo <- false;
  Unix.tcsetattr Unix.stdin TCSANOW tios;
  let tios = Unix.tcgetattr Unix.stdin in
  Unix.tcflush Unix.stdout TCIOFLUSH;
  ignore(read_line ()); 
  Unix.tcflush Unix.stdout TCIOFLUSH;
  tios.c_echo <- true;
  Unix.tcsetattr Unix.stdin TCSANOW tios;
  ignore(Sys.command "tput cnorm")

(** *)
let rec string_of_list acc = function
  | [] -> acc
  | [e] -> acc ^ e
  | h :: t -> string_of_list (h ^ ", " ^ acc) t

(** *)
let rec print_spr lst x y =
  match lst with
  | [] -> ANSITerminal.set_cursor 100 100
  | h::t -> (ANSITerminal.set_cursor x y; print_endline h; print_spr t x (y+1))

(** *)
let setup_stats s c x y = 
  let hp = get_health c s in
  let mp = get_magic_points c s in
  ANSITerminal.set_cursor x y;
  ANSITerminal.(print_string [Bold; cyan] (String.capitalize_ascii c));
  ANSITerminal.set_cursor x (y+2);
  ANSITerminal.(print_string [default] "HP: "); 
  ANSITerminal.(print_string [if hp > 200 then green else if hp > 100 then yellow
                              else red] (string_of_int hp));
  ANSITerminal.set_cursor x (y+3);
  ANSITerminal.(print_string [default] "MP: "); ANSITerminal.(print_string [blue] (string_of_int mp));
  ANSITerminal.set_cursor 100 100

(** *)
let setup_boss_stats s b x y = 
  let hp = get_health b s in
  ANSITerminal.set_cursor x y;
  ANSITerminal.(print_string [Bold; yellow] (String.capitalize_ascii b));
  ANSITerminal.set_cursor x (y+2);
  ANSITerminal.(print_string [default] "HP: "); 
  ANSITerminal.(print_string [if hp < 100 then red else green] (string_of_int hp));
  ANSITerminal.set_cursor 100 100

(** *)
let string_of_status = function
  | Poisoned -> "poisoned"
  | Blinded -> "blinded"
  | Paralyzed -> "paralyzed"
  | Silenced -> "silenced"

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

(** *)
let drink_comm g s curr pot = 
  match pot with
  | Heal -> if has_heal curr s then
      (ANSITerminal.set_cursor 31 45; 
       ANSITerminal.(print_string [green] ("The " ^ curr ^ " healed.\n\n")); 
       minisleep 1.5 ""; used_heal curr s |> heal_eff g)
    else (ANSITerminal.set_cursor 31 45;
          ANSITerminal.(print_string [red] ("The " ^ curr ^ " is out of heal potions!\n\n")); 
          minisleep 1.5 ""; s)
  | Pure -> if has_pure curr s then
      (ANSITerminal.set_cursor 31 45;
       ANSITerminal.(print_string [green] ("The " ^ curr ^ " purified their status effects.\n\n"));
       minisleep 1.5 ""; used_pure curr s |> pure_eff g)
    else (ANSITerminal.set_cursor 31 45;
          ANSITerminal.(print_string [red] ("The " ^ curr ^ " is out of pure potions!\n\n")); 
          minisleep 1.5 ""; s)

(** *)
let magic_help g s spell ch = 
  ANSITerminal.set_cursor 31 44;
  ANSITerminal.(print_string [white] "Enter the target for the spell: ");
  let sp_t = get_spell spell in
  let tar = read_line () in
  let curr = get_current_fighter s in
  if is_valid_target s sp_t tar && has_spell ch spell && is_enough_mp sp_t ch s then 
    (let b = magic g s spell ch tar in
     let msg = (desc b) ^ "\n" in 
     (* ANSITerminal.set_cursor 31 52;
        ANSITerminal.(print_string [green] msg); *)
     line_print msg [ANSITerminal.green] 46; minisleep 1.5 "";  new_st b)
  else if not (has_spell ch spell) then 
    (* (ANSITerminal.set_cursor 31 52;
       ANSITerminal.(print_string [red] ("The " ^ curr ^ " does not have that spell! Pick another.\n\n"));  *)
    (line_print ("The " ^ curr ^ " does not have that spell! Pick another.\n\n")
       [ANSITerminal.red] 46; minisleep 1.5 ""; s)
  else if not (is_enough_mp sp_t ch s) then
    (* (ANSITerminal.set_cursor 31 52;
       ANSITerminal.(print_string [red] ("The " ^ curr ^ " does not have enough magic points! Pick another spell.\n\n"));  *)
    (line_print ("The " ^ curr 
                 ^ " does not have enough magic points! Pick another spell.\n\n")
       [ANSITerminal.red] 46; minisleep 1.5 ""; s)
  else ((ANSITerminal.set_cursor 31 46; reject "target"; print_endline ""; 
         minisleep 1.5 ""; s))

(** *)
(* let fight_help g s curr *)


(******************************************************************************* 
   MAGIC BUGS AND THOUGHTS:
   - Can cast spells that aren't in the characters list of spells
   - Almost all casting of spells return a failed message
   - When casting NUKE, msg is: _ cast NUKE on _ and dealt 240 NONE damage!
 *******************************************************************************)

(******************************************************************************* 
   POTION BUGS AND THOUGHTS:
   - Heal doesn't heal enough
   - Unlimited potions
 *******************************************************************************)

(******************************************************************************* 
   RANDOM BUGS AND THOUGHTS:
   - add functionality to allow user to enter any case for target
   - work with height 50
   - dead people
 *******************************************************************************)

(** *)
let screen_setup g s state_party boss = 
  ANSITerminal.erase Screen;
  (* let state_party = get_party s in
     let boss = get_current_boss s in  *)
  let game_party_chars = List.map (fun x -> find_character x get_characters) state_party in
  let first = List.nth game_party_chars 0 in
  let second = List.nth game_party_chars 1 in
  let third = List.nth game_party_chars 2 in
  print_endline empty_frame;
  print_spr (boss_sprite g boss) 10 10;
  pr (first |> get_test) 54 3;
  pr (second |> get_test) 54 16;
  pr (third |> get_test) 54 29;
  setup_stats s (get_name first) 80 30;
  setup_stats s (get_name second) 80 37;
  setup_stats s (get_name third) 80 44;
  setup_boss_stats s boss 3 boss_name_pos

(** *)
let rec repl g s = 
  let state_party = get_party s in
  let boss = get_current_boss s in 
  screen_setup g s state_party boss;
  ANSITerminal.set_cursor 31 43;
  if not (check_alive s) then 
    (ANSITerminal.(print_string [red] "All party members are dead, you lost!"); 
     ANSITerminal.set_cursor 100 100;
     print_endline "\n"; exit 0)
  else if is_dead s boss && boss = final g then 
    (ANSITerminal.set_cursor 100 100;
     ANSITerminal.(print_string [green] ("\n" ^ dialogue g boss ^ "\n\n")); 
     exit 0)
  else if is_dead s boss then 
    (ANSITerminal.set_cursor 100 100;
     ANSITerminal.(print_string [green] ("\n" ^ dialogue g boss)); 
     print_string "\n\nPress enter to continue "; wait_no_cursor (); 
     minisleep 1.5 ""; reset_state g s |> repl g) 
  else
    let curr = get_current_fighter s in
    if List.mem curr char_names then
      let curr_char = find_character curr get_characters in
      (* let _ = ANSITerminal.set_cursor 31 49 in *)
      let _ = ANSITerminal.(print_string [Bold] ("Enter a command for " ^ (String.capitalize_ascii curr) ^ ": ")) in
      match parse (read_line ()) with
      | Fight -> if is_valid_com curr s Fight then 
          (let b = fight g s curr_char in
           let s' = new_st b in 
           let msg = desc b in
           scoot g s msg curr_char; repl g s' )
        else (ANSITerminal.set_cursor 31 45;
              ANSITerminal.(print_string [red] 
                              ("The " ^ curr ^ 
                               "'s status prevents him from fighting")); 
              minisleep 1.5 ""; repl g s)
      | Magic spell -> if is_valid_com curr s (Magic spell) then 
          (try magic_help g s spell curr_char |> repl g 
           with Not_found -> (ANSITerminal.set_cursor 31 46; reject "spell"; 
                              minisleep 1.5 ""; repl g s))
        else (line_print ("The " ^ curr ^ 
                          "'s status prevents him from casting spells") 
                [ANSITerminal.red] 51; minisleep 1.5 ""; repl g s)
      | Drink pot -> (*if is_valid_com curr s (Drink pot) then *)
        (try drink pot |> drink_comm g s curr |> repl g 
         with Invalid_potion -> (ANSITerminal.set_cursor 31 45; reject "potion"; 
                                 minisleep 1.5 ""; repl g s))
      (*else (ANSITerminal.(print_string [red] ("\nThe " ^ curr ^ "'s status prevents him from drinking potions\n\n")); 
            repl g s)   *)                    
      | Show -> let spell_str = get_spells curr_char |> string_of_list "" in 
        line_print ("Spells: " ^ spell_str) [ANSITerminal.green] 45;
        minisleep 2.5 ""; repl g s
      | Pass -> change_turns g s |> repl g
      | Quit -> ANSITerminal.set_cursor 31 45;
        ANSITerminal.(print_string [red] "Quiting game..."); 
        ANSITerminal.set_cursor 100 100;
        (* ignore(Sys.command "printf '\\e[8;24;80t'");  *)
        exit 0
      | exception Malformed -> ANSITerminal.set_cursor 31 45; 
        ANSITerminal.(print_string [red] "Invalid command. Try another."); 
        minisleep 1.5 ""; repl g s
      | exception Empty -> ANSITerminal.set_cursor 31 45; 
        ANSITerminal.(print_string [red] "Please type a command."); 
        minisleep 1.5 ""; repl g s
    else (
      let b = boss_turn g s in 
      let msg = desc b in
      let s' = new_st b in 
      (* line_print msg [ANSITerminal.yellow] 49; *)
      boss_scoot g s msg;
      minisleep 1.5 "";
      repl g s')

(** *)
and scoot g s msg ch = 
  let state_party = get_party s in
  let boss = get_current_boss s in
  ANSITerminal.erase Screen;
  let game_party_chars = List.map (fun x -> find_character x get_characters) state_party in
  let first = List.nth game_party_chars 0 in
  let second = List.nth game_party_chars 1 in
  let third = List.nth game_party_chars 2 in
  print_endline empty_frame;
  print_spr (boss_sprite g boss) 10 10;
  if ch = first then
    (pr (first |> get_test) 50 3;
     pr (second |> get_test) 54 16;
     pr (third |> get_test) 54 29;)
  else if ch = second then (
    pr (first |> get_test) 54 3;
    pr (second |> get_test) 50 16;
    pr (third |> get_test) 54 29;
  )
  else (
    pr (first |> get_test) 54 3;
    pr (second |> get_test) 54 16;
    pr (third |> get_test) 50 29;
  );
  setup_stats s (get_name first) 80 30;
  setup_stats s (get_name second) 80 37;
  setup_stats s (get_name third) 80 44;
  setup_boss_stats s boss 3 boss_name_pos;
  line_print msg [ANSITerminal.green] 43;
  minisleep 1.5 ""

(** *)
and boss_scoot g s msg =
  ANSITerminal.erase Screen;
  let state_party = get_party s in
  let boss = get_current_boss s in 
  let game_party_chars = List.map (fun x -> find_character x get_characters) state_party in
  let first = List.nth game_party_chars 0 in
  let second = List.nth game_party_chars 1 in
  let third = List.nth game_party_chars 2 in
  print_endline "";
  print_endline empty_frame;
  print_spr (boss_sprite g boss) 14 10;
  pr (first |> get_test) 54 3;
  pr (second |> get_test) 54 16;
  pr (third |> get_test) 54 29;
  setup_stats s (get_name first) 80 30;
  setup_stats s (get_name second) 80 37;
  setup_stats s (get_name third) 80 44;
  setup_boss_stats s boss 3 boss_name_pos;
  line_print msg [ANSITerminal.yellow] 43

(** *)
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
    print_string "> Press enter to continue "; wait_no_cursor (); 
    print_endline "";
    repl gaunt start 
  with Sys_error m -> reject "file"; print_string "> "; game_start (read_line ())

(** *)
let main () = 
  ignore(Sys.command "printf '\\e[8;50;120t'");
  ignore(Sys.command "clear");
  ANSITerminal.(print_string [Bold; red] "\n\nFINAL FANTASY MMMCX\n");
  print_endline "Enter a gauntlet file name to play.\n";
  print_string "> ";
  match read_line () with 
  | exception End_of_file -> ()
  | file_name when file_name = "quit" -> exit 0
  | file_name -> game_start file_name

let () = main ()