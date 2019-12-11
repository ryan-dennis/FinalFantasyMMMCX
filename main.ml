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

let title = [
  "  ▐                       ▐                                  ";
  "▀█▀▀█                   ▀█▀▀█           █▀█▀█                ";
  " █▐ ▐                    █▐ ▐           ▌ █ ▐                ";
  " █▐  ▀█▀ █   █▀ ██  ▀█▀  █▐   ██   █   █▀ █   ██   █▀█▐█▀ ▀█▀";
  " █▐   █  █▌  █  ██▌  █   █▐   ██▌  █▌  █  █   ██▌ █  ▐ █   █ ";
  " █▐   █  ██  █  █ █  █   █▐   █ █  ██  █  █   █ █ █    ▐█ █▌ ";
  " █▐ ▐ █  ██▌ █ ▐█ █  █   █▐ ▐▐█ █  ██▌ █  █  ▐█ █ █     █ █  ";
  " █▐██ █  █▐█ █ ▐█ █  █   █▐██▐█ █  █▐█ █  █  ▐█ █ ▐█    ▐█▌  ";
  " █▐ ▐ █  █ █ █ ▐█ █  █   █▐ ▐▐█ █  █ █ █  █  ▐█ █  ▐█    █   ";
  " █▐   █  █ █▌█ ▐█ █  █   █▐  ▐█ █  █ █▌█  █  ▐█ █   ▐█   █   ";
  " █▐   █  █ ▐██ █  █  █   █▐  █  █  █ ▐██  █  █  █    █   █   ";
  " █▐   █  █  ██ ████  █   █▐  ████  █  ██  █  ████   ▐█   █   ";
  " █▐   █  █  ▐█ █  █  █ ▐ █▐  █  █  █  ▐█  █  █  █   █    █   ";
  " █▐  ▄█▄ █▄  █ █▄ █▄ █▄█ █▐  █▄ █▄ █▄  █ ▄█▄ █▄ █▄▐█    ▄█▄  ";
  " █▐                      █▐                                  ";
  " █▐                      █▐                                  ";
  " █▐                      █▐                                  ";
  " █                       █                                   ";
  " ▌                       ▌                                   "
]

(** [print_title x y t] prints the title [t] to the screen at coordinates
    [x] and [y]. *)
let rec print_title x y = function
  | [] -> ANSITerminal.set_cursor 100 100
  | h::t -> (ANSITerminal.set_cursor x y; 
             ANSITerminal.(print_string [red] h); 
             print_title x (y+1) t)

(** [cap_all s] is [s] with the first letter of every word capitalized. *)
let cap_all s = 
  let clst = String.split_on_char ' ' s in
  let nlst = List.map (fun x -> String.capitalize_ascii x) clst in
  String.concat " " nlst

(** [color_helper h] is the style list from [h]. *)
let color_helper h = 
  match h with 
  | "cyan" -> [ANSITerminal.cyan]
  | "yellow" -> [ANSITerminal.yellow]
  | "black" -> [ANSITerminal.black]
  | "white" -> [ANSITerminal.white]
  | "red" -> [ANSITerminal.red]
  | "blue" -> [ANSITerminal.blue]
  | "magenta" -> [ANSITerminal.magenta]
  | "green" -> [ANSITerminal.green]
  | "background white" -> [Background White]
  | _ -> [ANSITerminal.default]

(** [helper string color int x y] prints each string in [string] with 
    colors [color] at height [y] and horizontal position [x]. *)
let rec helper string color int x y = 
  match string with 
  | [] -> ()
  | h:: t -> ANSITerminal.set_cursor x y; 
    ANSITerminal.(print_string (color_helper (List.nth color int)) h);
    helper t color (int+1) (x+1) y

(** [pr s x y] is the colored sprite [s] printed starting at coordinates [x] 
    and [y]. *)
let rec pr s x y= 
  match s with 
  | [] -> ()
  | (s1,lst)::t -> helper s1 lst 0 x y; pr t x (y+1)

(** [line_print s slst y] prints [s] with style [slst] at height [y] and wraps
    text to a new line if neccessary. *)
let rec line_print s slst y = 
  ANSITerminal.set_cursor 31 y;
  if (String.length s) <= 46 then ANSITerminal.(print_string slst s)
  else (ANSITerminal.(print_string slst (String.sub s 0 46)); 
        line_print (String.sub s 46 ((String.length s) - 46)) slst) (y + 1)

(** [minisleep sec s] prints [s] to the display and then waits [sec] seconds. *) 
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

(** [wait_no_cursor ()] waits for a new line character while hiding the cursor 
    and user input. *)
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

(** [string_of_list acc lst] is [acc] with the elements of [lst] concatenated
    to it. *)
let rec string_of_list acc = function
  | [] -> acc
  | [e] -> acc ^ e
  | h :: t -> string_of_list (h ^ ", " ^ acc) t

(** [setup_stats s c x y] prints the stats of character [c] from state [s] in 
    the [x] and [y] starting positions. *)
let setup_stats s c x y = 
  let hp = get_health c s in
  let mp = get_magic_points c s in
  ANSITerminal.set_cursor x y;
  ANSITerminal.(print_string [Bold; cyan] (cap_all c));
  ANSITerminal.set_cursor x (y+2);
  ANSITerminal.(print_string [default] "HP: "); 
  ANSITerminal.(print_string [if hp > 200 then green 
                              else if hp > 100 then yellow
                              else red] (string_of_int hp));
  ANSITerminal.set_cursor x (y+3);
  ANSITerminal.(print_string [default] "MP: "); 
  ANSITerminal.(print_string [blue] (string_of_int mp));
  ANSITerminal.set_cursor 100 100

(** [setup_boss_stats s b x y] prints the stats of boss [b] from state [s] in 
    the [x] and [y] starting positions. *)
let setup_boss_stats s b x y = 
  let hp = get_health b s in
  ANSITerminal.set_cursor x y;
  ANSITerminal.(print_string [Bold; yellow] (cap_all b));
  ANSITerminal.set_cursor x (y+2);
  ANSITerminal.(print_string [default] "HP: "); 
  ANSITerminal.(print_string [if hp < 100 then red else green] 
                  (string_of_int hp));
  ANSITerminal.set_cursor 100 100

(** [string_of_status stat] is the string form of the status [stat]. *)
let string_of_status = function
  | Poisoned -> "poisoned"
  | Blinded -> "blinded"
  | Paralyzed -> "paralyzed"
  | Silenced -> "silenced"

(** [char_names] is the list of names of all characters in the game. *)
let char_names = List.map (get_name) get_characters

(** [reject msg] prints a response to the terminal indicating that there is an
    invalid property [msg]. *)
let reject msg = ANSITerminal.(print_string [red] 
                                 ("Invalid " ^ msg ^ ". Pick another.\n"))

(** [select_party acc] is the list of character names selected according to user
    input. *)
let rec select_party (acc : string list) : string list = 
  ANSITerminal.(print_string [magenta] ("Current party: " 
                                        ^ (string_of_list "" acc) ^ "\n"));
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

(** [drink_comm g s curr pot] is the state after the character [curr] drinks the
    potion [pot] in state [s] and gauntlet [g]. *)
let drink_comm g s curr pot = 
  match pot with
  | Heal -> if has_heal curr s then
      (ANSITerminal.set_cursor 31 45; 
       ANSITerminal.(print_string [green] ("The " ^ curr ^ " healed.\n\n")); 
       minisleep 1.5 ""; used_heal curr s |> heal_eff g)
    else (ANSITerminal.set_cursor 31 45;
          ANSITerminal.(print_string [red] ("The " ^ curr 
                                            ^ " is out of heal potions!\n\n")); 
          minisleep 1.5 ""; s)
  | Pure -> if has_pure curr s then
      (ANSITerminal.set_cursor 31 45;
       ANSITerminal.(print_string [green] 
                       ("The " ^ curr ^ " purified their status effects.\n\n"));
       minisleep 1.5 ""; used_pure curr s |> pure_eff g)
    else (ANSITerminal.set_cursor 31 45;
          ANSITerminal.(print_string [red] ("The " ^ curr 
                                            ^ " is out of pure potions!\n\n")); 
          minisleep 1.5 ""; s)

(** [magic_help g s spell ch] is the state following the character [ch] casting
    the spell [spell] in state [s] and gauntlet [g]. *)
let magic_help g s spell ch = 
  ANSITerminal.set_cursor 31 44;
  ANSITerminal.(print_string [white] "Enter the target for the spell: ");
  let sp_t = get_spell spell in
  let friendly = is_friendly sp_t in
  let tar = read_line () in
  let tar' = (fun x -> if friendly then String.lowercase_ascii x 
               else cap_all x) tar in 
  let curr = get_current_fighter s in
  if is_valid_target s sp_t tar' && 
     has_spell ch spell && 
     is_enough_mp sp_t ch s then 
    (let b = magic g s spell ch tar' in
     let msg = (desc b) ^ "\n" in 
     line_print msg [ANSITerminal.green] 46; minisleep 1.5 ""; new_st b)
  else if not (has_spell ch spell) then 
    (line_print ("The " ^ curr ^ " does not have that spell! Pick another.\n\n")
       [ANSITerminal.red] 46; minisleep 1.5 ""; s)
  else if not (is_enough_mp sp_t ch s) then
    (line_print ("The " ^ curr 
                 ^ " does not have enough magic points! Pick another spell.\n")
       [ANSITerminal.red] 46; minisleep 1.5 ""; s)
  else ((ANSITerminal.set_cursor 31 46; reject "target"; print_endline ""; 
         minisleep 1.5 ""; s))

(****************************************************************************** 
   RANDOM BUGS AND THOUGHTS:
   - lockup after player selection -> fixed?
   - test not enough magic points
   - ******DOCUMENT******
 ******************************************************************************)

(** [mal_help ()] prints the response when a [Malformed] exception is 
    encountered. *)
let mal_help () = 
  ANSITerminal.set_cursor 31 45; 
  ANSITerminal.(print_string [red] "Invalid command. Try another."); 
  minisleep 1.5 ""

(** [emp_help ()] prints the response when an [Empty] exception is 
    encountered. *)
let emp_help () = 
  ANSITerminal.set_cursor 31 45; 
  ANSITerminal.(print_string [red] "Please type a command."); 
  minisleep 1.5 ""

(** [dead_team ()] prints a message and exits the game. *)
let dead_team () = 
  ANSITerminal.(print_string [red] "All party members are dead, you lost!"); 
  ANSITerminal.set_cursor 100 100;
  print_endline "\n"; exit 0

(** [screen_setup g s state_party boss] initializes the display with normal 
    player and boss positions along with their stats. *)
let screen_setup g s state_party boss = 
  ANSITerminal.erase Screen;
  let game_party_chars = 
    List.map (fun x -> find_character x get_characters) state_party in
  let first = List.nth game_party_chars 0 in
  let second = List.nth game_party_chars 1 in
  let third = List.nth game_party_chars 2 in
  print_endline empty_frame;
  pr (get_color_sprite boss) 10 10;
  pr (if is_dead s (get_name first) then skull else first |> get_test) 54 3;
  pr (if is_dead s (get_name second) then skull else second |> get_test) 54 16;
  pr (if is_dead s (get_name third) then skull else third |> get_test) 54 29;
  setup_stats s (get_name first) 80 30;
  setup_stats s (get_name second) 80 37;
  setup_stats s (get_name third) 80 44;
  setup_boss_stats s boss 3 boss_name_pos

(** [paryl_help curr f] prints the message indicating that the player cannot
    do something because they are paralyzed. *)
let paryl_help curr m = 
  line_print ("The " ^ curr ^ "'s status prevents him from " ^ m ^ "!") 
    [ANSITerminal.red] 45; minisleep 1.5 ""

(** [dead_boss g b] prints the battle message from defeating boss [b] in 
    gauntlet [g]. *)
let dead_boss g b = 
  ANSITerminal.set_cursor 100 100;
  ANSITerminal.(print_string [green] ("\n" ^ dialogue g b ^ "\n\n"))

(** [dead_boss_pr g b] prints the battle message from defeating boss [b] in 
    gauntlet [g], then prompts the user for an enter key. *)
let dead_boss_pr g b = 
  dead_boss g b;
  print_string "Press enter to continue "; 
  wait_no_cursor (); 
  minisleep 1.5 ""

(** [show_help curr_t] prints the spells of character [curr_t] to the screen
    and then waits for a newline character. *)
let show_help curr_t = 
  let spell_str = get_spells curr_t |> string_of_list "" in 
  line_print ("Spells: " ^ spell_str) [ANSITerminal.green] 45;
  ANSITerminal.set_cursor 31 47;
  print_string "Press enter to continue";
  minisleep 0.05 "";
  wait_no_cursor ()

let quit_help () = 
  ANSITerminal.set_cursor 31 45;
  ANSITerminal.(print_string [red] "Quiting game..."); 
  ANSITerminal.set_cursor 100 100; print_endline ""; exit 0

(** [repl g s] is the read-evaluate-print-loop for assessing player commands
    in the gauntlet [g] with state [s]. Quits once the player types the quit 
    command, otherwise responds according to the command and recurses. *)
let rec repl g s = 
  let state_party = get_party s in
  let boss = get_current_boss s in 
  screen_setup g s state_party boss;
  ANSITerminal.set_cursor 31 43;
  if not (check_alive s) then dead_team ()
  else if is_dead s boss && boss = final g then (dead_boss g boss; exit 0)
  else if is_dead s boss then (dead_boss_pr g boss; reset_state g s |> repl g) 
  else parse_help g s

and parse_help g s =
  let curr = get_current_fighter s in
  if List.mem curr char_names then
    let curr_t = find_character curr get_characters in
    let _ = ANSITerminal.(
        print_string [default] ("Enter a command for " ^ 
                                (cap_all curr) ^ ": ")) in
    match parse (read_line ()) with
    | Fight -> if is_valid_com curr s Fight then f_help g s curr_t |> repl g
      else (paryl_help curr "fighting"; repl g s)
    | Magic spell -> let sp = String.uppercase_ascii spell in
      if is_valid_com curr s (Magic sp) then 
        (try magic_help g s sp curr_t |> repl g 
         with Not_found -> (ANSITerminal.set_cursor 31 46; reject "spell"; 
                            minisleep 1.5 ""; repl g s))
      else (paryl_help curr "casting spells"; repl g s)
    | Drink pot -> (try drink pot |> drink_comm g s curr |> repl g 
                    with Invalid_potion -> (ANSITerminal.set_cursor 31 45;
                                            reject "potion"; minisleep 1.5 "";
                                            repl g s))
    | Show -> show_help curr_t; repl g s
    | Pass -> change_turns g s |> repl g
    | Paralyze -> status_add curr Paralyzed s |> repl g
    | Quit -> quit_help ()
    | exception Malformed -> mal_help (); repl g s
    | exception Empty -> emp_help (); repl g s
  else boss_help g s |> repl g

(** [scoot g s msg ch] is the character [ch] moved to the left and battle 
    message [msg] printed to the screen. *)
and scoot g s msg ch = 
  let state_party = get_party s in
  let boss = get_current_boss s in
  let sk = skull in
  ANSITerminal.erase Screen;
  let game_party_chars = 
    List.map (fun x -> find_character x get_characters) state_party in
  let first = List.nth game_party_chars 0 in
  let second = List.nth game_party_chars 1 in
  let third = List.nth game_party_chars 2 in
  print_endline empty_frame;
  pr (get_color_sprite boss) 10 10;
  if ch = first then (
    pr (if is_dead s (get_name first) then sk else first |> get_test) 50 3;
    pr (if is_dead s (get_name second) then sk else second |> get_test) 54 16;
    pr (if is_dead s (get_name third) then sk else third |> get_test) 54 29;)
  else if ch = second then (
    pr (if is_dead s (get_name first) then sk else first |> get_test) 54 3;
    pr (if is_dead s (get_name second) then sk else second |> get_test) 50 16;
    pr (if is_dead s (get_name third) then sk else third |> get_test) 54 29;)
  else (
    pr (if is_dead s (get_name first) then sk else first |> get_test) 54 3;
    pr (if is_dead s (get_name second) then sk else second |> get_test) 54 16;
    pr (if is_dead s (get_name third) then sk else third |> get_test) 50 29;);
  setup_stats s (get_name first) 80 30;
  setup_stats s (get_name second) 80 37;
  setup_stats s (get_name third) 80 44;
  setup_boss_stats s boss 3 boss_name_pos;
  line_print msg [ANSITerminal.green] 43;
  minisleep 1.5 ""

(** [scoot g s msg] is the boss in state [s] moved to the right and battle 
    message [msg] printed to the screen. *)
and boss_scoot g s msg =
  ANSITerminal.erase Screen;
  let state_party = get_party s in
  let boss = get_current_boss s in 
  let game_party_chars = 
    List.map (fun x -> find_character x get_characters) state_party in
  let first = List.nth game_party_chars 0 in
  let second = List.nth game_party_chars 1 in
  let third = List.nth game_party_chars 2 in
  print_endline "";
  print_endline empty_frame;
  pr (get_color_sprite boss) 14 10;
  pr (if is_dead s (get_name first) then skull else first |> get_test) 54 3;
  pr (if is_dead s (get_name second) then skull else second |> get_test) 54 16;
  pr (if is_dead s (get_name third) then skull else third |> get_test) 54 29;
  setup_stats s (get_name first) 80 30;
  setup_stats s (get_name second) 80 37;
  setup_stats s (get_name third) 80 44;
  setup_boss_stats s boss 3 boss_name_pos;
  line_print msg [ANSITerminal.yellow] 43

(** [f_help g s curr_char] is the character [curr_char] in state [s] moved to 
    the right and the response printed to the screen. *)
and f_help g s curr_char = 
  let b = fight g s curr_char in
  let s' = new_st b in 
  let msg = desc b in
  scoot g s msg curr_char;
  s'

(** [boss_help g s] is the state after the current boss has taken its turn 
    in state [s] and gauntlet [g]. *)
and boss_help g s = 
  let b = boss_turn g s in 
  let msg = desc b in
  let s' = new_st b in 
  boss_scoot g s msg; minisleep 1.5 ""; s'

(** [game_start f] is the gauntlet started from file [f]. *)
let rec game_start f = 
  try let gaunt = f |> Yojson.Basic.from_file |> from_json in 
    ignore(Sys.command "clear");
    let m = "\nEnter three of the following characters " ^ 
            "that you wish to add to your party:\n" in
    ANSITerminal.(print_string [green] m);
    ANSITerminal.(print_string [green] (string_of_list "" char_names));
    print_endline "\n";
    let game_party_names = select_party [] in
    let game_party_chars = 
      List.map (fun x -> find_character x get_characters) game_party_names in
    let start = init_state gaunt game_party_chars in 
    ANSITerminal.(print_string [cyan] ("\n" ^ start_dialogue gaunt ^ "\n\n"));
    minisleep 0.05 "> Press enter to continue "; wait_no_cursor (); 
    print_endline "";
    repl gaunt start 
  with Sys_error m -> reject "file"; print_string "> "; game_start (read_line ())

(** [main ()] prompts the game to start and then starts it. *)
let main () = 
  ignore(Sys.command "printf '\\e[8;50;120t'");
  ignore(Sys.command "clear");
  print_title 32 10 title;
  ANSITerminal.set_cursor 1 1;
  minisleep 0.05 "";
  let m = "\t\t\t\t\t\tPress enter to continue" in
  minisleep 0.05 "";
  ANSITerminal.(print_string [default; Blink] m);
  minisleep 0.05 "";
  wait_no_cursor ();
  (* minisleep 2.5 ""; *)
  game_start "gauntlet1.json"

let () = main ()