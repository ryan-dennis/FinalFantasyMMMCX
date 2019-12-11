(** Dimensions: 91 across, 47 vertically 
    Boss box: 44 across, 27 vertically
    Character box: 31 across, 27 vertically
    Characters: 16 px by 24 px, 16 characters across by 12 lines
    Boss: 40 px by 52 px, 40 characters across by 28 lines *)

open Party
open State

(** Dimensions of the frame. *)
let arena = 75
let b1_top = 44
let b2_top = arena - b1_top
let b3_top = 12
let b1_bot = 17
let b_inter_bot = 9
let b2_bot = arena - b1_bot - b_inter_bot-1
let b3_bot = 12
let offset = 16

(** [frame_sides b1 bi b2 b3 n acc] is a row of the frame that is not a
    boundary with the given bounds. *)
let rec frame_sidesi b1 bi b2 b3 n acc =
  if n = 0 then acc
  else (String.concat "" ["║"; String.make b1 ' ';
                          "║"; String.make bi ' ';
                          "║"; String.make b2 ' ';
                          "║"; String.make b3 ' ';
                          "║"])
       ::acc |>
       frame_sidesi b1 bi b2 b3 (n-1)

(** [frame_sides b1 b2 b3 n acc] is a row of the frame that is not a
    boundary with the given bounds. *)
let rec frame_sides b1 b2 b3 n acc =
  if n = 0 then acc
  else (String.concat "" ["║"; String.make b1 ' ';
                          "║"; String.make b2 ' ';
                          "║"; String.make b3 ' ';
                          "║"])
       ::acc |>
       frame_sides b1 b2 b3 (n-1)       

(** [frame_sides_gap n acc] is a row of the frame that is not a boundary
    after the character boxes on the right end. *)
let rec frame_sides_gap n acc =
  if n = 0 then acc
  else (String.concat "" ["║"; String.make b1_top ' ';
                          "║"; String.make b2_top ' ';
                          "║"; String.make (b3_top+1) ' '])
       ::acc |>
       frame_sides_gap (n-1)

(** [hor_frame n acc] is a horizontal line that is [n] characters long. *)
let rec hor_frame n acc =
  if n = 0 then acc
  else String.concat "" ["═"; acc] |> hor_frame (n-1)

(** [frame_side_bar b1 b2 b3] is the divider for the character boxes on the
    right. *)
let frame_side_bar b1 b2 b3 =
  String.concat "" ["║"; String.make b1 ' ';
                    "║"; String.make b2 ' ';
                    "╠"; hor_frame b3 "";
                    "╣"]

(** [frame_side_bari b1 bi b2 b3] is the divider for the character boxes on the
    right. *)
let frame_side_bari b1 bi b2 b3 =
  String.concat "" ["║"; String.make b1 ' ';
                    "║"; String.make bi ' ';
                    "║"; String.make b2 ' ';
                    "╠"; hor_frame b3 "";
                    "╣"]

(** [frame_side_word word] is the row across the bottom half of the frame
    with the five letter word [word] placed inside. *)
let frame_side_word word =
  String.concat "" ["║"; String.make b1_bot ' ';
                    "║"; String.make 2 ' ';
                    word; String.make 2 ' '; "║";
                    String.make b2_bot ' ';
                    "║"; String.make b3_bot ' ';
                    "║"]                   

(** [frame] is the GUI frame as a string list. *)
let frame =
  List.cons (String.concat "" ["╚"; hor_frame b1_bot "";
                               "╩"; hor_frame b_inter_bot "";
                               "╩"; hor_frame b2_bot "";
                               "╩"; hor_frame b3_bot "";
                               "╝"])
    (frame_sidesi b1_bot b_inter_bot b2_bot b3_bot 1 [] |>
     List.cons (frame_side_word "DRINK") |>
     List.append (frame_sidesi b1_bot b_inter_bot b2_bot b3_bot 1 []) |>
     List.cons (frame_side_word "MAGIC") |>
     List.append (frame_sidesi b1_bot b_inter_bot b2_bot b3_bot 1 []) |>
     List.cons (frame_side_word "FIGHT") |>
     List.cons (frame_side_bari b1_bot b_inter_bot b2_bot b3_bot) |>
     List.rev)
  @
  List.cons (String.concat "" ["╠"; hor_frame b1_bot "";
                               "╦"; hor_frame b_inter_bot "";
                               "╦"; hor_frame offset "";
                               "╩"; hor_frame b2_top "";
                               "╣"; String.make b3_bot ' ';
                               "║"])
    (frame_sides b1_top b2_top b3_top 5 [] |>
     List.cons (frame_side_bar b1_top b2_top b3_top) |>
     List.append (frame_sides b1_top b2_top b3_top 6 []) |>
     List.cons (String.concat "" ["║"; String.make b1_top ' ';
                                  "║"; String.make b2_top ' ';
                                  "╠"; hor_frame b3_top "";
                                  "╗"]) |>
     List.append (frame_sides_gap 25 []) |>
     List.rev) |>
  List.rev |>
  List.cons (String.concat "" ["╔"; hor_frame b1_top "";
                               "╦"; hor_frame b2_top "";
                               "╗"; String.make (b3_top+1) ' '])

(** [empty_frame] is the frame of the display with the words FIGHT, MAGIC and
    DRINK in the but no character's *)
let empty_frame = frame |> String.concat "\n"

(**
   ╔════════════════════════════════════════════╦═══════════════════════════════╗
   ║                                            ║                               ║
   ║                                            ║                               ║
   ║                                            ║                               ║
   ║                                            ║                               ║
   ║                                            ║                               ║
   ║                                            ║                               ║
   ║                                            ║                               ║
   ║                                            ║                               ║
   ║                                            ║                               ║
   ║                                            ║                               ║
   ║                                            ║                               ║
   ║                                            ║                               ║
   ║                                            ║                               ║
   ║                                            ║                               ║           
   ║                                            ║                               ║
   ║                                            ║                               ║
   ║                                            ║                               ║
   ║                                            ║                               ║
   ║                                            ║                               ║
   ║                                            ║                               ║
   ║                                            ║                               ║
   ║                                            ║                               ║
   ║                                            ║                               ║
   ║                                            ║                               ║
   ║                                            ║                               ║
   ║                                            ║                               ╠════════════╗
   ║                                            ║                               ║            ║
   ║                                            ║                               ║            ║
   ║                                            ║                               ║            ║
   ║                                            ║                               ║            ║
   ║                                            ║                               ║            ║
   ║                                            ║                               ║            ║
   ║                                            ║                               ╠════════════╣
   ║                                            ║                               ║            ║
   ║                                            ║                               ║            ║
   ║                                            ║                               ║            ║
   ║                                            ║                               ║            ║
   ║                                            ║                               ║            ║
   ╠═════════════════╦═════════╦════════════════╩═══════════════════════════════╣            ║
   ║                 ║         ║                                                ╠════════════╣
   ║                 ║  FIGHT  ║                                                ║            ║
   ║                 ║         ║                                                ║            ║
   ║                 ║  MAGIC  ║                                                ║            ║
   ║                 ║         ║                                                ║            ║
   ║                 ║  DRINK  ║                                                ║            ║
   ║                 ║         ║                                                ║            ║
   ╚═════════════════╩═════════╩════════════════════════════════════════════════╩════════════╝
*)

(* 
print_endline empty_frame; 
ANSITerminal.move_cursor 5 ~-20;  
print_string "hello";  
ANSITerminal.move_cursor 0 1; 
print_string "hi";  
ANSITerminal.move_cursor 0 1;  
print_string "this";  
ANSITerminal.move_cursor 0 1;  
print_string "is";  
ANSITerminal.move_cursor 0 1;  
print_string "a frame"; 
ANSITerminal.move_cursor ~-10 20;;
*)

(* let rec print_spr lst y =
   match lst with
   | [] -> ANSITerminal.set_cursor 100 100
   | h::t -> (ANSITerminal.set_cursor 4 y; print_endline h; print_spr t (y+1)) *)

(* 
print_endline empty_frame;
print_spr spr 14
*)

(* print_endline empty_frame;
   print_spr sprite 7 14;
   print_spr whit 55 9;
   print_spr whit 55 22;
   print_spr whit 55 35;; *)