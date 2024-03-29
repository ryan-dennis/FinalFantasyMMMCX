(** Dimensions: 70 across, 30 vertically 
    Boss box: 37 across, 20 vertically
    Character box: 19 across, 20 vertically *)

(** Dimensions of the frame. *)
let arena = 56
let b1_top = 37
let b2_top = arena - b1_top
let b3_top = 10
let b1_bot = 24
let b2_bot = arena - b1_bot
let b3_bot = 10
let offset = b1_top - b1_bot - 1

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

let frame_side_word word =
  String.concat "" ["║"; String.make b1_bot ' ';
                    "║"; String.make 2 ' ';
                    word; String.make (b2_bot - (String.length word) - 2) ' ';
                    "║"; String.make b3_bot ' ';
                    "║"]

(** [frame] is the GUI frame as a string list. *)
let frame =
  List.cons (String.concat "" ["╚"; hor_frame b1_bot "";
                               "╩"; hor_frame b2_bot "";
                               "╩"; hor_frame b3_bot "";
                               "╝"])
    (frame_sides b1_bot b2_bot b3_bot 1 [] |>
     List.cons (frame_side_word "DRINK") |>
     List.append (frame_sides b1_bot b2_bot b3_bot 1 []) |>
     List.cons (frame_side_word "MAGIC") |>
     List.append (frame_sides b1_bot b2_bot b3_bot 1 []) |>
     List.cons (frame_side_word "FIGHT") |>
     List.cons (frame_side_bar b1_bot b2_bot b3_bot) |>
     List.rev)
  @
  List.cons (String.concat "" ["╠"; hor_frame b1_bot "";
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
     List.append (frame_sides_gap 7 []) |>
     List.rev) |>
  List.rev |>
  List.cons (String.concat "" ["╔"; hor_frame b1_top "";
                               "╦"; hor_frame b2_top "";
                               "╗"; String.make (b3_top+1) ' '])

let empty_frame = frame |> String.concat "\n"