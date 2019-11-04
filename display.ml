(** Dimensions: 70 across, 30 vertically *)

let rec frame_sides n acc =
  if n = 0 then acc
  else (String.concat "" ["║"; String.make 42 ' ';
                          "║"; String.make 25 ' '; "║"])
       ::acc |>
       frame_sides (n-1)

let rec hor_frame n acc =
  if n = 0 then acc
  else String.concat "" ["═"; acc] |> hor_frame (n-1)

let frame =
  List.cons (String.concat "" ["╚"; hor_frame 42 ""; "╩";
                               hor_frame 25 ""; "╝"])
    (frame_sides 9 []) @
  List.cons (String.concat "" ["╠"; hor_frame 42 ""; "╬";
                               hor_frame 25 ""; "╣"])
    (frame_sides 18 []) |>
  List.rev |>
  List.cons (String.concat "" ["╔"; hor_frame 42 ""; "╦";
                               hor_frame 25 ""; "╗"])
  |> String.concat "\n"