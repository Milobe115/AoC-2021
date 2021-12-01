let readfile path =
  let lines = ref [] in
  let ic = open_in path in
  try
    while true; do
      lines := input_line ic :: !lines
    done; !lines
  with End_of_file ->
    close_in ic;
    List.rev !lines
;;

let rec sliding_window l = match l with
  |hd::he::hf::tl -> (hd+he+hf) :: (sliding_window (he::hf::tl))
  |_ -> [];;

let rec compare l = match l with
  |[] -> 0
  |[_] -> 0
  |hd::he::tl when he > hd -> 1 + (compare (he::tl))
  |hd::tl -> compare tl;;


compare (sliding_window (List.map int_of_string (readfile "./input.txt")));;
