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

let rec parse l = match l with
  |[] -> 0
  |[_] -> 0
  |hd::he::tl when he > hd -> 1 + (parse (he::tl))
  |hd::tl -> parse tl;;

parse (List.map int_of_string (readfile "./input.txt"));;
