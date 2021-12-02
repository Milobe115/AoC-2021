#load "str.cma";;

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

exception UnknownValue of string;;

let process l = 
  let rec process_aux l distance depth = match l with
    | hd :: tl ->  (match (String.sub hd 0 1) with
                   | "f" -> let rgx = Str.regexp "forward \\([0-9]+\\)" in
                            let value = Str.replace_first rgx "\\1" hd in
                            let delta = int_of_string value in
                            process_aux tl (distance + delta) depth
                   | "d" -> let rgx = Str.regexp "down \\([0-9]+\\)" in
                            let value = Str.replace_first rgx "\\1" hd in
                            let delta = int_of_string value in
                            process_aux tl distance (depth + delta)
                   | "u" -> let rgx = Str.regexp "up \\([0-9]+\\)" in
                            let value = Str.replace_first rgx "\\1" hd in
                            let delta = int_of_string value in
                            process_aux tl distance (depth - delta)
                   | _ -> raise (UnknownValue "error"))
    | _ -> distance * depth in
  process_aux l 0 0;;
    
