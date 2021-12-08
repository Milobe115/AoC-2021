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

exception Error;;

let rec remove_blanks list = match list with
    | [] -> []
    | hd :: tl when (String.length hd) == 0 -> remove_blanks tl
    | hd :: tl -> hd :: remove_blanks tl ;;

let rec inner_process list = match list with
  | [] -> 0
  | hd :: tl when String.length hd == 2 -> 1 + inner_process tl
  | hd :: tl when String.length hd == 3 -> 1 + inner_process tl
  | hd :: tl when String.length hd == 4 -> 1 + inner_process tl
  | hd :: tl when String.length hd == 7 -> 1 + inner_process tl
  | hd :: tl -> inner_process tl;;

let rec process list =  match list with
  | [] -> 0
  | hd::tl -> (inner_process hd) + (process tl);;
 
let input = List.map remove_blanks (List.map (String.split_on_char ' ') (List.map (fun x -> List.hd (List.tl x)) (List.map (String.split_on_char '|') (readfile "./input.txt"))));;

let answer = process input;;
