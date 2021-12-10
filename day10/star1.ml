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

exception Error of string;;

let process_one_line list corrupted incomplete =
  let rec pol_aux list corrupted incomplete stack = match list with
    | [] -> corrupted, list::incomplete
    | hd::tl -> match hd with
                | '[' | '(' | '{' | '<' ->
                   pol_aux tl corrupted incomplete (hd::stack)
                | ']' | ')' | '}' | '>' -> (
                  match hd, List.hd stack with
                  | ']', '[' | ')', '(' | '}', '{' | '>', '<' -> 
                     pol_aux tl corrupted incomplete (List.tl stack)
                  | _, _ -> list::corrupted, incomplete)
                | _ -> raise (Error "PAAAANIC")
  in
  pol_aux list corrupted incomplete [];;

let process list =
  let rec p_aux list corrupted incomplete =
    match list with
    | [] -> corrupted
    | hd::tl -> let ncorrupted, nincomplete = process_one_line hd corrupted incomplete in
                p_aux tl ncorrupted nincomplete
  in
  p_aux list [] [];;

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;

let char_score char = match char with
  | ')' -> 3
  | ']' -> 57
  | '}' -> 1197
  | '>' -> 25137
  | _ -> 0;;

let input = List.map explode (readfile "./input.txt");;
let error_chars = List.map List.hd (process input);;
let final_score = List.fold (+) 0 (List.map char_score error_chars);;
