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

let process_one_line list incomplete =
  let rec pol_aux list incomplete stack = match list with
    | [] -> stack::incomplete
    | hd::tl -> match hd with
                | '[' | '(' | '{' | '<' ->
                   pol_aux tl incomplete (hd::stack)
                | ']' | ')' | '}' | '>' -> (
                  match hd, List.hd stack with
                  | ']', '[' | ')', '(' | '}', '{' | '>', '<' -> 
                     pol_aux tl incomplete (List.tl stack)
                  | _, _ -> incomplete)
                | _ -> raise (Error "PAAAANIC")
  in
  pol_aux list incomplete [];;

let process list =
  let rec p_aux list incomplete =
    match list with
    | [] -> incomplete
    | hd::tl -> let nincomplete = process_one_line hd incomplete in
                p_aux tl nincomplete
  in
  p_aux list [];;

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;

let char_score char = match char with
  | '(' -> 1
  | '[' -> 2
  | '{' -> 3
  | '<' -> 4
  | _ -> 0;;

let compute_score list =
  let rec cs_aux list score = 
    match list with
    | [] -> score
    | hd::tl -> cs_aux tl (score * 5 + (char_score hd)) in
  cs_aux list 0;;       

let input = List.map explode (readfile "./input.txt");;
let error_chars = process input;;
let final_scores = Array.of_seq (List.to_seq (List.map compute_score error_chars));;
Array.sort Int.compare final_scores;;
final_scores.((Array.length final_scores) / 2);;
