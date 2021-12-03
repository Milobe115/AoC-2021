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

exception ValueError;;

let c2i c = match c with
  |'0' -> 0
  |'1' -> 1
  | _ -> raise ValueError;;

let rec add_two_lists l1 l2 = match l1, l2 with
  | [],  _ -> l2
  |  _, [] -> l1
  | hd1::tl1, hd2::tl2 -> (hd1 + hd2)::(add_two_lists tl1 tl2);;

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;

let clist2ilist l = List.map c2i l;;

let reduce_lists l = match l with
  | [] -> []
  | hd::tl -> List.fold_left add_two_lists hd tl;;

let rec calculate_bits list size = match list with
  | [] -> []
  | hd::tl when hd > size / 2 -> 1 :: (calculate_bits tl size)
  | hd::tl -> 0 :: (calculate_bits tl size);;

let get_gamma_epsilon list =
  let rec gamma list acc = match list with
    | [] -> acc
    | hd::tl -> gamma tl (2*acc + hd) in
  let rec epsilon list acc = match list with
    | [] -> acc
    | hd::tl -> epsilon tl (2*acc + (1-hd)) in
  (epsilon list 0) * (gamma list 0);;

let input = readfile "./input.txt";;
let n = List.length input;;
let bits = calculate_bits (reduce_lists (List.map clist2ilist (List.map explode input))) n;;
get_gamma_epsilon (bits);;
