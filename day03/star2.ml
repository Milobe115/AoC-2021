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

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;

let clist2ilist l = List.map c2i l;;

let convert list =
  let rec convert_aux list acc = match list with
    | [] -> acc
    | hd::tl -> convert tl (2*acc + hd)in
  convert_aux list 0;;

let filter_ones list index =
  let condition s = s.[index] == '1' in
  List.filter condition list;;
  
let filter_zeroes list index =
  let condition s = s.[index] == '0' in
  List.filter condition list;;

let rec count_ones list index = match list with
  | [] -> 0
  | hd :: tl when hd.[index] == '1' -> 1 + (count_ones tl index)
  | hd :: tl -> count_ones tl index;;

let filter_oxygen list =
  let rec filter_oxygen_aux list index =
    match list with
    | [] -> raise ValueError
    | [elt] -> elt
    | _ -> let n = List.length list in
           let ones = count_ones list index in
           if (ones > (n / 2)) || (ones == (n - ones))
           then
             let new_list = filter_ones list index in
             filter_oxygen_aux new_list (index + 1)
           else
             let new_list = filter_zeroes list index in
             filter_oxygen_aux new_list (index + 1) in
  filter_oxygen_aux list 0
;;

let filter_co2 list = 
  let rec filter_co2_aux list index = 
    match list with
    | [] -> raise ValueError
    | [elt] -> elt
    | _ -> let n = List.length list in
           let ones = count_ones list index in
           if (ones > (n / 2)) || (ones == (n - ones))
           then
             let new_list = filter_zeroes list index in
             filter_co2_aux new_list (index + 1)
           else
             let new_list = filter_ones list index in
             filter_co2_aux new_list (index + 1) in
  filter_co2_aux list 0
;;

let input = readfile "./input.txt";;

let oxygen = convert (clist2ilist (explode (filter_oxygen input))) in
    let co2 = convert (clist2ilist (explode (filter_co2 input))) in
    oxygen * co2;;
