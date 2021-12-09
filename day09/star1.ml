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


let rec parse_array list =
  let parse_one_line string =
    let n = String.length string in
    let array = Array.make n 0 in
    for i = 0 to (n-1) do
      array.(i) <- Int.of_string(String.make 1 string.[i]);
    done;
    array
  in
  match list with
  | [] -> []
  | hd::tl -> (parse_one_line hd) :: (parse_array tl)
  
let process array =
  let n = Array.length array in
  let p = Array.length array.(0) in
  let score = ref 0 in
  for i = 0 to (n - 1) do
    for j = 0 to (p - 1) do
      let minimum = ref true in
      if i > 0 then minimum := !minimum && (array.(i-1).(j) > array.(i).(j));
      if j > 0 then minimum := !minimum && (array.(i).(j-1) > array.(i).(j));
      if i < (n-1) then minimum := !minimum && (array.(i+1).(j) > array.(i).(j));
      if j < (p-1) then minimum := !minimum && (array.(i).(j+1) > array.(i).(j));
      if !minimum then
        score := !score + 1 + array.(i).(j);
    done;
  done;
  !score;;

            
let input = Array.of_seq (List.to_seq (parse_array (readfile "./input.txt")));;
let answer = process input;;

