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

let max array =
  let max = ref array.(0) in
  for i = 0 to (Array.length array) - 1 do
    if array.(i) > !max then
      max := array.(i);
  done;
  !max

let cost array distance =
  let cost = ref 0 in
  let n = Array.length array in
  for i = 0 to (n - 1) do
    let dst = (Int.abs (array.(i) - distance)) in
    let delta = (dst * (dst + 1)) / 2 in
    cost := !cost + delta;
  done;
  !cost;;

let process array =
  let n = max array in
  let mincost = ref 99999999999 in
  for i = 0 to n do
    let cost = cost array i in
    mincost := Int.min !mincost cost;
  done;
  !mincost

let input = (Array.of_seq (List.to_seq (List.map int_of_string (String.split_on_char ',' (List.hd (readfile "./input.txt"))))));;
Array.sort Int.compare input;;
process input;;
