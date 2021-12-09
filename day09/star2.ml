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

(** Types *)

module IntPair =
  struct
    type t = int * int
    let compare (x0,y0) (x1,y1) =
      match Stdlib.compare x0 x1 with
        0 -> Stdlib.compare y0 y1
      | c -> c
    let equals (x0, y0) (x1, y1) =
      x0 == x1 && y0 == y1
  end;;

module PairSet = Set.Make(IntPair);;

(** Functions *)

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
  let indexes = ref [] in
  for i = 0 to (n - 1) do
    for j = 0 to (p - 1) do
      let minimum = ref true in
      if i > 0 then minimum := !minimum && (array.(i-1).(j) > array.(i).(j));
      if j > 0 then minimum := !minimum && (array.(i).(j-1) > array.(i).(j));
      if i < (n-1) then minimum := !minimum && (array.(i+1).(j) > array.(i).(j));
      if j < (p-1) then minimum := !minimum && (array.(i).(j+1) > array.(i).(j));
      if !minimum then
        indexes := (i,j) :: !indexes;
    done;
  done;
  !indexes;;

let find_basin array minimum =
  let queue = Queue.create () in
  let flood_fill array queue =
    let n = Array.length array in
    let p = Array.length array.(0) in  
    let basin = ref PairSet.empty in
    let border = ref PairSet.empty in
    while (not (Queue.is_empty queue)) do
      let (i,j) = Queue.take queue in
      if (not (array.(i).(j) == 9)) then
        (basin := PairSet.add (i,j) !basin;
         if i > 0 then (
           if not ((PairSet.mem (i-1,j) !basin) || (PairSet.mem (i-1,j) !border)) then
             Queue.add (i-1,j) queue;
         );
         if j > 0 then (
           if not ((PairSet.mem (i,j-1) !basin) || (PairSet.mem (i,j-1) !border)) then
             Queue.add (i,j-1) queue;
         );
         if i < n - 1 then (
           if not ((PairSet.mem (i+1,j) !basin) || (PairSet.mem (i+1,j) !border)) then
             Queue.add (i+1,j) queue;
         );
         if j < p - 1 then (
           if not ((PairSet.mem (i,j+1) !basin) || (PairSet.mem (i,j+1) !border)) then
             Queue.add (i,j+1) queue;
         );
        )
      else
        border := PairSet.add (i,j) !border
    done;
    (PairSet.cardinal !basin)
  in
  Queue.add minimum queue;
  flood_fill array queue;;

let rec find_basins array list =
  match list with
  | [] -> []
  | hd :: tl ->
     (find_basin array hd) :: (find_basins array tl) ;;

let three_maxes list =
  let rec tm_aux list max1 max2 max3 = match list with
    | [] -> max1, max2, max3, max1 * max2 * max3
    | hd::tl -> if hd > max1 then
                  tm_aux tl hd max1 max2
                else if hd > max2 then
                  tm_aux tl max1 hd max2
                else if hd > max3 then
                  tm_aux tl max1 max2 hd
                else
                  tm_aux tl max1 max2 max3 in
  tm_aux list 0 0 0;;

let input = Array.of_seq (List.to_seq (parse_array (readfile "./input.txt")));;
let indexes = process input;;
let basins = find_basins input indexes;;
let answer = three_maxes basins;;
