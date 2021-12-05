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

type vline = {
    x: int;
    y1: int;
    y2: int;
  };;

type hline = {
    x1: int;
    x2: int;
    y: int;
  };;

type dline = {
    x1: int;
    y1: int;
    x2: int;
    y2: int;
  };;

type line =
  |H of hline
  |V of vline
  |D of dline;;

type vent = {
    x: int;
    y: int;
    n: int;
  };;

module IntPairs =
  struct
    type t = int * int
    let compare (x0,y0) (x1,y1) =
      match Stdlib.compare x0 x1 with
        0 -> Stdlib.compare y0 y1
      | c -> c
    let equals (x0, y0) (x1, y1) =
      x0 == x1 && y0 == y1
  end;;

module PairsMap = Map.Make(IntPairs);;

(** Functions *)

let rec parse_lines list = match list with
  | [] -> []
  | hd::tl -> let regex = Str.regexp "\\([0-9]+\\),\\([0-9]+\\) -> \\([0-9]+\\),\\([0-9]+\\)" in
              let x1 = int_of_string (Str.replace_first regex "\\1" hd) in
              let y1 = int_of_string (Str.replace_first regex "\\2" hd) in
              let x2 = int_of_string (Str.replace_first regex "\\3" hd) in
              let y2 = int_of_string (Str.replace_first regex "\\4" hd) in
              if (x1 == x2) then
                V({x = x1; y1 = (min y1 y2); y2 = (max y1 y2)}):: (parse_lines tl)
              else if (y1 == y2) then
                H({x1 = (min x1 x2); y = y1; x2 = (max x1 x2)}):: (parse_lines tl)
              else
                D({x1 = x1; y1 = y1; x2 = x2; y2 = y2})::parse_lines tl
;;

let add_vents line map =
  let nmap = ref map in
  match line with
  | V(vline) ->
     for i = vline.y1 to vline.y2 do
       nmap := PairsMap.update_stdlib (vline.x, i) (function |None -> Some 1 |Some y -> Some(y + 1)) !nmap;
     done;
     !nmap
  | H(hline) ->
     for i = hline.x1 to hline.x2 do
       nmap := PairsMap.update_stdlib (i, hline.y) (function |None -> Some 1 |Some y -> Some (y + 1)) !nmap;
     done;
     !nmap
  | D(dline) ->
     let x0 = dline.x1 and y0 = dline.y1 in
     let delta = abs (dline.x2 - dline.x1) in
     let dir_x = if dline.x1 < dline.x2 then 1 else -1 in
     let dir_y = if dline.y1 < dline.y2 then 1 else -1 in
     for i = 0 to delta do
       nmap := PairsMap.update_stdlib (x0 + dir_x * i, y0 + dir_y * i) (function |None -> Some 1 |Some y -> Some (y + 1)) !nmap;
     done;
     !nmap
;;

let get_vents list =
  let rec gv_aux list map = 
    match list with
    | [] -> map
    | hd :: tl -> let nmap = add_vents hd map in
                  gv_aux tl nmap  in
  gv_aux list PairsMap.empty;;

(** Runtime *)

let print_map key occ =
  let (x, y) = key in
  Printf.printf "Case %d %d : %d occurrences \n" x y occ;;

let input = readfile "./input.txt";;
let lines = parse_lines input;;
let vents = get_vents lines;;
let danger = PairsMap.map (fun occ -> if occ > 1 then 1 else 0) vents ;;
let value = PairsMap.fold (fun key occ acc -> occ + acc) danger 0;;
