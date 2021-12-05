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

type line = {
    x1: int;
    y1: int;
    x2: int;
    y2: int;
  };;

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
              if (x1 == x2) || (y1 == y2) then
                {x1 = (min x1 x2);
                 y1 = (min y1 y2);
                 x2 = (max x1 x2);
                 y2 = (max y1 y2)} :: (parse_lines tl)
              else
                parse_lines tl
;;

let add_vents line map =
  let nmap = ref map in
  match line with
  | line when line.x1 == line.x2 ->
     for i = line.y1 to line.y2 do
       nmap := PairsMap.update_stdlib (line.x1, i) (function |None -> Some 1 |Some y -> Some(y + 1)) !nmap;
     done;
     !nmap
  | line when line.y1 == line.y2 ->
     for i = line.x1 to line.x2 do
       nmap := PairsMap.update_stdlib (i, line.y1) (function |None -> Some 1 |Some y -> Some (y + 1)) !nmap;
     done;
     !nmap
  | _ -> raise Error
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
