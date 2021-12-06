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

module IntMap = Map.Make(struct type t = int let compare = compare end);;

(** Functions *)

let int_of_int_option = function None -> raise Error | Some z -> z;;

let rec populate map list = match list with
  | [] -> map
  | hd :: tl -> let nmap = IntMap.update_stdlib hd (function None -> Some 1 | Some z -> Some(z+1)) map in
                populate nmap tl;;

let age_gen map =
  let rec ag_aux map list = match list with
    | [] -> map
    | (key, v) :: tl when key == 0 ->
       let n1 = IntMap.update_stdlib 6 (function None -> Some v | Some z -> Some (z + v)) map in
       let n2 = IntMap.update_stdlib 8 (function None -> Some v | Some z -> Some (z + v)) n1 in
       ag_aux n2 tl
    | (key, v) :: tl ->
       let nmap = IntMap.update_stdlib (key - 1) (function None -> Some v | Some z -> Some (z + v)) map in
       ag_aux nmap tl in
  ag_aux IntMap.empty (IntMap.bindings map);;

let rec process map gens = match gens with
  | 0 -> map
  | _ -> process (age_gen map) (gens - 1);;

let get_answer map = IntMap.fold (fun key value acc -> value + acc) map 0;;

(** Runtime *)

let print_map key occ =
  Printf.printf "%d : %d occurrences \n" key occ;;

let input = populate IntMap.empty (List.map int_of_string (String.split_on_char ',' (List.hd (readfile "./input.txt"))));;
let answer = get_answer(process input 256);;

IntMap.iter print_map answer;;
