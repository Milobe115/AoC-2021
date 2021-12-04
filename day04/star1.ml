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

(** Classes **)

class board = object (self) 
  val mutable _board = ([|[|0;0;0;0;0|];
                          [|0;0;0;0;0|];
                          [|0;0;0;0;0|];
                          [|0;0;0;0;0|];
                          [|0;0;0;0;0|]|] : int array array);
  val mutable _found_board = ([|[|'x';'x';'x';'x';'x'|];
                                [|'x';'x';'x';'x';'x'|];
                                [|'x';'x';'x';'x';'x'|];
                                [|'x';'x';'x';'x';'x'|];
                                [|'x';'x';'x';'x';'x'|]|] : char array array);
  method set_number i j n =
    (_board.(i)).(j) <- n;
  method get_number i j =
    _board.(i).(j);
  method get_found i j =
    _found_board.(i).(j);
  method check_value number =
    for i = 0 to 4 do
      for j = 0 to 4 do
        if _board.(i).(j) == number then
           _found_board.(i).(j) <- 'o';
      done
    done;
  method check_col j =
    let valid = ref true in
    for i = 0 to 4 do
      valid := !valid && (_found_board.(i).(j) == 'o');
    done;
    !valid
  method check_row i =
    let valid = ref true in
    for j = 0 to 4 do
      valid := !valid && (_found_board.(i).(j) == 'o');
    done;
    !valid
  method check_board =
    let complete = ref false in
    for i = 0 to 4 do
      complete := !complete || (self#check_row i);
    done;
    for j = 0 to 4 do
      complete := !complete || (self#check_col j);
    done;
    !complete
  method calculate_score final_number =
    let temp_score = ref 0 in
    for i = 0 to 4 do
      for j = 0 to 4 do
        if _found_board.(i).(j) == 'x' then
          temp_score := !temp_score + _board.(i).(j) 
      done
    done;
    !temp_score * final_number;
end;;

(** Functions **)

let rec remove_blanks list = match list with
    | [] -> []
    | hd :: tl when (String.length hd) == 0 -> remove_blanks tl
    | hd :: tl -> hd :: remove_blanks tl ;;

let process_line_to_board line board row =
  let l = List.map int_of_string (remove_blanks (String.split_on_char ' ' line)) in
  let rec pl2b_aux list board row col = match list with
    | [] -> ()
    | hd::tl -> board#set_number row col hd;
                pl2b_aux tl board row (col + 1) in
  pl2b_aux l board row 0;;
  

let parse_boards list =
  let rec pb_aux list boards row = match list, row with
    | [], 0 -> boards
    | [], _ -> raise Error
    | hd::tl, 0 ->
       let board = new board in
                   process_line_to_board hd board row;
                   pb_aux tl (board::boards) (row + 1)
    | hd::tl, 4 ->
       let board = List.hd boards in
                   process_line_to_board hd board row;
                   pb_aux tl boards 0
    | hd::tl, _ ->
       let board = List.hd boards in
                   process_line_to_board hd board row;
                   pb_aux tl boards (row + 1) in
  pb_aux list [] 0
;;
              
let print_board board =
  for i = 0 to 4 do
    for j = 0 to 4 do
      print_int (board#get_number i j);
      print_char (' ');
    done
  done;
  print_char('\n');
;;

let print_found board = 
  for k = 0 to 4 do
    for l = 0 to 4 do
      print_char (board#get_found k l);
      print_char (' ');
    done
  done;
  print_char '\n';
;;


let rec process_numbers numbers boards =
  let rec check_boards boards last_number = match boards with
    | [] -> -1
    | hd::tl -> if hd#check_board then
                  hd#calculate_score last_number
                else
                  check_boards tl last_number in
  match numbers with
  | [] -> -1
  | hd::tl -> List.iter (fun x -> x#check_value hd) boards;
              let score = check_boards boards hd in
              if score > -1 then
                score
              else
                process_numbers tl boards
;;


(** Processing **)

let input = readfile "./input.txt";;
let numbers = List.map int_of_string (String.split_on_char ',' (List.hd input));;
let boards = parse_boards (remove_blanks (List.tl input));;

List.iter print_found boards;;

process_numbers numbers boards;;

List.iter print_board boards;;
List.iter print_found boards;;
