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

let rec remove_blanks list = match list with
    | [] -> []
    | hd :: tl when (String.length hd) == 0 -> remove_blanks tl
    | hd :: tl -> hd :: remove_blanks tl ;;

let sort s = 
  String.to_seq s |> List.of_seq |> List.sort Char.compare |> List.to_seq |> String.of_seq;;

let rec split list = match list with
  | [] -> []
  | hd::tl -> (List.map (fun x -> remove_blanks (String.split_on_char ' ' x)) hd) :: split tl;;

let delta signal_1 signal_2 =
  (Seq.fold_left (+) 0 (Seq.map (fun c -> if(String.contains signal_1 c) then 0 else 1) (String.to_seq signal_2)))

let differentiate069 signal patterns = 
  if not (Hashtbl.mem patterns 1) then -1
  else
    let signal_1 = Hashtbl.find patterns 1 in
    if (delta signal signal_1) <> 0 then 6
    else if not (Hashtbl.mem patterns 4) then -1
    else (
      let signal_4 = Hashtbl.find patterns 4 in
      if (delta signal signal_4) == 0 then 9 else 6
    )
;;

let differentiate235 signal patterns=
  if not (Hashtbl.mem patterns 1) then -1
  else
    let signal_1 = Hashtbl.find patterns 1 in
    if (delta signal signal_1) == 0 then 3
    else if not (Hashtbl.mem patterns 4) then -1
    else (
      let signal_4 = Hashtbl.find patterns 4 in
      if (delta signal signal_4) == 2 then 2 else 5
    )
;;

let decode_pattern signal patterns = match String.length signal with
  | 2 -> 1
  | 3 -> 7
  | 4 -> 4
  | 7 -> 8
  | 5 -> differentiate235 signal patterns
  | _ -> differentiate069 signal patterns;;

let decode_signals signals =
  let rec ds_aux signals_remaining impossible_signals signalsByDigits digitsBySignals =
    match signals_remaining, impossible_signals with
    | [], [] -> signalsByDigits,digitsBySignals
    | [], _ -> ds_aux impossible_signals [] signalsByDigits digitsBySignals
    | hd::tl, _ ->
       let pattern = decode_pattern hd signalsByDigits in
       if pattern == -1 then
         ds_aux tl (hd::impossible_signals) signalsByDigits digitsBySignals
       else
         (Hashtbl.add signalsByDigits pattern hd;
          Hashtbl.add digitsBySignals hd pattern;
          ds_aux tl impossible_signals signalsByDigits digitsBySignals)
  in
  ds_aux signals [] (Hashtbl.create 10) (Hashtbl.create 10);;

let decode_output code digitsBySignals =
  let decoded_code = List.map (fun x -> Hashtbl.find digitsBySignals x) code in
  List.fold_left (fun x y -> 10 * x + y) 0 decoded_code;;

let process_one_line list = match list with
  | signals :: code :: [] ->
     let signalsByDigits, digitsBySignals = decode_signals (List.map sort signals) in
     decode_output (List.map sort code) digitsBySignals
  | _ -> raise (Error "liste invalide");;



let input = split (List.map (String.split_on_char '|') (readfile "./test.txt"));;
let answer = List.fold (+) 0 (List.map process_one_line input);;
