open Board
open State
type comparator = GT | LT| GTE | LTE

let total_moves = ref 0;;
let turns = ref 0;;

(* Returns a move based on a simple set of heuristics as follows:*)
(* 1) Perform the move that moves us the most in the forward direction*)
(* 2) If tie for (1) then move the piece that is farthest back*)
(* 3) If tie for (2) then perform the move that puts us closest to the center vertical axis*)
(* 4) If tie for (3) then pick a random move from the remaining list*)

let baby_bot state player = 
	Random.self_init ();
  let move_set = available_moves state.board player in
	(* let stats_string = ("Move Stats:\t" ^ string_of_int(!turns) ^ "\t" ^ string_of_int(num_moves) ^ "\t" ^ string_of_float(float_of_int(!total_moves) /. float_of_int(!turns)) ^ "\n") in
	 prerr_endline stats_string; *)
	
	
	let vert_dist player (x1,y1,x2,y2, _, _) = 
		if player = P1 then y1-y2 else y2-y1 in
		
	let endzone_dist player (x1,y1,x2,y2, _, _) = 
		if player = P1 then 16-y1 else y1 in
		
	let center_dist player (x1,y1,x2,y2, _, _) = abs(12-x2) in
		
	let optimize f c (set,best) move =
		let result =  f player move in
			if result = best then (move::set,best) 
				else if (result > best && c = GT) || (result < best && c = LT) 
					then ([move],result)
				else (set,best) in
				
	let best_list = fst (List.fold_left (optimize vert_dist GT) ([],0) move_set) in
	let best_list = fst (List.fold_left (optimize endzone_dist LT) ([], 16) best_list) in
	let best_list = fst (List.fold_left (optimize center_dist LT) ([],12) best_list) in
			
	(* prerr_endline "Constructed moves list"; *)
 (* print_movelist best_list; *)
    match best_list with
    [] -> failwith "no moves"
		| _ -> List.nth best_list (Random.int (List.length best_list))
