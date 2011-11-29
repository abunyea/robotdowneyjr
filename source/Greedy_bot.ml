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

let greedy_bot state player = 
	Random.self_init ();
  let move_set = available_moves state.board player in
	(* let stats_string = ("Move Stats:\t" ^ string_of_int(!turns) ^ "\t" ^ string_of_int(num_moves) ^ "\t" ^ string_of_float(float_of_int(!total_moves) /. float_of_int(!turns)) ^ "\n") in
	 prerr_endline stats_string; *)
	let (target_x, target_y) = if player = P1 then (12, 0) else (12, 16) in
	
	let dist (x, y) = 
		let dr = target_y - y in
		let ds = (x + y - target_x - target_y)/2 in
		if dr * ds > 0 then ((abs dr) + (abs ds)) else (max (abs dr) (abs ds)) in
	
	let fold_func (max_dist, list_so_far) move =
		let (x1, y1, x2, y2, _, _) = move in
		let curr_dist = (dist (x1, y1)) - (dist (x2, y2)) in
		if curr_dist < max_dist then (max_dist, list_so_far) else
			(if curr_dist = max_dist then (max_dist, move::list_so_far) else (curr_dist, [move])) in
	let best_list = List.fold_left fold_func (0, []) move_set in
	
				
			
	(* prerr_endline "Constructed moves list"; *)
 (* print_movelist best_list; *)
    match snd best_list with
    [] -> failwith "no moves"
		| x::t -> x
