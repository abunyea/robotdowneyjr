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

let test_bot state player = 
	Random.self_init ();
  let move_set = available_moves state.board player in
	
	
	let get_grey_move() : int * int = 
		let board = state.board in
		let iter_func (y,x) = 
			prerr_endline (String.make 1 (space_to_char board.(y).(x))) in
		let rec find_empty home_coords =
			match home_coords with
				| [] -> (-1, -1)
				| (y, x)::t -> if state.board.(y).(x) = Empty then (x, y) else find_empty t in
		let (goal_positions, remaining) = if player = P1 then (get_home_coords P2, state.grey_remain_1) 
			else (get_home_coords P1, state.grey_remain_2) in
			List.iter iter_func goal_positions;
			prerr_endline "";
		if remaining = 0 then (-1, -1) else find_empty goal_positions in
		
	let num_moves = List.length move_set in
	total_moves:= !total_moves + num_moves;
	turns:= !turns + 1;
	let stats_string = ("Move Stats:\t" ^ string_of_int(!turns) ^ "\t" ^ string_of_int(num_moves) ^ "\t" ^ string_of_float(float_of_int(!total_moves) /. float_of_int(!turns)) ^ "\n") in
	prerr_endline stats_string;
	
	
	let vert_dist player (x1,y1,x2,y2, _, _ ) = 
		if player = P1 then y1-y2 else y2-y1 in
		
	let endzone_dist player (x1,y1,x2,y2, _, _ ) = 
		if player = P1 then 16-y1 else y1 in
		
	let center_dist player (x1,y1,x2,y2, _, _ ) = abs(12-x2) in
		
	let optimize f c (set,best) move =
		let result =  f player move in
			if result = best then (move::set,best) 
				else if (result > best && c = GT) || (result < best && c = LT) 
					then ([move],result)
				else (set,best) in
				
	let best_list = 
		fst (List.fold_left (optimize center_dist LT) ([],12) (
		fst (List.fold_left (optimize endzone_dist LT) ([],16) (
		fst (List.fold_left (optimize vert_dist GT) ([],0) move_set))))) in
			
	prerr_endline "Constructed moves list";
  print_movelist best_list;
    match best_list with
    [] -> failwith "no moves"
		| _ -> let (x1, y1, x2, y2, _, _) = List.nth best_list (Random.int (List.length best_list)) in
			let (x3, y3) = get_grey_move() in
			(x1, y1, x2, y2, y3, x3)
