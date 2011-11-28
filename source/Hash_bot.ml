open Board
open State
open Hashtbl


let cWINNING_SCORE = 1000.;;

let trans_table : (board, float) Hashtbl.t = Hashtbl.create 50000;;

(* REQUIRES *)
(* Evaluate function : player -> state -> float  *)

let add_to table board score = 
	Hashtbl.add table board score;;


let num_moves_dif player state =
	let board = state.board in
	let other_player = if player = P1 then P2 else P1 in
	let our_pieces = build_piece_list board player 0 0 [] in
	let their_pieces = build_piece_list board other_player 0 0 [] in
	
	let build_num_moves player sum (x, y) = 
		let (x2, y2) = if player = P1 then (12,0) else (12,16) in
		let v_dist = abs(y2 - y) in
		let moves = (if x = 12 || (x > 12 && ((x - y) <= 12)) || (x < 12 && ((x + y) >= 12)) then
			v_dist else
			v_dist + (if (x > 12) then (x - y - 12) / 2 else (12 - (x+y))/2)) in
		sum + moves in
		
	let our_fold_func = build_num_moves player in
	let their_fold_func = build_num_moves other_player in
	
	let dif = (List.fold_left their_fold_func 0 their_pieces) - (List.fold_left our_fold_func 0 our_pieces) in
	float_of_int dif


let num_moves_dif1 player state =
	let board = state.board in
	let other_player = toggle_player player in
	let our_pieces = build_piece_list board player 0 0 [] in
	let their_pieces = build_piece_list board other_player 0 0 [] in
	
	let build_num_moves player sum (x, y) = 
		let home_spots = if player = P1 then get_home_coords P2 else get_home_coords P1 in
		let rec find_first_empty spots = 
			match spots with
				| [] -> failwith "Fuck"
				| (y,x)::[] -> (x, y)
				| (y,x)::t -> if board.(y).(x) = Empty then (x, y) else find_first_empty t in
		let (x2, y2) = find_first_empty home_spots in
		let v_dist = abs(y2 - y) in
		let moves = (if x = 12 || (x > 12 && ((x - y) <= 12)) || (x < 12 && ((x + y) >= 12)) then
			v_dist else
			(v_dist + ((abs (x - x2)) / 2))) in
		sum + moves in
		
	let our_fold_func = build_num_moves player in
	let their_fold_func = build_num_moves other_player in
	
	let dif = (List.fold_left their_fold_func 0 their_pieces) - (List.fold_left our_fold_func 0 our_pieces) in
	float_of_int dif
	
	
let furthest_back player state = 
	let board = state.board in
	let pieces = build_piece_list board player 0 0 [] in
	let (x_home, y_home) = if player = P1 then (12,16) else (12, 0) in
	let y_distances = List.map (fun (x,y) -> abs(y_home - y)) pieces in
	float_of_int (List.fold_left min 100 y_distances)
	
let our_eval player state = 
	let result = (num_moves_dif player state) +. (furthest_back player state) in
	if player = P1 then result else -1.0 *. result

let modified_eval player state =
	let result = (num_moves_dif1 player state) +. (furthest_back player state) in
	if player = P1 then result else -1.0 *. result
	
let add_to table board score = 
	Hashtbl.add table board (abs_float score)
		
(*let manhattan_eval player state = 
	let board = state.board in
	let (x2, y2) = if player = P1 then (12,0) else (12,16) in
	let pieces = build_piece_list board player 0 0 [] in
	
	let fold_func sum (x1, y1) = 
		sum + abs(y2 - y1) + abs(x1 -x2) in
	
	let v_dist = List.fold_left fold_func 0 pieces in
	let result = if player = P1 then v_dist else -1 * v_dist in
	float_of_int(result) *)
	
(* Returns the sum of the vertical distances of all*)
(* a players pieces from its home position 
let v_dist_eval player state = 
	let board = state.board in
	let (x2, y2) = if player = P1 then (12,0) else (12,16) in
	let pieces = build_piece_list board player 0 0 [] in
	
	let fold_func sum (x1, y1) = 
		sum + abs(y2 - y1) in
	
	let v_dist = List.fold_left fold_func 0 pieces in
	let result = if player = P1 then v_dist else -1 * v_dist in
	float_of_int(result) *)


(* Should return a move *)
let alphabeta evaluate max_depth state player =
	let start = Unix.time() in
	let states_examined = ref 0 in
	
	(* Should return a value *)
	let rec max_value (state, alpha, beta) depth: float =
		(states_examined:= !states_examined + 1);
		let board = state.board in
		if Hashtbl.mem trans_table board then (Hashtbl.find trans_table board) else
				if depth = 0 
				then 
					(let score = evaluate player state in 
					 add_to trans_table board score;
					 score)
				else
					(let successors = available_moves state.board P1 in
					let v = ref neg_infinity in
					let alpha = ref alpha in
					let rec run_through moves = 
						match moves with
							| [] -> !v
							| move::tail -> (
							let state' = update_board state move in
							let best' = min_value (state', !alpha, beta) (depth - 1) in
							v:= (max !v best');
							if !v >= beta then !v else (
								alpha:= (max !alpha !v);
								run_through tail)) in
					let score = run_through successors in
					add_to trans_table board score;
					score)
	and min_value (state, alpha, beta) depth: float = 
		let board = state.board in
		(states_examined:= !states_examined + 1);
		if Hashtbl.mem trans_table board then (-1.0 *. (Hashtbl.find trans_table board)) else
				if depth = 0 
				then 
					(let score  = evaluate player state in
					add_to trans_table board score;
					score)
				else
					(let successors = available_moves state.board P2 in
					let v = ref infinity in
					let beta = ref beta in
					let rec run_through moves = 
						match moves with
							| [] -> !v
							| move::tail -> (
							let state' = update_board state move in
							let best' = max_value (state', alpha, !beta) (depth - 1) in
							v:= (min !v best');
							if !v <= alpha then !v else (
								beta:= (min !beta !v);
								run_through tail)) in
					let score = run_through successors in	
					add_to trans_table board score;
					score) in
	let successors = available_moves state.board player in 
	let best_move = ref (-1, -1, -1, -1, -1, -1) in
	let (start_func, comparison, best) = if player = P1 then (min_value, (>), ref neg_infinity) else (max_value, (<), ref infinity) in 
	let alpha = neg_infinity in
	let beta = infinity in
	let rec find_best moves =
		match moves with
			| [] -> !best_move 
			| x::t -> let state' = update_board state x in
				let best' = start_func (state', alpha, beta) max_depth in
				if comparison best' !best then (best:= best'; best_move:= x; find_best t) else find_best t in
	let best_move = find_best successors in
	let time_took = Unix.time() -. start in
	prerr_endline ("Examined: " ^ (string_of_int !states_examined) ^ " states");
	prerr_endline ("Took: " ^ (string_of_float time_took) ^ " seconds");
	best_move;;


(* We require a bot to be of type state -> player -> move *)
(* However, alphabeta is of the form (player -> state -> float) -> int -> state -> player -> move*)
(* So this function just does a little currying to make it seem prettier *)
let build_minimax_bot evaluate max_depth = 
	alphabeta evaluate max_depth;;
 
let basic_alphabeta_bot = build_minimax_bot our_eval 2;;
let hash_bot = build_minimax_bot modified_eval 2;;
				
				
			