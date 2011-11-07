open Board
open State

let cBOARD_SIZE = 4;;
let cWINNING_SCORE = 1000.;;

(*
let max_trans_table : (board, float) Hashtbl.t = Hashtbl.create 400000;;
let min_trans_table : (board, float) Hashtbl.t = Hashtbl.create 400000;;
*)

let add_to table board score = 
	Hashtbl.add table board score;;

let manhattan_eval player board = 
	let (x2, y2) = if player = P1 then (12,0) else (12,16) in
	let pieces = build_piece_list board player 0 0 [] in
	
	let fold_func sum (x1, y1) = 
		sum + abs(x2 - x1) + abs(y2 - y1) in
	
	let manhattan_dist = List.fold_left fold_func 0 pieces in
	let result = if player = P1 then manhattan_dist else -1 * manhattan_dist in
	float_of_int(result)
	
(* Should return a move *)
let alphabeta board player evaluate max_depth=
	let start = Unix.time() in
	let states_examined = ref 0 in
	
	(* Should return a value *)
	let rec max_value (board, alpha, beta) depth: float =
		(states_examined:= !states_examined + 1);
				let score = evaluate board in
				if depth = 0 then score else
					let successors = available_moves board P1 in
					let v = ref neg_infinity in
					let alpha = ref alpha in
					let rec run_through moves = 
						match moves with
							| [] -> !v
							| move::tail -> (
							let board' = do_move board move in
							let best' = min_value (board', !alpha, beta) (depth - 1) in
							v:= (max !v best');
							if !v >= beta then !v else (
								alpha:= (max !alpha !v);
								run_through tail)) in
					run_through successors 
	and min_value (board, alpha, beta) depth: float = 
		(states_examined:= !states_examined + 1);
				let score = evaluate board in
				if depth = 0 then score else
					let successors = available_moves board P2 in
					let v = ref infinity in
					let beta = ref beta in
					let rec run_through moves = 
						match moves with
							| [] -> !v
							| move::tail -> (
							let board' = do_move board move in
							let best' = max_value (board', alpha, !beta) (depth - 1) in
							v:= (min !v best');
							if !v <= alpha then !v else (
								beta:= (min !beta !v);
								run_through tail)) in
					run_through successors in	
	let successors = available_moves board player in 
	let best_move = ref (-1, -1, -1, -1) in
	let (start_func, comparison, best) = if player = P2 then (min_value, (>), ref neg_infinity) else (max_value, (<), ref infinity) in 
	let alpha = neg_infinity in
	let beta = infinity in
	let rec find_best moves =
		match moves with
			| [] -> !best_move 
			| x::t -> let board' = do_move board x in
				let best' = start_func (board', alpha, beta) max_depth in
				if comparison best' !best then (best:= best'; best_move:= x; find_best t) else find_best t in
	let best_move = find_best successors in
	let time_took = Unix.time() -. start in
	prerr_endline ("Examined: " ^ (string_of_int !states_examined) ^ " states");
	prerr_endline ("Took: " ^ (string_of_float time_took) ^ " seconds");
	best_move;;



				
				
			