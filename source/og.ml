(*     -----------------------------     *)
(*                ALPHABETA              *)
(*                ALPHABETA              *)
(*                ALPHABETA              *)
(*                ALPHABETA              *)
(*     -----------------------------     *)
(*     -----------------------------     *)
(*     -----------------------------     *)	

(*
let alphabeta board player =
	let max_trans_table : (board, float) Hashtbl.t = Hashtbl.create 400000 in
	let min_trans_table : (board, float) Hashtbl.t = Hashtbl.create 400000 in
	let states_examined = ref 0 in
	let rec max_value (board,alpha, beta): float =
		(states_examined:= !states_examined + 1);
		match Hashtbl.mem max_trans_table board with
			| true -> Hashtbl.find max_trans_table board 
			| false ->
				(match evaluate board with
				| Some (result) -> Hashtbl.add max_trans_table board result; result
				| None ->
					(let successors = available_moves board in
					let v = ref neg_infinity in
					let alpha = ref alpha in
					let rec run_through moves = 
						match moves with
							| [] -> !v
							| move::tail -> (
							let (board', next) = do_move board move P1 in
							let best' = if next = P1 then max_value (board', !alpha, beta) else min_value (board', !alpha, beta) in
							v:= (max !v best');
							if !v >= beta then !v else (
								alpha:= (max !alpha !v);
								run_through tail)) in
					let max_value = run_through successors in
					(Hashtbl.add max_trans_table board max_value); max_value))
	and min_value (board, alpha, beta): float = 
		(states_examined:= !states_examined + 1);
		match Hashtbl.mem min_trans_table board with
			| true  -> Hashtbl.find min_trans_table board 
			| false -> 
				(match evaluate board with
				| Some (result) -> Hashtbl.add min_trans_table board result; result
				| None -> 
					(let successors = available_moves board in
					let v = ref infinity in
					let beta = ref beta in
					let rec run_through moves = 
						match moves with
							| [] -> !v
							| move::tail -> (
							let (board', next) = do_move board move P2 in
							let best' = if next = P1 then max_value (board', alpha, !beta) else min_value (board', alpha, !beta) in
							v:= (min !v best');
							if !v <= alpha then !v else (
								beta:= (min !beta !v);
								run_through tail)) in
					let min_value = run_through successors in
					(Hashtbl.add min_trans_table board min_value); min_value)) in
	let successors = available_moves board in 
	let best_move = ref (-1, -1) in
	let (start_func, comparison, best) = if player = P1 then (min_value, (>), ref neg_infinity) else (max_value, (<), ref infinity) in
	let alpha = neg_infinity in
	let beta = infinity in
	let rec find_best moves =
		match moves with
			| [] -> !best_move 
			| x::t -> let (board', next) = do_move board x player in
				let best' = if next = P1 then max_value (board', alpha, beta) else min_value (board', alpha, beta) in
				if comparison best' !best then (best:= best'; best_move:= x; find_best t) else find_best t in
	let best_move = find_best successors in
	let player_string = if player = P1 then "player 1" else "player 2" in
	print_endline ("alphabeta " ^ player_string ^ " has value " ^ (string_of_float !best) ^ " with move " ^ (string_of_int (fst best_move)) ^ "," ^ (string_of_int (snd best_move)));
	print_endline ("Nodes visited " ^ (string_of_int !states_examined));
	print_endline ((string_of_int (Hashtbl.length max_trans_table)) ^ " (boards stored in transposition table for max_value)");
	print_endline ((string_of_int (Hashtbl.length min_trans_table)) ^ " (boards stored in transposition table for max_value)");
	best_move;;

*)

(*     -----------------------------     *)
(*     -----------------------------     *)
(*     -----------------------------     *)
(*                MINIMAX                *)
(*                MINIMAX                *)
(*                MINIMAX                *)
(*                MINIMAX                *)
(*     -----------------------------     *)
(*     -----------------------------     *)
(*     -----------------------------     *)	
(*
let minimax board player =
	let max_trans_table : (board, float) Hashtbl.t = Hashtbl.create 400000 in
	let min_trans_table : (board, float) Hashtbl.t = Hashtbl.create 400000 in
	let states_examined = ref 0 in
	let rec build_fold_func board player best move = 
		let comparison = if player = P1 then max else min in
		let (board', next) = do_move board move player in 
		let next_fun = if next = P1 then max_value else min_value in
		let best' = next_fun board' in
		comparison best best' 
		
	and max_value board: float =
		(states_examined:= !states_examined + 1);
		match Hashtbl.mem max_trans_table board with
			| true -> Hashtbl.find max_trans_table board 
			| false ->
				(match evaluate board with
				| Some (result) -> add_to max_trans_table board result; result
				| None ->
					(let successors = available_moves board in
					let fold_func = build_fold_func board P1 in
					let max_value = List.fold_left fold_func neg_infinity successors in
					(add_to max_trans_table board max_value); max_value))
	and min_value board: float = 
		(states_examined:= !states_examined + 1);
		match Hashtbl.mem min_trans_table board with
			| true  -> Hashtbl.find min_trans_table board 
			| false -> 
				(match evaluate board with
				| Some (result) -> add_to min_trans_table board result; result
				| None -> 
					(let successors = available_moves board in
					let fold_func = build_fold_func board P2 in
					let min_value = List.fold_left fold_func infinity successors in
					(add_to min_trans_table board min_value); min_value)) in
	let successors = available_moves board in 
	let best_move = ref (-1, -1) in
	let (start_func, comparison, best) = if player = P1 then (min_value, (>), ref neg_infinity) else (max_value, (<), ref infinity) in
	let rec find_best moves =
		match moves with
			| [] -> !best_move 
			| x::t -> let (board', next) = do_move board x player in
				let best' = if next = P1 then max_value board' else min_value board' in
				if comparison best' !best then (best:= best'; best_move:= x; find_best t) else find_best t in
	let best_move = find_best successors in
	let player_string = if player = P1 then "player 1" else "player 2" in
	print_endline ("minimax " ^ player_string ^ " has value " ^ (string_of_float !best) ^ " with move " ^ (string_of_int (fst best_move)) ^ "," ^ (string_of_int (snd best_move)));
	print_endline ("Nodes visited " ^ (string_of_int !states_examined));
	print_endline ((string_of_int (Hashtbl.length max_trans_table)) ^ " (boards stored in transposition table for max_value)");
	print_endline ((string_of_int (Hashtbl.length min_trans_table)) ^ " (boards stored in transposition table for max_value)");
	best_move;;

*)