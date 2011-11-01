type space = P1 | P2 | Empty;;
type result = Player1 | Player2 | Tie | NotOver;;
type board = space array array;;

let cBOARD_SIZE = 4;;


let get_winner board =
	let game_over = ref true in 
	let score_row (p1_score, p2_score) space = 
		match space with 
			| P1 -> (p1_score + 1, p2_score)
			| P2 -> (p1_score, p2_score + 1)
			| Empty -> (game_over:= false); (p1_score, p2_score)
		in
	let (p1_score, p2_score) = Array.fold_left (fun score row -> Array.fold_left score_row score row) (0, 0) board in
	if !game_over then (if p1_score > p2_score then Player1 else (if p1_score < p2_score then Player2 else Tie)) else NotOver;;
	

	
let evaluate (board  : board) : float option = 
		match get_winner board with
			| Player1 -> Some 1.0
			| Player2 -> Some (-1.0)
			| Tie -> Some 0.0
			| NotOver -> None

let cWINNING_SCORE = 1000;;

let evaluate1 board =
	1000.;; 

let make_board() = 
	let board = Array.make cBOARD_SIZE (Array.make cBOARD_SIZE Empty) in
	let rec fill_rows row = 
		if row == cBOARD_SIZE then () else
			(board.(row) <- Array.make cBOARD_SIZE Empty; fill_rows (row + 1); ()) in
	fill_rows 0; board;;
		
let rotate board =
	let new_board = make_board() in
	new_board.(0).(0) <- board.(3).(0);
	new_board.(0).(1) <- board.(2).(0);
	new_board.(0).(2) <- board.(1).(0);
	new_board.(0).(3) <- board.(0).(0);
	new_board.(1).(0) <- board.(3).(1);
	new_board.(1).(1) <- board.(2).(1);
	new_board.(1).(2) <- board.(1).(1);
	new_board.(1).(3) <- board.(0).(1);
	new_board.(2).(0) <- board.(3).(2);
	new_board.(2).(1) <- board.(2).(2);
	new_board.(2).(2) <- board.(1).(2);
	new_board.(2).(3) <- board.(0).(2);
	new_board.(3).(0) <- board.(3).(3);
	new_board.(3).(1) <- board.(2).(3);
	new_board.(3).(2) <- board.(1).(3);
	new_board.(3).(3) <- board.(0).(3);
	new_board;;

let add_to table board result = 
	let board1 = rotate board in
	let board2 = rotate board1 in
	let board3 = rotate board2 in
	Hashtbl.add table board result;
	Hashtbl.add table board1 result;
	Hashtbl.add table board2 result;
	Hashtbl.add table board3 result; 
	();;
	
let copy_board board = 
	let new_board = make_board() in
	let rec helper index =
		if index == cBOARD_SIZE then new_board else
		(new_board.(index) <- Array.copy board.(index); helper (index + 1)) in
	helper 0;;
	
let other_player space = match
 space with
	| P1 -> P2
	| P2 -> P1
	| Empty -> failwith "Invalid space to other_player. Empty";;


(* Executes the move, returning a tuple (new_board, next_turn) *)
let do_move board (x,y) space : board * space = 
	let new_board = copy_board board in
	if new_board.(y).(x) <> Empty then failwith "Invalid Move. Square is not Empty" else
		(new_board.(y).(x) <- space); 
	let spaces_to_check = [(x, y - 1); (x, y + 1); (x - 1, y); (x + 1, y)] in
	let spaces_to_check = List.filter (fun (x,y) -> x >= 0 && x < cBOARD_SIZE && y>= 0 && y < cBOARD_SIZE) spaces_to_check in
	let filled_a_square = ref false in	
	let check_space (x, y) = 
	if ( (y = 0 || new_board.(y - 1).(x) == space) &&
				 (y = cBOARD_SIZE - 1 || new_board.(y + 1).(x) == space) &&
				 (x = 0 || new_board.(y).(x - 1) == space) &&
				 (x = cBOARD_SIZE - 1 || new_board.(y).(x + 1) == space)) then 
			((new_board.(y).(x) <- space); (filled_a_square:= true)) else () in
	(List.iter check_space spaces_to_check); 
	if !filled_a_square then (new_board, space) else (new_board, other_player space);;
	

let print_board board = 
	let print_space space = 
		match space with
			| P1 -> print_string "1 "
			| P2 -> print_string "2 "
			| Empty -> print_string "0 "
		in
	let print_row row = 
		((Array.iter print_space row);
		print_newline()) in
	Array.iter print_row board;;

(* Run through the board and get a list of empty squares *)
let get_available_moves board = 
	let rec run_through_row x y moves =
		if x = cBOARD_SIZE then moves else 
			if board.(y).(x) = Empty then run_through_row (x + 1) y ((x,y)::moves) else
				run_through_row (x + 1) y moves in
	let rec run_through_board y moves = 
		if y = cBOARD_SIZE then moves else
			run_through_board (y + 1) (run_through_row 0 y moves) in
	run_through_board 0 [];;
		

	
let set board x y piece =
	board.(y - 1).(x - 1) <- piece;;
	
(*     -----------------------------     *)
(*     -----------------------------     *)
(*     -----------------------------     *)
(*                ALPHABETA              *)
(*                ALPHABETA              *)
(*                ALPHABETA              *)
(*                ALPHABETA              *)
(*     -----------------------------     *)
(*     -----------------------------     *)
(*     -----------------------------     *)	
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
					(let successors = get_available_moves board in
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
					(let successors = get_available_moves board in
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
	let successors = get_available_moves board in 
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
				(match evaluate board withv
				| Some (result) -> add_to max_trans_table board result; result
				| None ->
					(let successors = get_available_moves board in
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
					(let successors = get_available_moves board in
					let fold_func = build_fold_func board P2 in
					let min_value = List.fold_left fold_func infinity successors in
					(add_to min_trans_table board min_value); min_value)) in
	let successors = get_available_moves board in 
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



(*****************************************************)
(*****************************************************)
(*****************************************************)
(*****************************************************)
(*****************************************************)
(*****************************************************)
(*****************************************************)
(*****************************************************)
(*****************************************************)
(*****************************************************)
(*****************************************************)



let add_to table board score = 
	Hashtbl.add table board score;;

(* Should return a move *)
let alphabeta board player max_depth=
	let max_trans_table : (board, float) Hashtbl.t = Hashtbl.create 400000 in
	let min_trans_table : (board, float) Hashtbl.t = Hashtbl.create 400000 in
	let states_examined = ref 0 in
	(* Should return a value *)
	let rec max_value (board, alpha, beta) depth: float =
		(states_examined:= !states_examined + 1);
		match Hashtbl.mem max_trans_table board with
			| true -> Hashtbl.find max_trans_table board 
			| false ->
				let score = evaluate1 board in
				if depth = 0 || score = cWINNING_SCORE then (add_to max_trans_table board score; score) else
					(let successors = get_available_moves board in
					let v = ref neg_infinity in
					let alpha = ref alpha in
					let rec run_through moves = 
						match moves with
							| [] -> !v
							| move::tail -> (
							let (board', next) = do_move board move P1 in
							let best' = if next = P1 then max_value (board', !alpha, beta) (depth - 1) else min_value (board', !alpha, beta) (depth - 1) in
							v:= (max !v best');
							if !v >= beta then !v else (
								alpha:= (max !alpha !v);
								run_through tail)) in
					let max_value = run_through successors in
					(Hashtbl.add max_trans_table board max_value); max_value)
	and min_value (board, alpha, beta) depth: float = 
		(states_examined:= !states_examined + 1);
		match Hashtbl.mem min_trans_table board with
			| true  -> Hashtbl.find min_trans_table board 
			| false -> 
				let score = evaluate1 board in
				if depth = 0 || score = cWINNING_SCORE then (add_to min_trans_table board score; score) else
					(let successors = get_available_moves board in
					let v = ref infinity in
					let beta = ref beta in
					let rec run_through moves = 
						match moves with
							| [] -> !v
							| move::tail -> (
							let (board', next) = do_move board move P2 in
							let best' = if next = P1 then max_value (board', alpha, !beta) (depth - 1) else min_value (board', alpha, !beta) (depth - 1) in
							v:= (min !v best');
							if !v <= alpha then !v else (
								beta:= (min !beta !v);
								run_through tail)) in
					let min_value = run_through successors in
					(Hashtbl.add min_trans_table board min_value); min_value) in
	let successors = get_available_moves board in 
	let best_move = ref (-1, -1) in
	let (start_func, comparison, best) = if player = P1 then (min_value, (>), ref neg_infinity) else (max_value, (<), ref infinity) in
	let alpha = neg_infinity in
	let beta = infinity in
	let rec find_best moves =
		match moves with
			| [] -> !best_move 
			| x::t -> let (board', next) = do_move board x player in
				let best' = if next = P1 then max_value (board', alpha, beta) max_depth else min_value (board', alpha, beta) max_depth in
				if comparison best' !best then (best:= best'; best_move:= x; find_best t) else find_best t in
	let best_move = find_best successors in
	let player_string = if player = P1 then "player 1" else "player 2" in
	print_endline ("alphabeta " ^ player_string ^ " has value " ^ (string_of_float !best) ^ " with move " ^ (string_of_int (fst best_move)) ^ "," ^ (string_of_int (snd best_move)));
	print_endline ("Nodes visited " ^ (string_of_int !states_examined));
	print_endline ((string_of_int (Hashtbl.length max_trans_table)) ^ " (boards stored in transposition table for max_value)");
	print_endline ((string_of_int (Hashtbl.length min_trans_table)) ^ " (boards stored in transposition table for max_value)");
	best_move;;



				
				
			