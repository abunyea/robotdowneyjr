open Minimax_bot;;
open Baby_bot;;
open Evaluations;;
open Greedy_bot;;
open Mustache;;
open Print;;
open State;;
open Board;;
open Unix;;
open Reader;;

type weight_vector = int list;;
type score = int option;;
type node = weight_vector * score;;
let evaluater_bot = greedy_bot;;

Random.self_init();;
let cNUM_ITERATIONS = 75;; 
let cRANDOM_CANDIDATES = 2;;
let cBEST_CANDIDATES = 3;;
let cPOOL_SIZE = cRANDOM_CANDIDATES + cBEST_CANDIDATES;;
let cNUM_MOVES = 50;;
let cNUM_SUCCESSORS = 3;;
let path = "beam" ^ (string_of_int (Random.int 200)) ^ ".txt";;
print_endline ("Output written to: " ^ path);;

let shuffle_list lst = 
		List.sort (fun _ _ -> Random.int 3 - 1) lst
		
let file = ref None;;
let open_file() = file:= (Some (open_out_gen [Open_creat; Open_append] 0o666 path));;
let close_file() = match !file with
	| None -> ()
	| Some path -> close_out_noerr path; file:= None;;

let print_str str = match !file with
	| Some path -> output_string path str
	| None -> open_file(); (
		match !file with
			| None -> failwith "wtf"
			| Some x -> output_string x str);;

let print_line str = print_str (str ^ "\n");;

let compare_node (_, score1) (_, score2) = 
	match (score1, score2) with
		| (Some x1, Some x2) -> compare x2 x1 (* makes it so List.sort will put the smallest values near front (x1, x2) / biggest (x2, x1) *)
		| _ -> failwith "A node doesn't have a score when we're trying to harvest them";;

let build_minimax (f1, f2, f3, f4, f5) weights = 
	
	let evaluate_one player state = 
		let pieces = build_piece_list state.board player 0 0 [] in
		let avail_moves = available_moves state.board player in
		match weights with
			| (w1, d1)::(w2, d2)::(w3, d3)::(w4, d4)::(w5, d5)::[] -> w1 *. (f1 d1 player pieces) +. w2 *. (f2 d2 player pieces) +. w3 *. (f3 d3 player pieces) -. w4 *. (f4 d4 player avail_moves) +. w5 *. (f5 player pieces)**d5
			| _ -> failwith "problem in build_minimax" in
		let evaluate player state 
		= (evaluate_one (toggle_player player) state) -. (evaluate_one player state) in
	build_minimax_bot evaluate 0;;

let print_node (weight_vector, score) =
	let weight_vector = List.map (fun (w,d) -> (string_of_float w, string_of_float d)) weight_vector in
	let weight_str = (match weight_vector with
		| (w1, d1)::(w2, d2)::(w3, d3)::(w4, d4)::(w5, d5)::[] -> ("(" ^ w1 ^ ", " ^ d1 ^ ", " ^ w2 ^ ", " ^ d2 ^ ", " ^ w3 ^ ", " ^ d3 ^ ", " ^ w4 ^ ", " ^ d4 ^ "," ^ w5 ^ ", " ^ d5 ^ ")")
		| _ -> failwith "invalid weight_vector in print_node") in
	print_str weight_str;
	print_string weight_str;
	print_line "";
	print_endline "";
	(match score with
		| None -> print_line "No score yet"; print_endline "No score yet";
		| Some x -> print_line (string_of_float x); print_endline (string_of_float x) );
	print_line "";;

let print_round nodes iteration time_took =
	let it_str = ("Iteration " ^ (string_of_int iteration)) in
	print_endline it_str;
	print_line it_str;
	let time_str = (" Took: " ^ (string_of_float time_took) ^ " seconds") in 
	print_line time_str;
	print_endline time_str;
	List.iter print_node nodes
	
	
	

let generate_random_weights n = 
	let get_weight() = 
		let weight = Random.float 2. in
		weight in
	let rec generate_weight_vector weights num = 
		if num = n then weights else
			generate_weight_vector (get_weight()::weights) (num + 1) in
	let rec generate_candidates candidates num =
		if num = cPOOL_SIZE then candidates else
			generate_candidates ((generate_weight_vector [] 0)::candidates) (num + 1) in
	generate_candidates [] 0;;

(* Return number of moves baby_bot would take to finish. Assumes we're player 1 *)
let calculate_score state =
	let score_p2 = (evaluate2 evaluater_bot P2 state) in
	let score_p1 = (evaluate2 evaluater_bot P1 state) in
	score_p2 -. score_p1

let better_calculate_score state =
	let score_p2 = (evaluate2 evaluater_bot P2 state) in
	let score_p1 = (evaluate2 evaluater_bot P1 state) in
	(score_p2 -. score_p1, score_p1 -. score_p2)
	
(* Checks if all the nodes have a score. *)
let check_if_done nodes = 
	let scored = List.map (fun (w, s) -> match s with | None -> false | _ -> true) nodes in
	not (List.mem false scored)
	
(* Takes the top cBEST_CANDIDATES nodes and then cRANDOM_CANDIDATES random nodes *)
let harvest_nodes nodes = 
	let sorted = List.sort compare_node nodes in
		
	let take lst n =
		let rec helper taken not_taken num_taken =
			if num_taken = n then taken else (
				match not_taken with
					| [] -> taken
					| x::t -> helper (x::taken) t (num_taken + 1)) in
		helper [] lst 0 in
	let rec take_best (best, rest) num_left= 
		match rest with
			| [] -> (best, rest)
			| x::t -> if num_left = 0 then (best, rest) else (take_best (x::best, t) (num_left - 1)) in
	let (best, rest) = take_best ([], sorted) cBEST_CANDIDATES in
	let rest = shuffle_list rest in
	(best @ (take rest cRANDOM_CANDIDATES))
	
	

(* Generates successors of a given weight_vector*)
(* A successor is just a modified weight_vector, the trick is how are we *)
(* going to generate the successors. Each node takes approximately 3 seconds to *)
(* evaluate, a node's successors will take #successors * 3 seconds to evaluate *)
(* This means each iteration will take cPOOL_SIZE * #successors * 3 seconds to run*)
(* and the whole program will take cNUM_ITERATIONS * cPOOL_SIZE * #successors * 3 seconds to run *)

(* So what we are doing right now is we take each weight and we add a random value between -.05 and .05 to it*)
let generate_successors nodes _ =
	let degree_mutation() = 
		(Random.float 0.2) -. 0.1 in
	let weight_mutation() =
		(Random.float 1.) -. 0.5 in
	let rec generate_one (weights, score) successors =
		if (List.length successors) = cNUM_SUCCESSORS 
			then 
				(weights, score)::successors 
			else (
				let new_weights = List.map (fun (w, d) -> (w +. weight_mutation(), d +. degree_mutation())) weights in
				generate_one (weights, score) ((new_weights, None)::successors)
			) in
	let successors = List.map (fun node -> generate_one node []) nodes in
	List.flatten successors
	
(* Find weights takes a list of feature functions [f1; f2; ... fn] *)
(* and returns weights [w1; ... wn] for the best evaluation function*)
(* w1 * f1(board) + w2 * f2(board) ... *)
(* It does this by conducting a beam search, using minimax of depth 1 (equivilent to baby bot) *)
let find_weights() =
	(* Build a blank board *)
	let initial_state = get_initial_state() in
	let features = (stalin_dist, hogan_dist, dali_dist, teutul_dist, Mustache.furthest_back) in
	
	(* Plays cNUM_MOVES with a bot with the specified weights and baby_bot*)
	(* then returns the number of moves that the minimax bot is winning by*)
	(* (negative if it is losing) *)
	let play_game (weights, score) =
			let bot = build_minimax features weights in (*Build our bot *)
			(* If we've played cNUM_MOVEs return a score. Else play a move for us, then opponent, then recurse*)
			let rec play_game_helper state move_num =
				if ((move_num = cNUM_MOVES) || (is_game_over state.board)) then calculate_score state else
				(
				let state = update_board state (bot state P1) in
				let state = update_board state (evaluater_bot state P2) in
				play_game_helper state (move_num + 1)) in
			match score with
				| Some x -> (weights, Some x)
				| None -> (weights, Some (play_game_helper initial_state 0))
			 in
	
	let rec beam_search candidates iteration = 
		if (iteration = cNUM_ITERATIONS) 
		then 
			candidates 
		else (
			(* Want to take cBEST_CANDIDATES, and cRANDOM_CANDIDATES random candidates*)
			(* Then generate successors for all. Score them all, and run beam search again on them *)
			print_endline ("Starting iteration " ^ (string_of_int iteration) ^ "...");
			let start_time = Unix.time() in
			let pool = generate_successors (candidates) iteration in
			print_endline ("Scoring " ^ (string_of_int (List.length pool)) ^ " nodes");
			let scored_pool = List.map play_game pool in
			let nodes = harvest_nodes scored_pool in 
			let time_took = Unix.time() -. start_time in
			print_round nodes iteration time_took;
			beam_search nodes (iteration + 1)
		) in
	
		let better_play_game (weights1, score1) (weights2, score2) =
			let bot1 = build_minimax features weights1 in (*Build our bot *)
			let bot2 = build_minimax features weights2 in
			(* If we've played cNUM_MOVEs return a score. Else play a move for us, then opponent, then recurse*)
			let rec play_game_helper state move_num =
				if ((move_num = cNUM_MOVES) || (is_game_over state.board)) then (calculate_score state; )  else
				(
				let state = update_board state (bot1 state P1) in
				let state = update_board state (bot2 state P2) in
				play_game_helper state (move_num + 1)) in
			let score = play_game_helper initial_state 0 in
			score in
		
	let rec better_beam_search candidates iteration =
		if (iteration = cNUM_ITERATIONS) 
		then 
			candidates 
		else (
			print_endline ("\n\nStarting iteration " ^ (string_of_int iteration) ^ "...");
			let start_time = Unix.time() in
			print_endline ("Inital pool size: " ^ (string_of_int (List.length candidates)));
			let pool = generate_successors (candidates) iteration in
			let pool = List.map (fun (w, s) -> (w, Some 0.)) pool in
			let shuffled_pool = shuffle_list pool in
			print_endline ("Scoring " ^ (string_of_int (List.length pool)) ^ " nodes");
			(* let scored_pool = List.map2 (fun (w1, s1) (w2, s2) -> (w1, Some (better_play_game (w1, s1) (w2, s2)))) pool shuffled_pool in *)
			let scored_pool = List.map (fun (w, s) -> (w, Some (List.fold_left (fun acc node2 -> acc +. (better_play_game (w, s) node2)) 0. pool))) pool in
			let nodes = harvest_nodes scored_pool in 
			print_endline ("Harvested " ^ (string_of_int (List.length nodes)) ^ " nodes");
			let time_took = Unix.time() -. start_time in
			print_round nodes iteration time_took;
			better_beam_search nodes (iteration + 1)
		) in
			
	let starting_weights = [
									[(12., 0.8); (0.25, 1.); (5., 1.); (1., 1.); (2., 1.)]] in
	let starting_nodes = List.map (fun x -> (x, None)) starting_weights in
	let start_time = Unix.time() in
	let _ = better_beam_search starting_nodes 0 in
	let time_took = Unix.time() -. start_time in
	print_endline ("Completed! Took " ^ (string_of_float time_took) ^ " seconds to complete " ^ (string_of_int cNUM_ITERATIONS) ^ " iterations");;

find_weights()



	


	
	
	
