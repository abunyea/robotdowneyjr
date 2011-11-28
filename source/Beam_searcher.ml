open Minimax_bot;;
open Baby_bot;;
open Evaluations;;
open State;;
open Board;;
open Unix;;
open Reader;;

type weight_vector = int list;;
type score = int option;;
type node = weight_vector * score;;

Random.self_init();;
let cNUM_ITERATIONS = 2;; 
let cRANDOM_CANDIDATES = 3;;
let cBEST_CANDIDATES = 7;;
let cPOOL_SIZE = cRANDOM_CANDIDATES + cBEST_CANDIDATES;;
let cNUM_MOVES = 35;;
let cNUM_SUCCESSORS = 3;;
let path = "beam" ^ (string_of_int (Random.int 200)) ^ ".txt";;
print_endline ("Output written to: " ^ path);;

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
		| (Some x1, Some x2) -> compare x1 x2
		| _ -> failwith "A node doesn't have a score when we're trying to harvest them";;

let build_minimax features weights = 
	let evaluate player state = 
		let scores = List.map2 (fun f w -> w *. (f player state)) features weights in
		List.fold_left (+.) 0.0 scores in
	build_minimax_bot evaluate 1;;

let print_node (weight_vector, score) =
	List.iter (fun x -> print_str ((string_of_float x) ^ " ")) weight_vector;
	print_line "";
	(match score with
		| None -> print_line "No score yet"
		| Some x -> print_line (string_of_float x));
	print_line "";;

let print_round nodes iteration time_took =
	let it_str = ("Iteration " ^ (string_of_int iteration)) in
	print_endline it_str;
	print_line it_str;
	print_line ("Took: " ^ (string_of_float time_took) ^ " seconds");
	List.iter print_node nodes
	
	
	

let generate_random_weights n = 
	let get_weight() = 
		let weight = Random.float 1. in
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
	evaluate2 baby_bot P1 state;;


(* Checks if all the nodes have a score. *)
let check_if_done nodes = 
	let scored = List.map (fun (w, s) -> match s with | None -> false | _ -> true) nodes in
	not (List.mem false scored)
	
(* Takes the top cBEST_CANDIDATES nodes and then cRANDOM_CANDIDATES random nodes *)
let harvest_nodes nodes = 
	let sorted = List.sort compare_node nodes in
	let shuffle_list lst = 
		List.sort (fun _ _ -> Random.int 3 - 1) lst in
	let take lst n =
		let rec helper taken not_taken num_taken =
			if num_taken = n then taken else (
				match not_taken with
					| [] -> taken
					| x::t -> helper (x::taken) t (num_taken + 1)) in
	let rec take_best (best, rest) num_left= 
		match rest with
			| [] -> (best, rest)
			| x::t -> if num_left = 0 then (best, rest) else (take_best (x::best, t) (num_left - 1)) in
	let (best, rest) = take_best ([], sorted) cBEST_CANDIDATES in
	best @ (take rest cRANDOM_CANDIDATES);;
	
	

(* Generates successors of a given weight_vector*)
(* A successor is just a modified weight_vector, the trick is how are we *)
(* going to generate the successors. Each node takes approximately 3 seconds to *)
(* evaluate, a node's successors will take #successors * 3 seconds to evaluate *)
(* This means each iteration will take cPOOL_SIZE * #successors * 3 seconds to run*)
(* and the whole program will take cNUM_ITERATIONS * cPOOL_SIZE * #successors * 3 seconds to run *)

(* So what we are doing right now is we take each weight and we add a random value between -.05 and .05 to it*)
let generate_successors nodes _ =
	let mutation() = 
		(Random.float 0.1) -. 0.05 in
	let rec generate_one (weights, score) successors =
		if (List.length successors) = cNUM_SUCCESSORS 
			then 
				(weights, score)::successors 
			else (
				let new_weights = List.map (fun w -> w +. mutation()) weights in
				generate_one (weights, score) ((new_weights, None)::successors)
			) in
	let successors = List.map (fun node -> generate_one node []) nodes in
	List.flatten successors
	
(* Find weights takes a list of feature functions [f1; f2; ... fn] *)
(* and returns weights [w1; ... wn] for the best evaluation function*)
(* w1 * f1(board) + w2 * f2(board) ... *)
(* It does this by conducting a beam search, using minimax of depth 1 (equivilent to baby bot) *)
let find_weights (features : (space -> state -> float) list) : (float list) =
	(* Build a blank board *)
	let initial_state = get_initial_state() in
	let candidates = generate_random_weights (List.length features) in

		
	(* Plays cNUM_MOVES with a bot with the specified weights and baby_bot*)
	(* then returns the number of moves that the minimax bot is winning by*)
	(* (negative if it is losing) *)
	let play_game (weights, score) =
			let bot = build_minimax features weights in (*Build our bot *)
			(* If we've played cNUM_MOVEs return a score. Else play a move for us, then opponent, then recurse*)
			let rec play_game_helper state move_num =
				if move_num = cNUM_MOVES then calculate_score state else
				(
				let state = update_board state (bot state P1) in
				let state = update_board state (baby_bot state P2) in
				play_game_helper state (move_num + 1)) in
			match score with
				| Some x -> (weights, Some x)
				| None -> (weights, Some (play_game_helper initial_state 0))
			 in
	let nodes = List.map (fun c -> (c, None)) candidates in
	
	let rec beam_search candidates iteration = 
		if (iteration = cNUM_ITERATIONS) 
		then 
			candidates 
		else (
			(* Want to take cBEST_CANDIDATES, and cRANDOM_CANDIDATES random candidates*)
			(* Then generate successors for all. Score them all, and run beam search again on them *)
			let start_time = Unix.time() in
			let pool = generate_successors (candidates) iteration in
			let scored_pool = List.map play_game pool in
			let nodes = harvest_nodes scored_pool in 
			let time_took = Unix.time() -. start_time in
			print_round nodes iteration time_took;
			beam_search scored_pool (iteration + 1)
		) in
	let solutions = beam_search nodes 0 in
	let sorted = List.sort compare_node solutions in
	fst (List.hd sorted);;		
		


find_weights [num_moves_dif; furthest_back]
	


	
	
	
