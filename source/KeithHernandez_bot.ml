open Board
open State

open Print
open Board
open State
open Mustache
open Stack

type node = board*move_set

let q = Stack.create();;
let continuation = ref false;;
let move_sequence = ref [];;
let cDEPTH = ref 4;;
let depth = ref 1;;
let nodes_examined = ref 0;;
let evaluate = evaluate_one;;

(*
let rec generate_move_sequence player =
	
	let (board,sequence) = Queue.pop q in
	nodes_examined:= !nodes_examined + 1;
	let current_depth = List.length sequence in
	(if current_depth > !depth then ((depth:= current_depth); 
	(prerr_endline ((string_of_int current_depth) ^ "\nNodes examined: " ^ (string_of_int !nodes_examined))))); 
	if current_depth = 10 then failwith "fuck this shit";
	
	
	if (has_won board player) then (match sequence with 
		| [] -> failwith "well we certainly fucked it up..."
		| h::t -> move_sequence := t; continuation := true; h) 
	else (
		let move_set = available_moves board player in
		let move_set = List.filter (fun (x,y,x',y',_,_) -> (player = P1 && y' <= y) || (player = P2 && y' >= y)) move_set in
		
		let do_and_add move = 
			let new_board = do_move board move in
			let new_sequence = sequence@[move] in
			Queue.add (new_board,new_sequence) q; in
			
		List.iter do_and_add move_set;
		generate_move_sequence player
		)
*)

let ids player state = 
	let (start, comp) = if player = P1 then (100000000., (>=)) else (-10000000000., (<=)) in
	let best_so_far = ref (state, start, []) in
	let score_node (state, sequence) = 
		let (best_state, best_score, best_seq) = !best_so_far in
		let score = evaluate player state in
		if comp best_score score then 
			(best_so_far:= (state, score, sequence)) else () in
	let rec ids_helper() =
		if Stack.is_empty q then () else (
			let (state ,sequence) = Stack.pop q in
			if (List.length sequence) = !cDEPTH then (score_node (state, sequence); ids_helper())
			else (
				let moves = available_moves state.board player in
				let moves = List.filter (fun (x,y,x',y',_,_) -> (player = P1 && y' <= y) || (player = P2 && y' >= y)) moves in
				let do_and_add move = 
					let state' : state = update_board state move in
					let new_sequence = sequence@[move] in
					(Stack.push (state',new_sequence) q;) in
					
				List.iter do_and_add moves;
				ids_helper()
				)) in
				
		ids_helper();
		!best_so_far

(* Follows the move sequence and returns false if one of the moves*)
(* is invalid or if we have not won at the end. True if all moves *)
(* are valid and we won at the end *)
let check_sequence state player sequence = 
	 if List.length sequence = 0 then false else (
		
		let check_validity move state =
			let (x1, y1, x2, y2, _, _) = move in
			let move_set = available_moves state.board player in
			let rec find_move moves = 
				match moves with
					| [] -> prerr_endline "Not a valid move"; false
					| (x1', y1', x2', y2', _, _)::t -> 					
						if (x1 = x1' && y1 = y1' && x2 = x2' && y2 = y2') then true else (find_move t) in
			find_move move_set in
		let rec do_moves state moves = 
			match moves with
				| [] -> true;
				| move::t -> (if (check_validity move state) then (do_moves (update_board state move) t) else false) in
		let valid = do_moves state sequence in
		if valid then true else (continuation:= false; false))

(* Below here is done... BITCH! *)
let ids_bot depth state player = 
	cDEPTH:= depth;
	if (!continuation && (check_sequence state player !move_sequence)) 
		then 
			(match !move_sequence with 
				| [] -> failwith "wtf??"
				| h::t -> move_sequence := t; h )
		else 
			(Stack.push (state, []) q;
			let start_time = Unix.time() in
			prerr_endline "Bringing in the mustache...";
		  let (_, _, sequence) = ids player state in
			let time_took = Unix.time() -. start_time in
			prerr_endline ("Hernandezz search took " ^ (string_of_float time_took) ^ " seconds");
			(match sequence with 
				| [] -> failwith "Search return empty list"
				| h::t -> print_movelist sequence; continuation:= true; move_sequence := t; h ))
		

  

				


