open Print
open Board
open State
open Baby_bot
open Stack

type node = board*move_set

let q = Stack.create();;
let continuation = ref false;;
let move_sequence = ref [];;
let cMAX_DEPTH = 4;;
let depth = ref 1;;
let nodes_examined = ref 0;;

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

let rec ids player initial = 
	if !depth > cMAX_DEPTH then (depth:= 1; baby_bot initial player) else (
	let rec ids_helper() =
		if Stack.is_empty q then [] else (
			let (board,sequence) = Stack.pop q in
			nodes_examined:= !nodes_examined + 1;			
			if (has_won board player) then sequence
			else (
				let move_set = available_moves board player in
				let move_set = List.filter (fun (x,y,x',y',_,_) -> (player = P1 && y' <= y) || (player = P2 && y' >= y)) move_set in
				let do_and_add move = 
					let new_board = do_move board move in
					let new_sequence = sequence@[move] in
					if List.length new_sequence <= !depth then
					(Stack.push (new_board,new_sequence) q;) else (); in
					
				List.iter do_and_add move_set;
				ids_helper()
				)) in
				
		let path = ids_helper() in
		match path with
			| [] ->  (prerr_endline ("Didn't find it at depth " ^ (string_of_int !depth))); incr depth; Stack.push (initial.board, []) q; ids player initial;
			| x::t -> (prerr_endline ("Found it at depth " ^ (string_of_int !depth))); (continuation:= true); (move_sequence:= t); x);;

(* Follows the move sequence and returns false if one of the moves*)
(* is invalid or if we have not won at the end. True if all moves *)
(* are valid and we won at the end *)
let check_sequence state player sequence = 
	 let all_moves_valid = ref true in
	
	let check_validity move state =
		let (x1, y1, x2, y2, _, _) = move in
		let move_set = available_moves state.board player in
		prerr_endline "Move to check: ";
		print_movelist [move];
		prerr_endline "Available moves: ";
		print_movelist move_set;
		let rec find_move moves = 
			match moves with
				| [] -> prerr_endline "Not a valid move"; false
				| (x1', y1', x2', y2', _, _)::t -> 					
					if (x1 = x1' && y1 = y1' && x2 = x2' && y2 = y2') then true else (find_move t) in
		find_move move_set in
	let rec do_moves state moves = 
		match moves with
			| [] -> state
			| move::t -> (if (check_validity move state) then (do_moves (update_board state move) t) else (all_moves_valid:=false; state)) in
	let end_state = do_moves state sequence in
	(if not (has_won end_state.board player) then (prerr_endline "Didn't win at the end"));
	(!all_moves_valid && (has_won end_state.board player)) 

(* Below here is done... BITCH! *)
let rocker_bot state player = 
	if (!continuation && (check_sequence state player !move_sequence)) 
		then 
			(match !move_sequence with 
				| [] -> failwith "wtf??"
				| h::t -> move_sequence := t; h )
		else 
			(Stack.push (state.board, []) q;
			let start_time = Unix.time() in
			prerr_endline "Warming him up...";
		  let move = ids player state in
			let time_took = Unix.time() -. start_time in
			prerr_endline ("Search took " ^ (string_of_float time_took) ^ " seconds");
			move)
		

  
