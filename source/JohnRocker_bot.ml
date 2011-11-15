open Board
open State
open Queue

type node = board*move_set

let q = Queue.create();;
let continuation = ref false;;
let move_sequence = ref [];;

let rec generate_move_sequence player =
	
	let (board,sequence) = Queue.pop q in
	
	if (has_won board player) then (match sequence with 
		| [] -> failwith "well we certainly fucked it up..."
		| h::t -> move_sequence := t; continuation := true; h) 
	else (
		let move_set = available_moves board player in
		
		let do_and_add move = 
			let new_board = do_move board move in
			let new_sequence = sequence@[move] in
			Queue.add (new_board,new_sequence) q; in
			
		List.iter do_and_add move_set;
		generate_move_sequence player
		)

(* Below here is done... BITCH! *)
let rocker_bot state player = 
	if !continuation then (match !move_sequence with 
		| [] -> failwith "wtf??"
		| h::t -> move_sequence := t; h )
	else 
		(Queue.push (state.board, []) q;
	  generate_move_sequence player )
  
