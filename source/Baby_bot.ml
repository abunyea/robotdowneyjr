open Board
open State

(* Returns a move based on a simple set of heuristics as follows:*)
(* 1) Perform the move that moves us the most in the forward direction*)
(* 2) If tie for (1) then move the piece that is farthest back*)
(* 3) If tie for (2) then perform the move that puts us closest to the center vertical axis*)
(* 4) If tie for (3) then pick a random move from the remaining list*)

let baby_bot state player = 
	Random.self_init ();
	
  let player = if player = Player1 then P1 else P2 in
  let move_set = available_moves state.board player in
	
	let max_distance (set,best) (x1,y1,x2,y2) = 
		let distance = 
			(if player = P1 then y1-y2 else y2-y1) in
		if distance = best then ((x1,y1,x2,y2)::set,best)
			else if distance > best then ([(x1,y1,x2,y2)],distance)
				else (set,best) in 
				
	let furthest_back (set,back) (x1,y1,x2,y2) = 
		let endzone_distance = 
			(if player = P1 then 16-y1 else y1) in
		if endzone_distance = back then ((x1,y1,x2,y2)::set,back)
			else if endzone_distance < back then ([(x1,y1,x2,y2)],endzone_distance)
				else (set,back) in 		
				
	let most_center (set,best) (x1,y1,x2,y2) = 
		let center_distance = abs(12-x2) in
		if center_distance = best then ((x1,y1,x2,y2)::set,best)
			else if center_distance < best then ([(x1,y1,x2,y2)],center_distance)
				else (set,best) in 
				
	let best_list = List.fold_left max_distance ([],0) move_set in
	
	let best_list = if List.length best_list = 1 then best_list 
		else List.fold_left furthest_back ([],16) move_set in
		
  let best_list = if List.length best_list = 1 then best_list 
		else List.fold_left most_center ([],12) move_set in
		
	prerr_endline "Constructed moves list";		
  print_movelist best_list;
	let len = List.length best_list in
	let index = Random.int len in
	List.nth best_list index