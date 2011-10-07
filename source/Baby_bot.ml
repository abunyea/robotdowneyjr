open Board
open State
type comparator = GT | LT| GTE | LTE
(* Returns a move based on a simple set of heuristics as follows:*)
(* 1) Perform the move that moves us the most in the forward direction*)
(* 2) If tie for (1) then move the piece that is farthest back*)
(* 3) If tie for (2) then perform the move that puts us closest to the center vertical axis*)
(* 4) If tie for (3) then pick a random move from the remaining list*)

let baby_bot state player = 
	Random.self_init ();
  let player = if player = Player1 then P1 else P2 in
  let move_set = available_moves state.board player in
	
	let vert_dist player (x1,y1,x2,y2) = 
		if player = P1 then y1-y2 else y2-y1 in
		
	let endzone_dist player (x1,y1,x2,y2) = 
		if player = P1 then 16-y1 else y1 in
		
	let center_dist player (x1,y1,x2,y2) = abs(12-x2) in
		
	let optimize f c (set,best) move =
		let result =  f player move in
			if result = best then (move::set,best) 
				else if (result > best && c = GT) || (result < best && c = LT) 
					then ([move],result)
				else (set,best) in
				
	let best_list = 
		fst (List.fold_left (optimize center_dist LT) ([],12) (
		fst (List.fold_left (optimize endzone_dist LT) ([],16) (
		fst (List.fold_left (optimize vert_dist GT) ([],0) move_set))))) in
			
	prerr_endline "Constructed moves list";	
  print_movelist best_list;
    match best_list with
    [] -> failwith "no moves"
		| _ -> List.nth best_list (Random.int (List.length best_list))
