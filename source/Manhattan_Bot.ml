open Board
open State

(* Returns a move based on one heuristic: Make the move such that the*)
(*collective manhattan distance of all of our pieces from the opposite point*)
(*of the board is minimized*)

(*TODO: change this from baby_bot, which is what it is now*)
let manhattan_bot state player = 
	Random.self_init ();
  let move_set = available_moves state.board player in

	let manhattan (x2,y2) sum (x1,y1) =
		sum + abs(x1-x2) + abs (y1-y2) in

	let dist f board =
		let pieces = build_piece_list board player 0 0 [] in 
		let target = if player = P1 then (12,0) else (12,16)in
		List.fold_left (f target) 0 pieces  in
		
	let improve board move =
			let resulting_board = do_move board move in
			let original = (dist manhattan board) in
			original - (dist manhattan resulting_board) in
	
	let max_improve board move_set = 
		List.fold_left (fun best x -> (max (improve board x) best)) (-20) move_set in
		
	let best_list = 
		List.filter (fun x -> improve state.board x >= max_improve state.board move_set) move_set in
		
					
	prerr_endline "Constructed moves list";	
  print_movelist best_list;
    match best_list with
    [] -> failwith "no moves"
		| _ -> List.nth best_list (Random.int (List.length best_list))
		
	
				


