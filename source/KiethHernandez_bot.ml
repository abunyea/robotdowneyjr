open Board
open State

(*Calculates the mustache distance *)

let hernandez_bot state player = 
	Random.self_init();
  let move_set = available_moves state.board player in

	let mustache_dist (x2,y2) sum (x1,y1) =
		let v_dist = abs(y2 - y1) in
		let dist = 
			if x - y <= 12 && y - x <= 4 && x + y >= 12 && x + y <= 28 then
				v_dist
			else (if y < 9 then
				v_dist + (if (x > 12) then (x - y - 12) / 2 else (12 - (x+y))/2)
			else
				v_dist + (if (x > 12) then (x + y - 28) / 2 else (y - x - 4)/2)) in
		sum + dist**1.5 in

	let dist f board =
		let pieces = build_piece_list board player 0 0 [] in 
		let target = if player = P1 then (12,0) else (12,16)in
		List.fold_left (f target) 0. pieces  in
		
	let improve board move =
			let resulting_board = do_move board move in
			let original = (dist manhattan board) in
			original -. (dist manhattan resulting_board) in
	
	let max_improve board move_set = 
		List.fold_left (fun best x -> (max (improve board x) best)) (-20.) move_set in
		
	let best_list = 
		List.filter (fun x -> improve state.board x >= max_improve state.board move_set) move_set in
		
					
    match best_list with
    [] -> failwith "no moves"
		| _ -> List.nth best_list (Random.int (List.length best_list))
		
	
				


