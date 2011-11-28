open Board
open State

(* Evaluation based on how many turns it takes for bot 
 * to complete the game if the opponent doesn't move *)
let evaluate1 bot player state = 
  let rec eval s score = 
    if has_won s.board player then score else 
       eval (update_board s (bot s player)) (score +. 1.0) in
  eval state 0.0

(* Evaluation exactly the same as evaluate1. However we first set all the opponents pieces to empty*)
(* Rationale for doing that: if the opponent doesn't have at least 5 pieces out of their endzone when this function*)
(* is called, then we can never win in the sense of has_won. So we need to remove at least 5 pieces from their endzone.*)
(* But then we need to decide how many to remove/which ones to remove/where to put them. Now how many moves it takes us *)
(* to finish is highly dependant on where the opponents pieces are, so moving there pieces would greatly affect the *)
(* the accuracy of this, so we just set all their pieces to empty *)

(* Also evaluates 10 times and takes the median since baby bot is a little unreliable *)
let evaluate2 bot player state = 
	let opponent = toggle_player player in
	let opponents_pieces = build_piece_list state.board opponent 0 0 [] in
	let new_board = copy_board state.board in
	let rec build_board pieces = 
		match pieces with
			| [] -> ()
			| (x, y)::t -> new_board.(y).(x) <- Empty; build_board t; in
	build_board opponents_pieces;
	let new_state =   
		{player=state.player;
    status=state.status;
    time1=state.time1;
    time2=state.time2;
    board=new_board;
    grey_remain_1=state.grey_remain_1;
    grey_remain_2=state.grey_remain_2} in
	let rec evaluate lst times = 
		if times = 0 then lst else
			evaluate ((evaluate1 bot player new_state)::lst) (times - 1) in
	let num_times = 5 in
	let scores = evaluate [] num_times in
	let sorted = List.sort compare scores in
	let middle = num_times / 2 in
	List.nth sorted middle
	
		  
		
	