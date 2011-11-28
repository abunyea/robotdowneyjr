
let cEXCEPTION = -100000.0
(* Taken from RosettaCode.com *)
(* Executes cmd in terminal, and returns the results. *)
let syscall cmd =
  let ic, oc = Unix.open_process cmd in
  let buf = Buffer.create 16 in
  (try
     while true do
       Buffer.add_channel buf ic 1
     done
   with End_of_file -> ());
  let _ = Unix.close_process (ic, oc) in
  (Buffer.contents buf);;

(* Returns how many moves it would've taken baby bot to finish for the given player 
let score player = 
	let file = open_in "server.out" in
	let lines = Std.input_list file in
	
	let get_last num_to_get list' = 
		if List.length list' = num_to_get then list' else
			get_last num_to_get (List.tl list') in
	let last_20 = get_last 20 lines in
	let board_with_hashes = List.rev (get_last 17 (List.rev last_20)) in
	let board_strings = List.map (fun x -> Str.string_after x 1) board_with_hashes in 
	
	let board = Array.make 17 (Array.make 25 Void) in
	let make_new_board y =
		if y = 17 then () else
			(board.(y) <- (Array.make 25 Void));
			(make_new_board (y + 1)); in
	make_new_board 0;
	
	let parse_row y line= 
		List.fold_left (fun x -> board.(y).(x) <- (char_to_space  line.[x]); y + 1) 0 line in
	let parse_cols 
		
		
*)			
	
	
	
(*Plays a game a returns a result of 1.0 if we win, or 0.0 if we lose*)
(* Also returns the number of milliseconds it took to play the game *)
(* Requires that we execute this in the server directory *)
let play_game our_command opp_command =
		try
			let cmd = "java GameServer 4700 7 p1.log p2.log '" ^ our_command ^ "' '" ^ opp_command ^ "' game.log 1>learner.out 2>learner.err" in
			let _ = syscall cmd in
			let output = syscall "cat learner.err" in 
			let lines = Str.split (Str.regexp "\n") output in
			let time_line = List.nth lines 2 in 
			let time_line_list = Str.split (Str.regexp " ") time_line in
			let time_left = int_of_string (List.hd time_line_list) in
			let time_took = 600000 - time_left in
			let winner_str = List.hd (List.rev lines) in
			let winner = if winner_str = "1" then 1.0 else 0.0 in
			(winner, time_took)
		with
			| e -> print_endline "There was an exception this time"; (c_EXCEPTION, -1);;
	
(*
(* Builds an evaluation function that takes the dot products of weights and a bunch of attributes of the boards *)
let build_evaluater (weights : float list) (feature_functions : (board -> float) list) board : float = 
		if Board.has_won board P1 then cWINNING_SCORE else if Board.has_won board P2 then -1.0 *. cWINNING_SCORE else
		List.fold_left2 (fun acc alpha beta -> acc +. alpha *. (beta board)) 0.0 weights feature_functions;;

*)


let get_stats() = 
	let our_command = Sys.argv.(1) in
	let opp_command = Sys.argv.(2) in 
	let out_stream = open_out Sys.argv.(3) in
	let number_times = int_of_string Sys.argv.(4) in
	
	let print_stats winner time_took = 
		let output_str = ((string_of_float winner) ^ "\t" ^ (string_of_int time_took) ^ "\n") in
				output_string out_stream output_str in
				
	let rec helper times_left wins_so_far total_time =
		if times_left = 0 then (wins_so_far, total_time) else
			let (winner, time_took) = play_game our_command opp_command in
			(print_endline ("Trial " ^ (string_of_int (number_times - times_left)) ^ "\nWinner:" ^ (string_of_float winner)));
			if winner = cEXCEPTION then (helper times_left wins_so_far total_time) else
			(print_stats winner time_took;
			if winner = 1.0 then (helper (times_left - 1) (wins_so_far + 1) (total_time + time_took)) else
				(helper (times_left - 1) (wins_so_far) (total_time + time_took))) in
				
	let (wins, time_took) = helper number_times 0 0 in
	let avg = (float_of_int time_took) /. (float_of_int number_times) in 
	let final_string = "\n\nPlayed: " ^ (string_of_int number_times) ^ " games\nTook an average of " ^ (string_of_float avg) ^ " milliseconds\nNumber won: " ^ (string_of_int wins) ^ "\n\n" in
	output_string out_stream final_string;;


get_stats()
			
let learn_weights feature_functions =
	();;
