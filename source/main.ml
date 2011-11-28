open Board;;
open State;;
open Evaluations;;
open Random_bot;;
open Manhattan_Bot;;
open Baby_bot;;
open Test_bot;;
open Sixminuteab_bot;;
open Minimax_bot;;



let total_moves = ref 0.0;;
let total_time = ref 0.0;; 
let current_time = ref 0.0;;
let count_moves = evaluate2 baby_bot;;

let count_moves_left state = 
	let moves_left = count_moves state.player state in
	prerr_endline ("After " ^ (string_of_float !total_moves) ^ " moves, baby bot would need " ^ (string_of_float moves_left) ^ " more moves to finish");;

let start_timer() = 
	current_time:= Unix.time();;
	
let end_timer() =
	total_moves:= !total_moves +. 1.;
	total_time:= !total_time +. (Unix.time() -. !current_time);
	();;

let print_stats won code num_moves = 
	let win_or_lose = if won then "\nWon by " else "\nLost by " in
	prerr_endline ("Game took " ^ (string_of_float !total_moves));
	let how_many = string_of_float num_moves in
	let str = "Game over with code " ^ (string_of_int code) ^ win_or_lose ^ how_many ^ " moves.\nMoves took an average of " ^ (string_of_float (!total_time /. !total_moves)) ^ " seconds" in
	prerr_endline str;;
	
let run () = 
  let (first_player, initial_state) = read_initial_input () in
  print_board initial_state.board;
  let rec run_helper player current =
    if current.player = player then (
	  (* Our turn *)
			 start_timer();
      (* do and output the current move *)
      let move = sixminuteab_bot current player in
        print_endline (string_of_move move);
      (* get move status *)
      let status = int_of_string (read_line ()) in
			end_timer();
		(* (if ((int_of_float !total_moves) mod 5) = 0 then count_moves_left current); *)
        (match status with
         | 0 -> run_helper (toggle_player player) 
                           (update_board current move)
				 | 1 -> let num_moves = count_moves (toggle_player current.player) current in (print_stats true status num_moves)
				 | _ -> let num_moves = count_moves current.player current in (print_stats false status num_moves)
       )) 
    else (
	  (* Their turn *)
      (* Read their move *)
      let next = read_opponent_input current in
        (match next.status with
         | 0 -> run_helper (toggle_player player) next
				 | 1 -> let num_moves = count_moves (toggle_player current.player) current in (print_stats true next.status num_moves)
				 | _ -> let num_moves = count_moves current.player current in (print_stats false next.status num_moves)
       )) in
  run_helper first_player initial_state in

run();;
