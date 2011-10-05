open Board;;
open State;;
open Random_bot;;
open Baby_bot;;

let run () = 
  let (first_player, initial_state) = read_initial_input () in
  print_board initial_state.board;
  let rec run_helper player current =
    if current.player = player then (
	  (* Our turn *)
      prerr_endline "Our turn..";
      (* do and output the current move *)
      let move = baby_bot current player in
        print_endline (string_of_move move);
        prerr_endline "Moved";
      (* get move status *)
      let status = int_of_string (read_line ()) in
        (match status with
         | 0 -> run_helper (toggle_player player) 
                           (update_board current move)
         | _ -> prerr_endline ("Game over with code " ^ (string_of_int status)))
       )
    else (
	  (* Their turn *)
      prerr_endline "Their turn..";
      (* Read their move *)
      let next = read_opponent_input current in
        (match next.status with
         | 0 -> run_helper (toggle_player player) next
         | _ -> prerr_endline ("Game over with code " ^ (string_of_int next.status)))
     ) in
  run_helper first_player initial_state in

run ();;
