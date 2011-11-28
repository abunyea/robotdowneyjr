open Board;;
open State;;

let rec parse line : space array =
	let row = Array.make 25 Void in
	let rec run_through index = 
		if index == 25 then row else
		((row.(index) <- (char_to_space line.[index])); 
          run_through (index + 1)) in
	run_through 0;;

let get_lines() =
	let lines = ref [] in
	let chan = open_in "blank_board.txt" in
	try
	  while true; do
	    lines := input_line chan :: !lines
	  done; []
	with End_of_file ->
	  close_in chan;
	  List.rev !lines;;

let get_initial_state() = 
	let lines = get_lines() in
  let rec run_through_board board index = 
	if index = 17 then board else
	  (let line = List.nth lines index in 
		(board.(index) <- (parse line);
		 run_through_board board (index + 1);
		)) in
  let board = run_through_board (Array.make 17 (Array.make 25 Void)) 0 in
	{player = P1; status = 0; time1=600000; time2=600000; board=board; grey_remain_1=0; grey_remain_2=0};;
  (*
  let numbers = List.map int_of_string 
                 (Str.split (Str.regexp_string " ") (read_line ())) in
  let player = player_of_int (int_of_string (read_line ())) in
    match numbers with 
    | a::b::c::d::_::[] ->
                           { player=player; 
                             status=0; 
                             time1=a; 
                             time2=b; 
                             board=board; 
                             grey_remain_1=c; 
                             grey_remain_2=d; }
    | _ -> failwith "Bad input on read_initial_input";; *)