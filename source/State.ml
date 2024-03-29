open Board

type state = { player : space;
               status : int;
               time1 : int;
               time2 : int;
               board : board;
               grey_remain_1 : int;
               grey_remain_2 : int; }

let rec parse_line line : space array =
	let row = Array.make 25 Void in
	let rec run_through index = 
		if index == 25 then row else
		((row.(index) <- (char_to_space line.[index])); 
          run_through (index + 1)) in
	run_through 0

let copy_state state = 
	let board' = copy_board state.board in
	  { player=state.player;
    status=state.status;
    time1=state.time1;
    time2=state.time2;
    board=board';
    grey_remain_1=state.grey_remain_1;
    grey_remain_2=state.grey_remain_2 }   
	
let read_initial_input () = 
  let rec run_through_board board index = 
	if index = 17 then board else
	  let line = read_line () in
		((board.(index) <- (parse_line line)); 
          run_through_board board (index + 1)) in
  let board = run_through_board (Array.make 17 (Array.make 25 Void)) 0 in
  let numbers = List.map int_of_string 
                 (Str.split (Str.regexp_string " ") (read_line ())) in
  let player = player_of_int (int_of_string (read_line ())) in
    match numbers with 
    | a::b::c::d::e::[] -> ((player_of_int e), 
                           { player=player; 
                             status=0; 
                             time1=a; 
                             time2=b; 
                             board=board; 
                             grey_remain_1=c; 
                             grey_remain_2=d; } )
    | _ -> failwith "Bad input on read_initial_input"


let update_board_with_time state time1 time2 move =
	let (x1, y1, _, _, x3, y3) = move in
	let (grey1, grey2) = 
		(if x3 <> -1 && y3 <> -1 then 
			(if state.board.(y1).(x1) = P1 then 
				(state.grey_remain_1 - 1, state.grey_remain_2) else
				(state.grey_remain_1, state.grey_remain_2 - 1)) else
			(state.grey_remain_1, state.grey_remain_2)) in
  { player=state.player;
    status=state.status;
    time1=time1;
    time2=time2;
    board=do_move state.board move;
    grey_remain_1=grey1;
    grey_remain_2=grey2 }   
		
				
let update_board state move =
	update_board_with_time state state.time1 state.time2 move    
		
let read_opponent_input previous = 
  let line = read_line () in
  let numbers = List.map int_of_string (Str.split (Str.regexp_string " ") line) in
    match numbers with 
		  | a::b::c::d::e::f::g::h::i::[] -> update_board_with_time previous b c (e, d, g, f, i, h)
      | _ -> failwith "Bad input to read_opponent_input"  



  
 
