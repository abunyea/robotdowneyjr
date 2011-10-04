open Board

type state = { player : player;
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
   

let read_opponent_input previous = 
  let line = read_line () in
  let numbers = List.map int_of_string (Str.split (Str.regexp_string " ") line) in
    match numbers with 
	  | a::b::c::d::e::f::g::h::i::[] -> 
        { player=previous.player;
          status=a;
          time1=b;
          time2=c;
          board=Board.do_move previous.board (e, d, g, f);
          grey_remain_1=(if (h <> -1 && i <> -1) then 
            previous.grey_remain_1 else
            (if previous.player = Player1 then previous.grey_remain_1 else 
             previous.grey_remain_1 - 1));
          grey_remain_2=if (h <> -1 && i <> -1) then 
            previous.grey_remain_2 else
            (if previous.player = Player2 then previous.grey_remain_2 else 
             previous.grey_remain_2 - 1) }
      | _ -> failwith "Bad input to read_opponent_input"  

let update_board state move =
  { player=state.player;
    status=state.status;
    time1=state.time1;
    time2=state.time2;
    board=do_move state.board move;
    grey_remain_1=state.grey_remain_1;
    grey_remain_2=state.grey_remain_2 }    
  
 
