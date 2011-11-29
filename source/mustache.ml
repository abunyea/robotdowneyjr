open Board
open State

(* A suite of useful board evaluation functions named after famous mustachioed historical figures *)


let nikos_dist (x, y) (x', y') = 
		let dr = y' - y in
		let ds = (x + y - x'- y')/2 in
		let dist = if dr * ds > 0 then ((abs dr) + (abs ds)) else (max (abs dr) (abs ds)) in
		float_of_int dist
		
(*Famous mustache number 1: Joseph Stalin (18 December 1878[1] – 5 March 1953) *)

(* Stalin was the Premier of the Soviet Union from 6 May 1941 to 5 March 1953. He was among the Bolshevik *)
(* revolutionaries who brought about the October Revolution and had held the position of first General *)
(* Secretary of the Communist Party of the Soviet Union's Central Committee from 1922 until his death in 1953. *)
(* While formally the office of the General Secretary was elective and was not initially regarded as the *)
(* top position in the Soviet state, after Vladimir Lenin's death in 1924, Stalin managed to consolidate *)
(* more and more power in his hands, gradually putting down all opposition groups within the party. This *)
(* included Leon Trotsky, the Red Army organizer, proponent of world revolution, and principal critic of *)
(* Stalin among the early Soviet leaders, who was exiled from the Soviet Union in 1929. Instead, Stalin's *)
(* idea of socialism in one country became the primary line of the Soviet politics. *)

(* Stalin Distance is equivalent to the average number of moves each piece needs to reach the end point *)
(* Smaller is better *)
let stalin_dist degree player pieces =

	let (x2, y2) = if player = P1 then (12,0) else (12,16) in
	
	let stalin degree sum (x, y) =
		let v_dist = abs(y2 - y) in
		let dist = 
			if x - y <= 12 && y - x <= 4 && x + y >= 12 && x + y <= 28 then
				v_dist
			else (if y < 9 then
				v_dist + (if (x > 12) then (x - y - 12) / 2 else (12 - (x+y))/2)
			else
				v_dist + (if (x > 12) then (x + y - 28) / 2 else (y - x - 4)/2)) in
		sum +. (float_of_int dist)**degree in
		
	(List.fold_left (stalin degree) 0. pieces) /.(float_of_int (List.length pieces));;


(*Famous mustache number 2: Hulk Hogan (born August 11, 1953) *)

(*Hogan is an American Semi-retired professional wrestler, actor, television personality, *)
(* and musician currently signed to Total Nonstop Action Wrestling (TNA). Hogan enjoyed mainstream *)
(* popularity in the mid 1980s through the early 1990s as the all-American character "Hulk" Hogan in *)
(* the World Wrestling Federation (WWF—now the WWE), and was notable in the mid-to-late 1990s as Hollywood *)
(* Hogan, the villainous nWo leader, in World Championship Wrestling (WCW). Following WCW's fold, he made a *)
(* brief return to WWE in the early 2000s, revising his heroic character by combining elements of his two most *)
(* famous personas. *)

(*Hogan Distance is equivalent to the average x distance of each piece from the center axis *)
(* Smaller numbers are better *)
let hogan_dist degree player pieces = 
	
	let hogan degree sum (x,_) = 
		let center_dist = abs(12 - x) in
		sum +. (float_of_int center_dist)**degree in
		
	(List.fold_left (hogan degree) 0. pieces)/.(float_of_int (List.length pieces));;
		
		
(*Famous mustache number 3: Salvador Dali (May 11, 1904 – January 23, 1989) *)

(* Dali was a prominent Spanish Catalan surrealist painter born in Figueres,Spain. Dalí was a skilled *)
(* draftsman, best known for the striking and bizarre images in his surrealist work. His painterly skills *)
(* are often attributed to the influence of Renaissance masters.[1][2] His best-known work, The Persistence *)
(* of Memory, was completed in 1931. Dalí's expansive artistic repertoire includes film, sculpture, and *)
(* photography, in collaboration with a range of artists in a variety of media. *)
		
(*Dali distance is equivalent to the average physical distance from all other pieces each piece is*)

let dali_dist degree player pieces = 
	
	let dali degree piece_list sum (x,y)  = 
		let rec tally sum list = match list with 
			| [] -> sum
		(*	| (x2,y2)::t -> tally (sum +. (((float_of_int (abs(x2-x))) +. (float_of_int (abs(y2-y)))) /. 2.)) t in *)
			| (x2,y2)::t -> tally (sum +. (nikos_dist (x,y) (x2,y2))) t in 
		(tally sum piece_list)**degree in
		
	(List.fold_left (dali degree pieces) 0. pieces)/.(float_of_int (List.length pieces));;
		
		
(*Famous mustache number 4: Paul Teutul (born May 1, 1949 in Yonkers, New York) *)

(*Teutal is the founder of Orange County Choppers, a manufacturer of custom motorcycles *)
(* and focus of the reality television series American Chopper. Teutul first appeared on the *)
(* show with his sons Paul Teutul, Jr. and Michael Teutul. *)

(*Teutul distance is equivalent to the average distance gained by an available move *)
(* More is better *)
let teutul_dist degree player pieces = 
	
	let teutul degree sum (x,y,x2,y2,_,_) =  
		(sum +. (nikos_dist (x,y) (x2, y2)))**degree in
		
	(List.fold_left (teutul degree) 0. pieces)/.(float_of_int (List.length pieces));;

(*Overall mustache evaluator that takes in 4 weights and computes a total score *)

let mustache_evaluator w1 d1 w2 d2 w3 d3 w4 d4 player state =
	let pieces = build_piece_list state.board player 0 0 [] in
	let avail_moves = available_moves state.board player in
	let result = (w1 *. (stalin_dist d1 player pieces) +.
								w2 *. (hogan_dist d2 player pieces) +.
								w3 *. (dali_dist d3 player pieces) -.
								w4 *. (teutul_dist d4 player avail_moves)) in
	if player = P1 then result else -1.0 *. result

	
let basic_mustache_evaluator player state = 
	(mustache_evaluator 12. 0.8 1. 1. 0.3 1.0 1. 1. (toggle_player player) state) -.
	(mustache_evaluator 12. 0.8 1. 1. 0.3 1.0 1. 1. player state)
	
let winning_evaluator player state = 
	(mustache_evaluator 12. 0.8 1. 1. 0.3 1. 1. 1. (toggle_player player) state) -.
	(mustache_evaluator 12. 0.8 1. 1. 0.3 1. 1. 1. player state)

let beam_evaluator player state =
	(mustache_evaluator 9.948 1.085 1.417 1.089 0.774 0.799 0.611 0.855 (toggle_player player) state) -.
	(mustache_evaluator 9.948 1.085 1.417 1.089 0.774 0.799 0.611 0.855 player state)
	
	
				


