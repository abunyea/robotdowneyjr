open Board;;
open State;;

let calc_manhattan_wo_grey state player =
	let grey_remaining = if player = Player1 then state.grey_remain_1 else state.grey_remain_2 in
	
	