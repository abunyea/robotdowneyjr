open Board
open State

(* Returns a random move out of the move_set*)
let random_bot state player = 
    Random.self_init ();
    let player = if player = Player1 then P1 else P2 in
    let move_set = available_moves state.board player in
    prerr_endline "Constructed moves list";
	let len = List.length move_set in
	let index = Random.int len in
	List.nth move_set index
