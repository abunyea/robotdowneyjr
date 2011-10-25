open Board
open State
open Manhattan_Bot

let middle = ref false
let ending = ref false

let sixminuteab_bot state player =
  if not !middle then
	(* manhattan *)
    manhattan_bot state player
  else if not !ending then
    (* minimax *) failwith "no"
  else
    (* lookup table or search *) failwith "no"
