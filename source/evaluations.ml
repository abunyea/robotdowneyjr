open Board
open State

(* Evaluation based on how many turns it takes for bot 
 * to complete the game if the opponent doesn't move *)
let evaluate1 bot player state = 
  let rec eval s score = 
    if has_won s.board (space_of_player player) then score else 
       eval (update_board s (bot s player)) (score +. 1.0) in
  eval state 0.0
