open Board
open State

(* Evaluation based on how many turns it takes for bot 
 * to complete the game if the opponent doesn't move *)
val evaluate1 : (state -> space -> move) -> space -> state -> float
