open Board
open State

val sixminutebeam : state -> space -> move
val build_sixminuteab_bot : (state -> space -> move) -> (state -> space -> move) -> (state -> space -> move) -> (state -> space -> move) -> (state -> space -> move) -> state -> space -> move
