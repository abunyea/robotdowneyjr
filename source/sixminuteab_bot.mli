open Board
open State

val sixminuteab_bot : state -> space -> move
val sixminutemanhattan_bot : state -> space -> move
val sixminutetest_bot : state -> space -> move
val sixminutebeam_bot : state -> space -> move
val build_sixminuteab_bot : (state -> space -> move) -> (state -> space -> move) -> (state -> space -> move) -> (state -> space -> move) -> state -> space -> move
