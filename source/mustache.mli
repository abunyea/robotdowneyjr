open Board
open State

val stalin_dist : float -> space -> (int * int) list -> float

val hogan_dist : float -> space -> (int * int) list -> float

val dali_dist : float -> space -> (int * int) list -> float

val teutul_dist : float -> space -> move_set -> float

val furthest_back : space -> (int * int) list -> float

val mustache_evaluator : float -> float -> float -> float -> float -> float -> float -> float -> float -> float -> space -> state -> float

val basic_mustache_evaluator : space -> state -> float
val winning_evaluator : space -> state -> float
val build_evaluater : (float * float * float * float * float * float * float * float) -> (float * float) -> space -> state -> float
val beam_evaluator : space -> state -> float
val evaluate_one : space -> state -> float