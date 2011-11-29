open Board
open State

val stalin_dist : float -> space -> (int * int) list -> float

val hogan_dist : float -> space -> (int * int) list -> float

val dali_dist : float -> space -> (int * int) list -> float

val teutul_dist : float -> space -> move_set -> float

val mustache_evaluator : float -> float -> float -> float -> float -> float -> float -> float -> space -> state -> float

val basic_mustache_evaluator : space -> state -> float