open Hardcaml

val crc32 : (module Comb.S with type t = 'a) -> 'a -> 'a -> 'a

(** Calculates ones' complement sum of all 16-bit words in the given signal *)
val one_complement_sum : (module Comb.S with type t = 'a) -> 'a -> 'a
val one_complement_sum_pipeline : Signal.register -> data:Signal.t -> enable:Signal.t -> (Signal.t, Signal.t) With_valid.t2
