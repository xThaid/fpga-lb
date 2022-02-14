open Hardcaml

val crc32 : (module Comb.S with type t = 'a) -> 'a -> 'a -> 'a

val one_complement_sum : (module Comb.S with type t = 'a) -> 'a -> 'a
val one_complement_sum_pipeline : Signal.register -> Signal.t -> Signal.t -> (Signal.t, Signal.t) With_valid.t2
