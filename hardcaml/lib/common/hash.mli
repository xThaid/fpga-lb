open Hardcaml

val crc32 : (module Comb.S with type t = 'a) -> 'a -> 'a -> 'a
