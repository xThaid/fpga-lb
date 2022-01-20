open Base
open Hardcaml
open Lb_dataplane
open Expect_test_helpers_base

let test_crc32 data_in state_in = 
  let data_in_bits = Bits.of_int ~width:32 data_in in
  let state_in_bits = Bits.of_int ~width:32 state_in in
  let result = Hashes.crc32 (module Bits) state_in_bits data_in_bits |> Bits.to_int in
  print_s [%message (result : int)]

let%expect_test "crc32" =
  test_crc32 0xffeeff00 0xaabbccdd;
  test_crc32 0x0 0xffffffff;
  test_crc32 0xffffffff 0xffffffff;
  test_crc32 0x12345678 0xffffffff;
  test_crc32 0x12345678 0x12345678;
  test_crc32 0xffeeff00 0xff000fff;
  test_crc32 0x0 0x0;
  [%expect {|
    (result 786314983)
    (result 3736805603)
    (result 0)
    (result 1351776301)
    (result 0)
    (result 2872879939)
    (result 0) |}]

