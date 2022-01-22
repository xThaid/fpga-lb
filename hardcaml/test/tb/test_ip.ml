open Base
open Hardcaml
open Lb_dataplane
open Expect_test_helpers_base

let%expect_test "header_checksum" =
  let test_ipv4_checksum hdr =
    let result = Ip.calc_checksum (module Bits) hdr |> Bits.to_constant |> Constant.to_hex_string ~signedness:Unsigned in
    print_s [%message (result : string)]
  in

  test_ipv4_checksum (Common.IPv4Header.Of_bits.unpack ~rev:true (Bits.of_hex ~width:160 "45000073000040004011b861c0a80001c0a800c7"));
  test_ipv4_checksum (Common.IPv4Header.Of_bits.unpack ~rev:true (Bits.of_hex ~width:160 "450000730000400040110000c0a80001c0a800c7"));

  test_ipv4_checksum (Common.IPv4Header.Of_bits.unpack ~rev:true (Bits.of_hex ~width:160 "4500003c1c4640004006b1e6ac100a63ac100a0c"));
  test_ipv4_checksum (Common.IPv4Header.Of_bits.unpack ~rev:true (Bits.of_hex ~width:160 "4500003c1c46400040060000ac100a63ac100a0c"));

  [%expect {|
    (result 0000)
    (result b861)
    (result 0000)
    (result b1e6) |}]

