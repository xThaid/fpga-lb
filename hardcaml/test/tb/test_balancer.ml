open Base
open Hardcaml
open Lb_dataplane
open Lb_dataplane_test_lib
open Sim_elements

module BalancerSim = struct
  module I = Balancer.I
  module O = Balancer.O

  let create_fn (scope : Scope.t) (i : Signal.t I.t) : (Signal.t O.t) =
    let o = O.Of_signal.wires () in
    Balancer.create_from_if scope i o;
    o

end

let%expect_test "balancer" =
  let module Sim = Sim.Sim(BalancerSim) in
  let module Emitter = FlowWithHeaderEmitter(Common.IPv4Header) in
  let module Consumer = FlowWithHeaderConsumer(Common.IPv4Header) in
  let module BusHost = BusHost(Balancer.BusAgent) in
  
  let sim = Sim.create ~name:"balancer" ~gtkwave:false ~trace:false () in

  let inputs = Sim.inputs sim in
  let outputs = Sim.outputs sim in

  let bus = BusHost.create outputs.bus inputs.bus in
  let emitter = Emitter.create inputs.ip_rx outputs.ip_rx in
  let consumer = Consumer.create outputs.ip_tx inputs.ip_tx in

  Sim.add_element sim (module BusHost) bus;
  Sim.add_element sim (module Emitter) emitter;
  Sim.add_element sim (module Consumer) consumer;

  let udp_pkt src_ip dst_ip src_port dst_port payload_len = 
    let payload = (List.init payload_len ~f:(fun i -> Char.of_int_exn i)) in
    let udp = Packet.create_udp_hdr src_port dst_port (8 + payload_len) in
    let ip = Packet.create_ipv4_hdr (20 + 8 + payload_len) "11" src_ip dst_ip in
  
    Emitter.add_transfer emitter ip (Bytes.of_char_list (Packet.serialize_udp_hdr udp @ payload))
  in

  let write_vip_map ip idx =
    BusHost.schedule_write bus 0 (Constant.to_int (Constant.of_hex_string ~signedness:Unsigned ~width:32 ip));
    BusHost.schedule_write bus 1 idx
  in

  let write_hash_ring vip_idx slot real_idx = 
    BusHost.schedule_write bus 3 real_idx;
    BusHost.schedule_write bus 2 ((vip_idx * Balancer.Consts.ring_size) + slot);
  in

  let write_real_info real_idx real_ip =
    BusHost.schedule_write bus 5 (Constant.to_int (Constant.of_hex_string ~signedness:Unsigned ~width:32 real_ip));
    BusHost.schedule_write bus 4 real_idx;
  in

  Sim.cycle_n sim 1;

  write_vip_map "12121212" 2;
  write_vip_map "14141414" 4;
  write_vip_map "16161616" 5;

  for i = 0 to 3 do
    write_hash_ring 5 i (i + 20);
    write_hash_ring 5 (i + 4) (i + 20)
  done;

  write_real_info 0 "efefefef";

  write_real_info 20 "beef0001";
  write_real_info 21 "beef0002";
  write_real_info 22 "beef0003";
  write_real_info 23 "beef0004";

  Sim.cycle_n sim 40;

  udp_pkt "01010101" "11111111" "aaaa" "b0b0" 30;
  udp_pkt "02020202" "12121212" "bbbb" "d0d0" 15;
  udp_pkt "03030303" "13131313" "cccc" "c0c0" 32;
  udp_pkt "04040404" "14141414" "dddd" "e0e0" 14;
  udp_pkt "05050505" "15151515" "eeee" "d0d0" 13;

  udp_pkt "06060606" "16161616" "ffff" "f0f0" 41;
  udp_pkt "06060606" "16161616" "ffff" "f0f0" 41;
  udp_pkt "06060606" "16161616" "ffff" "f0f0" 41;
  udp_pkt "06060607" "16161616" "ffff" "f0f0" 12;
  udp_pkt "06060606" "16161616" "fff0" "f0f0" 12;
  udp_pkt "06060606" "16161616" "ffff" "f0fb" 12;
  udp_pkt "abababab" "16161616" "bebe" "efef" 12;

  emitter.enabled <- true;
  consumer.enabled <- true;

  Sim.cycle_n sim 15;
  consumer.enabled <- false;
  Sim.cycle_n sim 3;
  consumer.enabled <- true;
  Sim.cycle_n sim 5;
  consumer.enabled <- true;
  Sim.cycle_n sim 5;
  consumer.enabled <- false;
  Sim.cycle_n sim 2;
  emitter.enabled <- false;
  consumer.enabled <- true;
  Sim.cycle_n sim 1;
  emitter.enabled <- true;
  Sim.cycle_n sim 3;
  consumer.enabled <- false;
  Sim.cycle_n sim 5;
  consumer.enabled <- true;
  emitter.enabled <- false;
  Sim.cycle_n sim 5;
  consumer.enabled <- true;
  emitter.enabled <- true;

  Sim.cycle_n sim 400;

  let read_stats real_idx = 
    BusHost.schedule_read bus (10 + real_idx * 4);
    BusHost.schedule_read bus (10 + real_idx * 4 + 1);
    BusHost.schedule_read bus (10 + real_idx * 4 + 2);

    Sim.cycle_n sim 5;
    
    let pkt_cnt = Linked_queue.dequeue_exn bus.responses in
    let bytes_cnt_lo = Linked_queue.dequeue_exn bus.responses in
    let bytes_cnt_hi = Linked_queue.dequeue_exn bus.responses in

    let bytes_cnt = (Int.shift_left bytes_cnt_hi 32) + bytes_cnt_lo in

    Stdio.print_s [%message (pkt_cnt : int) (bytes_cnt : int)]
  in

  for i = 0 to Balancer.Consts.max_reals - 1 do read_stats i done;

  Consumer.expect_transfers consumer;

  [%expect {|
    ((pkt_cnt 2) (bytes_cnt 125))
    ((pkt_cnt 0) (bytes_cnt 0))
    ((pkt_cnt 0) (bytes_cnt 0))
    ((pkt_cnt 0) (bytes_cnt 0))
    ((pkt_cnt 0) (bytes_cnt 0))
    ((pkt_cnt 0) (bytes_cnt 0))
    ((pkt_cnt 0) (bytes_cnt 0))
    ((pkt_cnt 0) (bytes_cnt 0))
    ((pkt_cnt 0) (bytes_cnt 0))
    ((pkt_cnt 0) (bytes_cnt 0))
    ((pkt_cnt 0) (bytes_cnt 0))
    ((pkt_cnt 0) (bytes_cnt 0))
    ((pkt_cnt 0) (bytes_cnt 0))
    ((pkt_cnt 0) (bytes_cnt 0))
    ((pkt_cnt 0) (bytes_cnt 0))
    ((pkt_cnt 0) (bytes_cnt 0))
    ((pkt_cnt 0) (bytes_cnt 0))
    ((pkt_cnt 0) (bytes_cnt 0))
    ((pkt_cnt 0) (bytes_cnt 0))
    ((pkt_cnt 0) (bytes_cnt 0))
    ((pkt_cnt 2) (bytes_cnt 120))
    ((pkt_cnt 1) (bytes_cnt 60))
    ((pkt_cnt 3) (bytes_cnt 267))
    ((pkt_cnt 1) (bytes_cnt 60))
    ((pkt_cnt 0) (bytes_cnt 0))
    ((pkt_cnt 0) (bytes_cnt 0))
    ((pkt_cnt 0) (bytes_cnt 0))
    ((pkt_cnt 0) (bytes_cnt 0))
    ((pkt_cnt 0) (bytes_cnt 0))
    ((pkt_cnt 0) (bytes_cnt 0))
    ((pkt_cnt 0) (bytes_cnt 0))
    ((pkt_cnt 0) (bytes_cnt 0))
    (consumed
     (((version 4) (ihl 5) (dscp 00) (ecn 0) (total_length 003f)
       (identification 0000) (flags 0) (fragment_offset 0000) (ttl 64)
       (protocol 04) (hdr_checksum 0000) (src_ip ac100001) (dst_ip efefefef))
      ((version 4) (ihl 5) (dscp 00) (ecn 0) (total_length 003e)
       (identification 0000) (flags 0) (fragment_offset 0000) (ttl 64)
       (protocol 04) (hdr_checksum 0000) (src_ip ac100001) (dst_ip efefefef))
      ((version 4) (ihl 5) (dscp 00) (ecn 0) (total_length 0059)
       (identification 0000) (flags 0) (fragment_offset 0000) (ttl 64)
       (protocol 04) (hdr_checksum 0000) (src_ip ac100001) (dst_ip beef0003))
      ((version 4) (ihl 5) (dscp 00) (ecn 0) (total_length 0059)
       (identification 0000) (flags 0) (fragment_offset 0000) (ttl 64)
       (protocol 04) (hdr_checksum 0000) (src_ip ac100001) (dst_ip beef0003))
      ((version 4) (ihl 5) (dscp 00) (ecn 0) (total_length 0059)
       (identification 0000) (flags 0) (fragment_offset 0000) (ttl 64)
       (protocol 04) (hdr_checksum 0000) (src_ip ac100001) (dst_ip beef0003))
      ((version 4) (ihl 5) (dscp 00) (ecn 0) (total_length 003c)
       (identification 0000) (flags 0) (fragment_offset 0000) (ttl 64)
       (protocol 04) (hdr_checksum 0000) (src_ip ac100001) (dst_ip beef0001))
      ((version 4) (ihl 5) (dscp 00) (ecn 0) (total_length 003c)
       (identification 0000) (flags 0) (fragment_offset 0000) (ttl 64)
       (protocol 04) (hdr_checksum 0000) (src_ip ac100001) (dst_ip beef0002))
      ((version 4) (ihl 5) (dscp 00) (ecn 0) (total_length 003c)
       (identification 0000) (flags 0) (fragment_offset 0000) (ttl 64)
       (protocol 04) (hdr_checksum 0000) (src_ip ac100001) (dst_ip beef0004))
      ((version 4) (ihl 5) (dscp 00) (ecn 0) (total_length 003c)
       (identification 0000) (flags 0) (fragment_offset 0000) (ttl 64)
       (protocol 04) (hdr_checksum 0000) (src_ip ac100001) (dst_ip beef0001))))
    4500002b 00000000 ff11939a 02020202
    12121212 bbbbd0d0 00170000 00010203
    04050607 08090a0b 0c0d0e

    4500002a 00000000 ff118b93 04040404
    14141414 dddde0e0 00160000 00010203
    04050607 08090a0b 0c0d

    45000045 00000000 ff118370 06060606
    16161616 fffff0f0 00310000 00010203
    04050607 08090a0b 0c0d0e0f 10111213
    14151617 18191a1b 1c1d1e1f 20212223
    24252627 28

    45000045 00000000 ff118370 06060606
    16161616 fffff0f0 00310000 00010203
    04050607 08090a0b 0c0d0e0f 10111213
    14151617 18191a1b 1c1d1e1f 20212223
    24252627 28

    45000045 00000000 ff118370 06060606
    16161616 fffff0f0 00310000 00010203
    04050607 08090a0b 0c0d0e0f 10111213
    14151617 18191a1b 1c1d1e1f 20212223
    24252627 28

    45000028 00000000 ff11838c 06060607
    16161616 fffff0f0 00140000 00010203
    04050607 08090a0b

    45000028 00000000 ff11838d 06060606
    16161616 fff0f0f0 00140000 00010203
    04050607 08090a0b

    45000028 00000000 ff11838d 06060606
    16161616 fffff0fb 00140000 00010203
    04050607 08090a0b

    45000028 00000000 ff113842 abababab
    16161616 bebeefef 00140000 00010203
    04050607 08090a0b |}]
