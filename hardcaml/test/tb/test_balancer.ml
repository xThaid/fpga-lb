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
  
  let sim = Sim.create ~name:"balancer" ~gtkwave:true ~trace:false () in

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

  Sim.cycle_n sim 1;

  write_vip_map "12121212" 2;
  write_vip_map "14141414" 4;
  write_vip_map "16161616" 5;

  udp_pkt "01010101" "11111111" "aaaa" "b0b0" 30;
  udp_pkt "02020202" "12121212" "bbbb" "d0d0" 15;
  udp_pkt "03030303" "13131313" "cccc" "c0c0" 32;
  udp_pkt "04040404" "14141414" "dddd" "e0e0" 14;
  udp_pkt "05050505" "15151515" "eeee" "d0d0" 13;
  udp_pkt "06060606" "16161616" "ffff" "f0f0" 41;
  udp_pkt "06060606" "16161616" "ffff" "f0f0" 41;
  udp_pkt "06060606" "16161616" "ffff" "f0f0" 41;

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

  Consumer.expect_transfers consumer;

  [%expect {|
    (consumed ()) |}]
