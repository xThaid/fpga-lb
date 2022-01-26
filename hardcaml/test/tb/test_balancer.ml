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
  
  let sim = Sim.create ~name:"balancer" ~gtkwave:false ~trace:false () in

  let inputs = Sim.inputs sim in
  let outputs = Sim.outputs sim in

  let emitter = Emitter.create inputs.ip_rx outputs.ip_rx in
  let consumer = Consumer.create outputs.ip_tx inputs.ip_tx in

  Sim.add_element sim (module Emitter) emitter;
  Sim.add_element sim (module Consumer) consumer;

  let _udp_pkt sha spa tpa = 
    let ip = Packet.create_ipv4_hdr 3 "11" "" "" in
    let arp = Packet.create_arp_pkt "01" sha spa "000000000000" tpa in
    let padding = (List.init 18 ~f:(fun _ -> Char.of_int_exn 0)) in
  
    Emitter.add_transfer emitter ip (Bytes.of_char_list (Packet.serialize_arp_pkt arp @ padding))
  in

  Sim.cycle_n sim 1;

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

  Sim.cycle_n sim 50;

  Consumer.expect_transfers consumer;

  [%expect {|
    (consumed ()) |}]
