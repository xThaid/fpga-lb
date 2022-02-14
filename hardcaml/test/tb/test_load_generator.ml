open Base
open Hardcaml
open Lb_dataplane
open Lb_dataplane_test_lib
open Sim_elements

module LoadGeneratorSim = struct
  module I = Load_generator.I
  module O = Load_generator.O

  let create_fn (scope : Scope.t) (i : Signal.t I.t) : (Signal.t O.t) =
    Load_generator.create_from_if scope i

end

let%expect_test "load_generator" =
  let module Sim = Sim.Sim(LoadGeneratorSim) in
  let module Consumer = AvalonFlowConsumer in
  let module Emitter = AvalonFlowEmitter in
  let module BusHost = BusHost(Load_generator.BusAgent) in
  
  let sim = Sim.create ~name:"load_generator" ~gtkwave:false ~trace:false () in

  let inputs = Sim.inputs sim in
  let outputs = Sim.outputs sim in

  let bus = BusHost.create outputs.bus inputs.bus in
  let emitter = Emitter.create inputs.rx outputs.rx in
  let consumer = Consumer.create outputs.tx inputs.tx in

  Sim.add_element sim (module BusHost) bus;
  Sim.add_element sim (module Emitter) emitter;
  Sim.add_element sim (module Consumer) consumer;

  let arp_resp src_mac spa = Emitter.add_transfer emitter (Packet.create_arp_resp src_mac spa) in

  let write_bus addr data = 
    BusHost.schedule_write bus addr (Constant.to_int (Constant.of_hex_string ~signedness:Unsigned ~width:32 data));
  in

  let enable_generator en =
    write_bus 7 (if en then "00000001" else "00000000")
  in

  Sim.cycle_n sim 2;

  consumer.enabled <- true;
  emitter.enabled <- true;

  arp_resp "bee0beef0001" "0a000a02";
  
  write_bus 0 "00000f00";
  write_bus 1 "deadbeef";
  write_bus 2 "0a000a01";
  write_bus 3 "0a000a02";
  write_bus 4 "0035a000";
  write_bus 5 "00000020";
  write_bus 6 "0000000a";
  enable_generator true;

  Sim.cycle_n sim 100;

  consumer.enabled <- false;
  Sim.cycle_n sim 20;
  consumer.enabled <- true;
  Sim.cycle_n sim 20;

  enable_generator false;
  Sim.cycle_n sim 100;
  write_bus 5 "00000010";
  write_bus 6 "00000020";
  enable_generator true;

  Sim.cycle_n sim 100;
  write_bus 6 "00000040";
  Sim.cycle_n sim 100;
  write_bus 6 "00000001";
  Sim.cycle_n sim 100;
  write_bus 5 "00000080";

  Sim.cycle_n sim 200;

  enable_generator false;

  Sim.cycle_n sim 200;

  let read_stats () = 
    BusHost.schedule_read bus 8;
    BusHost.schedule_read bus 9;
    BusHost.schedule_read bus 10;

    Sim.cycle_n sim 5;
    
    let pkt_cnt = Linked_queue.dequeue_exn bus.responses in
    let bytes_cnt_lo = Linked_queue.dequeue_exn bus.responses in
    let bytes_cnt_hi = Linked_queue.dequeue_exn bus.responses in

    let bytes_cnt = (Int.shift_left bytes_cnt_hi 32) + bytes_cnt_lo in

    Stdio.print_s [%message (pkt_cnt : int) (bytes_cnt : int)]
  in

  read_stats ();

  Consumer.expect_data_digest consumer;

  [%expect {|
    ((pkt_cnt 26) (bytes_cnt 1944))
    (digest f7951c2a2c9dbcfe6f952dd344514a2d) |}]
