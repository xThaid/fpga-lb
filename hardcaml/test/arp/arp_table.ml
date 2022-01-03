open Base
open Hardcaml
open Lb_dataplane

module ArpTableSim = struct
  module I = Arp_table.I
  module O = Arp_table.O

  let create_fn (i : Signal.t I.t) : (Signal.t O.t) =
    let scope = Scope.create ~flatten_design:true () in
    Arp_table.create ~capacity:32 scope i

end

let%expect_test "arp_table" =
  let module Sim = Sim.Sim(ArpTableSim) in
  
  let sim = Sim.create ~name:"arp_table" ~gtkwave:false ~trace:false () in

  let inputs = Sim.inputs sim in
  let _outputs = Sim.outputs sim in

  let write_cache ip mac = 
    inputs.write.ip := Bits.of_hex ~width:32 ip;
    inputs.write.mac := Bits.of_hex ~width:48 mac;
    inputs.write.valid := Bits.vdd;
    Sim.cycle sim;
    inputs.write.valid := Bits.gnd
  in

  let send_query ip = 
    inputs.query.ip := Bits.of_hex ~width:32 ip;
    inputs.query.req_valid := Bits.vdd;
    Sim.cycle sim;
    inputs.query.req_valid := Bits.gnd;
  in

  Sim.cycle_n sim 2;

  inputs.query.resp_ready := Bits.vdd;

  write_cache "0001" "0001";
  write_cache "0002" "2000";
  write_cache "00ff" "00ff";
  write_cache "f0f0" "fafa";

  Sim.cycle_n sim 2;

  send_query "0002";
  Sim.cycle sim;
  send_query "bbbb";
  Sim.cycle sim;
  send_query "f0f0";
  Sim.cycle_n sim 2;

  send_query "00ff";
  send_query "cccc";
  send_query "0002";
  Sim.cycle_n sim 2;

  send_query "00ff";
  send_query "0002";
  Sim.cycle_n sim 4;
  inputs.query.resp_ready := Bits.vdd;
  send_query "0001";
  inputs.query.resp_ready := Bits.gnd;
  Sim.cycle_n sim 4;
  inputs.query.resp_ready := Bits.vdd;
  Sim.cycle_n sim 2;
  send_query "0001";
  inputs.query.resp_ready := Bits.gnd;
  Sim.cycle sim;
  send_query "0002";
  send_query "00ff";
  send_query "aaaa";
  inputs.query.resp_ready := Bits.vdd;
  Sim.cycle sim;

  Sim.cycle_n sim 10;

  Sim.expect_trace_digest sim;

  [%expect {| 66c20b7442bcded75e1a796395d09879 |}]
