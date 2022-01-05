open Base
open Hardcaml
open Lb_dataplane

module ArpTableSim = struct
  module I = Arp_table.I
  module O = Arp_table.O

  let create_fn (scope : Scope.t) (i : Signal.t I.t) : (Signal.t O.t) =
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


module BusAdapterSim = struct
  module BusAgent = Arp_table.WriteBusAdapter.Agent

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; bus : 'a BusAgent.I.t [@rtlprefix "bus_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { write_port : 'a Arp_table.WritePort.I.t [@rtlprefix "port_"]
      ; bus : 'a BusAgent.O.t [@rtlprefix "bus_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create_fn (_scope : Scope.t) (i : Signal.t I.t) : (Signal.t O.t) =
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in

    let bus = BusAgent.create_empty () in

    let outs = O.Of_signal.wires ~named:true () in

    BusAgent.I.Of_signal.assign (BusAgent.inputs bus) i.bus;
    BusAgent.O.Of_signal.assign outs.bus (BusAgent.outputs bus);

    Arp_table.WriteBusAdapter.create spec ~bus ~write_port:outs.write_port;

    outs

end

let%expect_test "arp_table_bus_write_adapter" =
  let module Sim = Sim.Sim(BusAdapterSim) in
  
  let sim = Sim.create ~name:"arp_table_bus_write_adapter" ~gtkwave:false ~trace:false () in

  let inputs = Sim.inputs sim in

  let write_bus addr data = 
    inputs.bus.address := Bits.of_int ~width:2 addr;
    inputs.bus.writedata := Bits.of_hex ~width:32 data;
    inputs.bus.write := Bits.vdd;
    Sim.cycle sim;
    inputs.bus.write := Bits.gnd
  in

  write_bus 0 "ff";
  write_bus 1 "bb";
  write_bus 2 "33";

  write_bus 2 "44";

  write_bus 3 "ff00bbaa";

  write_bus 1 "ffffffff";
  write_bus 0 "aaaaaaaa";

  write_bus 2 "11223344";
  
  Sim.cycle_n sim 5;

  Sim.expect_trace_digest sim;

  [%expect {| ac8d220536b8c13d3981e9ad8aae1d9e |}]
