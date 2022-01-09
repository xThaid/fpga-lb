open Base
open Hardcaml
open Lb_dataplane

module ArpTableSim = struct
  module I = Arp.I
  module O = Arp.O

  let create_fn (scope : Scope.t) (i : Signal.t I.t) : (Signal.t O.t) =
    let o = O.Of_signal.wires () in
    Arp.create_from_if scope i o;
    o

end

let%expect_test "arp" =
  let module Sim = Sim.Sim(ArpTableSim) in
  
  let sim = Sim.create ~name:"arp" ~gtkwave:false ~trace:false () in

  Sim.cycle_n sim 10;

  Sim.expect_trace_digest sim;
  Sim.output_verilog sim;

  [%expect {| 66c20b7442bcded75e1a796395d09879 |}]
