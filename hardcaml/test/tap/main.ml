open Hardcaml
open Lb_dataplane
open Lb_dataplane_test_lib
open Sim_elements

module DataplaneSim = struct
  module I = Dataplane.I
  module O = Dataplane.O

  let create_fn (scope : Scope.t) (i : Signal.t I.t) : (Signal.t O.t) =
    Dataplane.create_from_if scope i

end

let () =
  let module Sim = Sim.Sim(DataplaneSim) in
  let module Emitter = AvalonFlowEmitter in
  let module Consumer = AvalonFlowConsumer in

  let sim = Sim.create ~name:"dataplane" ~gtkwave:false ~trace:false () in

  let inputs = Sim.inputs sim in
  let outputs = Sim.outputs sim in

  let emitter = Emitter.create inputs.rx outputs.rx in
  let consumer = Consumer.create outputs.tx inputs.tx in

  Sim.add_element sim (module Emitter) emitter;
  Sim.add_element sim (module Consumer) consumer;
