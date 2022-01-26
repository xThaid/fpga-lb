open Base
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

let%expect_test "dataplane" =
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

  Emitter.add_transfer emitter (Packet.create_arp_req "112233445566" "99999999" "0a640001");
  Emitter.add_transfer emitter (Packet.create_arp_req "aaaaaaaaaaaa" "88888888" "0a640001");
  Emitter.add_transfer emitter (Packet.create_arp_req "bbbbbbbbbbbb" "77777777" "0a640001");

  Emitter.add_transfer emitter (Packet.create_icmp_echo_req "121234345656" "cccccccccccc" "0a640009" "0a0b0c0d" "bacd" "9876");
  Emitter.add_transfer emitter (Packet.create_arp_resp "a0b0c0d0e0f0" "ff00ff00ff");
  Emitter.add_transfer emitter (Packet.create_arp_resp "fafafafafafa" "0a0b0c0d");
  
  (*
  Emitter.add_transfer emitter (Packet.create_icmp_echo_req "121234345656" "cccccccccccc" "0a640009" "0a0b0c0d" "bacd" "9876");

  for _ = 1 to 16 do
    Emitter.add_transfer emitter (Packet.create_icmp_echo_req "ffffffffffff" "dddddddddddd" "0a640008" "0a0b0c0d" "b003" "0001")
  done;
  *)

  Sim.cycle_n sim 2;

  emitter.enabled <- true;
  consumer.enabled <- true;

  Sim.cycle_n sim 400;

  Consumer.expect_data_digest consumer;

  [%expect {|
    (digest 5344f1ac7a2e01598e71e21b3df39962) |}]
