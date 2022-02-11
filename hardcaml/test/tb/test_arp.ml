open Base
open Hardcaml
open Lb_dataplane
open Lb_dataplane_test_lib
open Sim_elements

module ArpTableSim = struct
  module I = Arp.Table.I
  module O = Arp.Table.O

  let create_fn (scope : Scope.t) (i : Signal.t I.t) : (Signal.t O.t) =
    Arp.Table.create ~capacity:32 scope i

end

let%expect_test "arp_table" =
  let module Sim = Sim.Sim(ArpTableSim) in
  
  let sim = Sim.create ~name:"arp_table" ~gtkwave:false ~trace:false () in

  let inputs = Sim.inputs sim in
  let _outputs = Sim.outputs sim in

  let write_cache ip mac = 
    inputs.write.data.key.ip := Bits.of_hex ~width:32 ip;
    inputs.write.data.data.mac := Bits.of_hex ~width:48 mac;
    inputs.write.valid := Bits.vdd;
    Sim.cycle sim;
    inputs.write.valid := Bits.gnd
  in

  let send_query ip = 
    inputs.query.req.data.ip := Bits.of_hex ~width:32 ip;
    inputs.query.req.valid := Bits.vdd;
    Sim.cycle sim;
    inputs.query.req.valid := Bits.gnd;
  in

  Sim.cycle_n sim 2;

  inputs.query.resp.ready := Bits.vdd;

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
  inputs.query.resp.ready := Bits.vdd;
  send_query "0001";
  inputs.query.resp.ready := Bits.gnd;
  Sim.cycle_n sim 4;
  inputs.query.resp.ready := Bits.vdd;
  Sim.cycle_n sim 2;
  send_query "0001";
  inputs.query.resp.ready := Bits.gnd;
  Sim.cycle sim;
  send_query "0002";
  send_query "00ff";
  send_query "aaaa";
  inputs.query.resp.ready := Bits.vdd;
  Sim.cycle sim;

  Sim.cycle_n sim 10;

  Sim.expect_trace_digest sim;

  [%expect {| f1ab05e02ca13f52c53aa1c8ce2bb09c |}]

module ArpSim = struct
  module I = Arp.I
  module O = Arp.O

  let create_fn (scope : Scope.t) (i : Signal.t I.t) : (Signal.t O.t) =
    let o = O.Of_signal.wires () in
    Arp.create_from_if scope i o;
    o

end

let%expect_test "arp" =
  let module Sim = Sim.Sim(ArpSim) in
  let module Emitter = FlowWithHeaderEmitter(Common.EthernetHeader) in
  let module Consumer = FlowWithHeaderConsumer(Common.EthernetHeader) in
  
  let sim = Sim.create ~name:"arp" ~gtkwave:false ~trace:false () in

  let inputs = Sim.inputs sim in
  let outputs = Sim.outputs sim in

  let emitter = Emitter.create inputs.rx outputs.rx in
  let consumer = Consumer.create outputs.tx inputs.tx in

  Sim.add_element sim (module Emitter) emitter;
  Sim.add_element sim (module Consumer) consumer;

  let arp_req sha spa tpa = 
    let eth = Packet.create_eth_hdr sha "ffffffffffff" "0806" in
    let arp = Packet.create_arp_pkt "01" sha spa "000000000000" tpa in
    let padding = (List.init 18 ~f:(fun _ -> Char.of_int_exn 0)) in
  
    Emitter.add_transfer emitter eth (Bytes.of_char_list (Packet.serialize_arp_pkt arp @ padding))
  in

  inputs.cfg.mac_addr := Bits.of_hex ~width:48 "aabbccddeeff";
  inputs.cfg.vips := Bits.uresize (Bits.of_hex ~width:32 "0a640001") (32 * 6);

  arp_req "a1a2a3a4a5a6" "b1b2b3b4" "0a640001";
  arp_req "f1f2f3f4f5f6" "a1a2a3a4" "0a640001";
  arp_req "f1f2f3f4f5f6" "a1a2a3a4" "0a640001";
  arp_req "aaaaaaaaaaaa" "eeeeeeee" "0a640002";
  arp_req "bbbbbbbbbbbb" "ffffffff" "0a640001";
  arp_req "bbbbbbbbbbbb" "ffffffff" "0a640001";
  arp_req "cccccccccccc" "fefefefe" "0a640001";

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
    (consumed
     (((dest_mac a1a2a3a4a5a6) (src_mac aabbccddeeff) (ether_type 0806))
      ((dest_mac f1f2f3f4f5f6) (src_mac aabbccddeeff) (ether_type 0806))
      ((dest_mac f1f2f3f4f5f6) (src_mac aabbccddeeff) (ether_type 0806))
      ((dest_mac bbbbbbbbbbbb) (src_mac aabbccddeeff) (ether_type 0806))
      ((dest_mac bbbbbbbbbbbb) (src_mac aabbccddeeff) (ether_type 0806))
      ((dest_mac cccccccccccc) (src_mac aabbccddeeff) (ether_type 0806))))
    00010800 06040002 aabbccdd eeff0a64
    0001a1a2 a3a4a5a6 b1b2b3b4

    00010800 06040002 aabbccdd eeff0a64
    0001f1f2 f3f4f5f6 a1a2a3a4

    00010800 06040002 aabbccdd eeff0a64
    0001f1f2 f3f4f5f6 a1a2a3a4

    00010800 06040002 aabbccdd eeff0a64
    0001bbbb bbbbbbbb ffffffff

    00010800 06040002 aabbccdd eeff0a64
    0001bbbb bbbbbbbb ffffffff

    00010800 06040002 aabbccdd eeff0a64
    0001cccc cccccccc fefefefe |}]
