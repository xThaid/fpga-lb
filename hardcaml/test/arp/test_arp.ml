open Base
open Hardcaml
open Lb_dataplane
open Sim_elements

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
  let module Emitter = TransactionWithFlowEmitter(Common.EthernetHeader) in
  let module Consumer = TransactionWithFlowConsumer(Common.EthernetHeader) in
  
  let sim = Sim.create ~name:"arp" ~gtkwave:false ~trace:false () in

  let inputs = Sim.inputs sim in
  let outputs = Sim.outputs sim in

  let emitter = Emitter.create inputs.rx outputs.rx in
  let consumer = Consumer.create outputs.tx inputs.tx in

  Sim.add_element sim (module Emitter) emitter;
  Sim.add_element sim (module Consumer) consumer;

  let get_eth_hdr src_mac dest_mac =
    { Common.EthernetHeader.src_mac
    ; dest_mac
    ; ether_type = "0806"
    }
  in
  let get_arp_as_bytes oper sha spa tha tpa ?(zeroes=0) ()=
    let pkt = 
      { Common.ArpPacket.htype = "0001"
      ; ptype = "0800"
      ; hlen = "06"
      ; plen = "04"
      ; oper; sha; spa; tha; tpa
      }
    in
    let data = Common.ArpPacket.map2 Common.ArpPacket.port_widths pkt ~f:(fun width v -> Bits.of_hex ~width v) |>
      Common.ArpPacket.Of_bits.pack ~rev:true |> Bits.split_msb ~part_width:8 |> List.map ~f:Bits.to_char
    in
    data @ (List.init zeroes ~f:(fun _ -> Char.of_int_exn 0)) |> Bytes.of_char_list
  in
  let arp_req sha spa tpa = get_arp_as_bytes "01" sha spa "000000000000" tpa () in

  Emitter.add_transfer emitter (get_eth_hdr "010203040506" "000000000000") (arp_req "a1a2a3a4a5a6" "b1b2b3b4" "0a640001");
  Emitter.add_transfer emitter (get_eth_hdr "e1e2e3e4e5e6" "000000000000") (arp_req "f1f2f3f4f5f6" "a1a2a3a4" "0a640001");
  Emitter.add_transfer emitter (get_eth_hdr "e1e2e3e4e5e6" "000000000000") (arp_req "f1f2f3f4f5f6" "a1a2a3a4" "0a640001");
  Emitter.add_transfer emitter (get_eth_hdr "111111111111" "000000000000") (arp_req "aaaaaaaaaaaa" "eeeeeeee" "0a640002");
  Emitter.add_transfer emitter
    (get_eth_hdr "222222222222" "000000000000")
    (get_arp_as_bytes "01" "bbbbbbbbbbbb" "ffffffff" "000000000000" "0a640001" ~zeroes:32 ());
  Emitter.add_transfer emitter (get_eth_hdr "222222222222" "000000000000") ((arp_req "bbbbbbbbbbbb" "ffffffff" "0a640001"));
  Emitter.add_transfer emitter (get_eth_hdr "333333333333" "000000000000") (arp_req "cccccccccccc" "fefefefe" "0a640001");

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

  Sim.expect_trace_digest sim;
  Consumer.expect_transfers consumer;

  [%expect {|
    b8f7f6529e1135294313670334868ea0
    (consumed
     (((dest_mac 010203040506) (src_mac aabbccddeeff) (ether_type 0806))
      ((dest_mac e1e2e3e4e5e6) (src_mac aabbccddeeff) (ether_type 0806))
      ((dest_mac e1e2e3e4e5e6) (src_mac aabbccddeeff) (ether_type 0806))
      ((dest_mac 222222222222) (src_mac aabbccddeeff) (ether_type 0806))
      ((dest_mac 222222222222) (src_mac aabbccddeeff) (ether_type 0806))
      ((dest_mac 333333333333) (src_mac aabbccddeeff) (ether_type 0806))))
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
