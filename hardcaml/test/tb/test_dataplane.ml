open Base
open Hardcaml
open Lb_dataplane
open Lb_dataplane_test_lib
open Sim_elements

let serialize_eth_hdr (hdr : string Common.EthernetHeader.t) =
  Common.EthernetHeader.map2 Common.EthernetHeader.port_widths hdr ~f:(fun width v -> Bits.of_hex ~width v) |>
  Common.EthernetHeader.Of_bits.pack ~rev:true |>
  Bits.split_msb ~part_width:8 |>
  List.map ~f:Bits.to_char

let serialize_arp_pkt (pkt : string Common.ArpPacket.t) = 
  Common.ArpPacket.map2 Common.ArpPacket.port_widths pkt ~f:(fun width v -> Bits.of_hex ~width v) |>
  Common.ArpPacket.Of_bits.pack ~rev:true |>
  Bits.split_msb ~part_width:8 |>
  List.map ~f:Bits.to_char

let create_eth_hdr src_mac dest_mac ether_type = 
  { Common.EthernetHeader.src_mac
  ; dest_mac
  ; ether_type
  }

let create_arp_pkt oper sha spa tha tpa =
  { Common.ArpPacket.htype = "0001"
  ; ptype = "0800"
  ; hlen = "06"
  ; plen = "04"
  ; oper; sha; spa; tha; tpa
  }

let create_arp_req src_mac spa tpa =
  let eth = create_eth_hdr src_mac "ffffffffffff" "0806" in
  let arp = create_arp_pkt "01" src_mac spa "000000000000" tpa in
  let padding = (List.init 18 ~f:(fun _ -> Char.of_int_exn 0)) in
  
  let data = serialize_eth_hdr eth @ serialize_arp_pkt arp @ padding in
  Bytes.of_char_list data

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

  Emitter.add_transfer emitter (create_arp_req "112233445566" "99999999" "0a640001");
  Emitter.add_transfer emitter (create_arp_req "aaaaaaaaaaaa" "88888888" "0a640001");
  Emitter.add_transfer emitter (create_arp_req "bbbbbbbbbbbb" "77777777" "0a640001");

  Sim.cycle_n sim 2;

  emitter.enabled <- true;
  consumer.enabled <- true;

  Sim.cycle_n sim 100;

  Consumer.expect_data consumer;

  [%expect {|
    11223344 5566aabb ccddeeff 08060001
    08000604 0002aabb ccddeeff 0a640001
    11223344 55669999 9999

    aaaaaaaa aaaaaabb ccddeeff 08060001
    08000604 0002aabb ccddeeff 0a640001
    aaaaaaaa aaaa8888 8888

    bbbbbbbb bbbbaabb ccddeeff 08060001
    08000604 0002aabb ccddeeff 0a640001
    bbbbbbbb bbbb7777 7777 |}]
