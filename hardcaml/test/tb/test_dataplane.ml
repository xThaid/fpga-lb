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

let serialize_ipv4_hdr (hdr : string Common.IPv4Header.t) = 
  let pkt = Common.IPv4Header.map2 Common.IPv4Header.port_widths hdr ~f:(fun width v -> Bits.of_hex ~width v) in
  let checksum = Ip.calc_checksum (module Bits) pkt in
  let pkt = {pkt with hdr_checksum = checksum} in
  Common.IPv4Header.Of_bits.pack ~rev:true pkt |>
  Bits.split_msb ~part_width:8 |>
  List.map ~f:Bits.to_char

let serialize_icmp_echo_req (req : string Common.ICMPEchoRequest.t) = 
  let req = Common.ICMPEchoRequest.map2 Common.ICMPEchoRequest.port_widths req ~f:(fun width v -> Bits.of_hex ~width v) in
  let checksum = Hashes.one_complement_sum (module Bits) (Common.ICMPEchoRequest.Of_bits.pack ~rev:true req) in
  let req = {req with checksum} in
  Common.ICMPEchoRequest.Of_bits.pack ~rev:true req |>
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

let create_ipv4_hdr total_length protocol src_ip dst_ip=
  { Common.IPv4Header.version = "4"
  ; ihl = "5"
  ; dscp = "0"
  ; ecn = "0"
  ; total_length = Constant.to_hex_string ~signedness:Unsigned (Constant.of_int ~width:16 total_length)
  ; identification = "0000"
  ; flags = "0"
  ; fragment_offset = "0"
  ; ttl = "ff"
  ; protocol
  ; hdr_checksum = "0"
  ; src_ip
  ; dst_ip
  }

let create_icmp_echo_req identifier seq_number = 
  { Common.ICMPEchoRequest.type_of_msg = "8"
    ; code = "0"
    ; checksum = "0"
    ; identifier
    ; seq_number
    }

let create_arp_req src_mac spa tpa =
  let eth = create_eth_hdr src_mac "ffffffffffff" "0806" in
  let arp = create_arp_pkt "01" src_mac spa "000000000000" tpa in
  let padding = (List.init 18 ~f:(fun _ -> Char.of_int_exn 0)) in
  
  let data = serialize_eth_hdr eth @ serialize_arp_pkt arp @ padding in
  Bytes.of_char_list data

let create_arp_resp src_mac spa =
  let eth = create_eth_hdr src_mac "ffffffffffff" "0806" in
  let arp = create_arp_pkt "02" src_mac spa "000000000000" spa in
  let padding = (List.init 18 ~f:(fun _ -> Char.of_int_exn 0)) in
  
  let data = serialize_eth_hdr eth @ serialize_arp_pkt arp @ padding in
  Bytes.of_char_list data

let create_icmp_echo_req src_mac dst_mac src_ip dst_ip id seq =
  let eth = create_eth_hdr src_mac dst_mac "0800" in
  let icmp = create_icmp_echo_req id seq in
  let icmp_serialized = serialize_icmp_echo_req icmp in

  let ipv4 = create_ipv4_hdr ((List.length icmp_serialized) + 20) "01" src_ip dst_ip in

  let data = serialize_eth_hdr eth @ serialize_ipv4_hdr ipv4 @ icmp_serialized in
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

  Emitter.add_transfer emitter (create_icmp_echo_req "121234345656" "cccccccccccc" "0a640009" "0a0b0c0d" "bacd" "9876");
  Emitter.add_transfer emitter (create_arp_resp "a0b0c0d0e0f0" "ff00ff00ff");
  
  Emitter.add_transfer emitter (create_icmp_echo_req "121234345656" "cccccccccccc" "0a640009" "0a0b0c0d" "bacd" "9876");

  for _ = 1 to 16 do
    Emitter.add_transfer emitter (create_icmp_echo_req "ffffffffffff" "dddddddddddd" "0a640008" "0a0b0c0d" "b003" "0001")
  done;

  Sim.cycle_n sim 2;

  emitter.enabled <- true;
  consumer.enabled <- true;

  Sim.cycle_n sim 400;

  Consumer.expect_data_digest consumer;

  [%expect {|
    (digest 9197b661972d86ace40819393b58267f) |}]
