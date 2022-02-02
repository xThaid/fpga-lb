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
  let module BusHost = BusHost(Dataplane.BusAgent) in
  
  let sim = Sim.create ~name:"dataplane" ~gtkwave:false ~trace:false () in

  let inputs = Sim.inputs sim in
  let outputs = Sim.outputs sim in

  let bus = BusHost.create outputs.bus inputs.bus in
  let emitter = Emitter.create inputs.rx outputs.rx in
  let consumer = Consumer.create outputs.tx inputs.tx in

  Sim.add_element sim (module BusHost) bus;
  Sim.add_element sim (module Emitter) emitter;
  Sim.add_element sim (module Consumer) consumer;

  let arp_req sha spa tpa = Emitter.add_transfer emitter (Packet.create_arp_req sha spa tpa) in
  let arp_resp src_mac spa = Emitter.add_transfer emitter (Packet.create_arp_resp src_mac spa) in

  let udp_pkt src_mac dst_mac src_ip dst_ip src_port dst_port payload_len = 
    let payload = (List.init payload_len ~f:(fun i -> Char.of_int_exn i)) in
    let udp = Packet.create_udp_hdr src_port dst_port (8 + payload_len) in
    let ip = Packet.create_ipv4_hdr (20 + 8 + payload_len) "11" src_ip dst_ip in
    let eth = Packet.create_eth_hdr src_mac dst_mac "0800" in
  
    let data = Packet.serialize_eth_hdr eth @ Packet.serialize_ipv4_hdr ip @ Packet.serialize_udp_hdr udp @ payload in
    Emitter.add_transfer emitter (Bytes.of_char_list data)
  in

  let write_vip_map ip idx =
    BusHost.schedule_write bus 0 (Constant.to_int (Constant.of_hex_string ~signedness:Unsigned ~width:32 ip));
    BusHost.schedule_write bus 1 idx
  in

  let write_hash_ring vip_idx slot real_idx = 
    BusHost.schedule_write bus 3 real_idx;
    BusHost.schedule_write bus 2 ((vip_idx * Balancer.Consts.ring_size) + slot);
  in

  let write_real_info real_idx real_ip =
    BusHost.schedule_write bus 5 (Constant.to_int (Constant.of_hex_string ~signedness:Unsigned ~width:32 real_ip));
    BusHost.schedule_write bus 4 real_idx;
  in
  
  Sim.cycle_n sim 2;

  emitter.enabled <- true;
  consumer.enabled <- true;

  BusHost.schedule_write bus 138 (Constant.to_int (Constant.of_hex_string ~signedness:Unsigned ~width:32 "ccddeeff"));
  BusHost.schedule_write bus 139 (Constant.to_int (Constant.of_hex_string ~signedness:Unsigned ~width:32 "0000aabb"));
  BusHost.schedule_write bus 140 (Constant.to_int (Constant.of_hex_string ~signedness:Unsigned ~width:32 "0a640001"));

  write_vip_map "88008800" 2;
  write_vip_map "99009900" 3;

  for i = 0 to 1 do
    write_hash_ring 2 (i + 0) (i + 10);
    write_hash_ring 2 (i + 2) (i + 10);
    write_hash_ring 2 (i + 4) (i + 10);
    write_hash_ring 2 (i + 6) (i + 10);
  done;

  for i = 0 to 3 do
    write_hash_ring 3 (i + 0) (i + 20);
    write_hash_ring 3 (i + 4) (i + 20)
  done;

  write_real_info 0 "efefefef";

  write_real_info 10 "face0001";
  write_real_info 11 "face0002";

  write_real_info 20 "beef0001";
  write_real_info 21 "beef0002";
  write_real_info 22 "beef0003";
  write_real_info 23 "beef0004";

  arp_req "112233445566" "99999999" "0a640001";
  arp_req "aaaaaaaaaaaa" "88888888" "0a640001";
  arp_req "bbbbbbbbbbbb" "77777777" "0a640001";
  
  Sim.cycle_n sim 100;

  udp_pkt "202020202020" "303030303030" "dead0000" "01010101" "ff00" "0050" 16;
  udp_pkt "202020202020" "303030303030" "dead0000" "88008800" "ff00" "0050" 16;

  Sim.cycle_n sim 40;

  arp_resp "bee0face0001" "face0001";
  arp_resp "bee0face0002" "face0002";

  Sim.cycle_n sim 50;
  
  udp_pkt "202020202020" "303030303030" "dead0000" "88008800" "ff00" "0050" 16;
  udp_pkt "202020202020" "303030303030" "dead0001" "88008800" "ff02" "0050" 16;
  udp_pkt "202020202020" "303030303030" "dead0002" "88008800" "ff03" "0050" 16;
  udp_pkt "202020202020" "303030303030" "dead0002" "88008801" "ff03" "0050" 16;

  udp_pkt "202020202020" "303030303030" "dead0000" "99009900" "ff03" "0050" 16;

  Sim.cycle_n sim 200;

  arp_resp "bee0beef0001" "beef0001";
  arp_resp "bee0beef0002" "beef0002";
  arp_resp "bee0beef0003" "beef0003";
  arp_resp "bee0beef0004" "beef0004";

  udp_pkt "202020202020" "303030303030" "dead0000" "99009900" "ff03" "0050" 16;
  udp_pkt "202020202020" "303030303030" "dead0000" "99009900" "ff03" "0050" 16;
  udp_pkt "202020202020" "303030303030" "dead0005" "99009900" "fff0" "0050" 16;
  udp_pkt "202020202020" "303030303030" "dead0005" "99009900" "fff0" "0050" 16;

  Sim.cycle_n sim 200;

  Consumer.expect_data_digest consumer;

  [%expect {|
    (digest 21871f54375a0e44b5db895fea7dd5d9) |}]
