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
    inputs.write.data.ip := Bits.of_hex ~width:32 ip;
    inputs.write.data.mac := Bits.of_hex ~width:48 mac;
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

  [%expect {| a88b901a590719815500c51ec3d4b3af |}]


module BusAdapterSim = struct
  module BusAgent = Arp.Table.WriteBusAdapter.Agent

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
      { write_port : 'a Arp.Table.WritePort.I.t [@rtlprefix "port_"]
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

    Arp.Table.WriteBusAdapter.create spec ~bus ~write_port:outs.write_port;

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

  Consumer.expect_transfers consumer;

  [%expect {|
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
