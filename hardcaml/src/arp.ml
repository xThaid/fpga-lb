open Base
open Hardcaml

module Eth_flow = Flow.With_header(Common.EthernetHeader)
module EthTst = Transaction.Make(Common.EthernetHeader)
module EthArpTst = Transaction.Of_pair(Common.EthernetHeader)(Common.ArpPacket)

module Table = struct
  module Key = struct
    type 'a t =
      { ip : 'a [@bits 32]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Data = struct
    type 'a t =
      { mac : 'a [@bits 48]
      }
    [@@deriving sexp_of, hardcaml]
  end

  include Hash_table.Make(Key)(Data)
end

module Config = struct
  let arp_table_capacity = 32
  let mac_addr = "aabbccddeeff"
  let mac_addr_bytes = Signal.of_hex ~width:48 mac_addr
  let ip_addr = [10;100;0;1]
  let ip_addr_bytes = List.map ip_addr ~f:(Signal.of_int ~width:8) |> Signal.concat_msb
end

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; rx : 'a Eth_flow.Src.t
    ; tx : 'a Eth_flow.Dst.t
    ; query : 'a Table.QueryPort.I.t
    }
  [@@deriving sexp_of, hardcaml ~rtlmangle:true]
end

module O = struct
  type 'a t = 
    { rx : 'a Eth_flow.Dst.t 
    ; tx : 'a Eth_flow.Src.t
    ; query : 'a Table.QueryPort.O.t
    }
  [@@deriving sexp_of, hardcaml ~rtlmangle:true]
end

let datapath spec ~(rx : Eth_flow.t) (table_write_port : Table.WritePort.t) = 
  let module Serializer = Flow.Serializer(Common.ArpPacket) in

  let tst_in = EthArpTst.join_comb rx.hdr (Serializer.deserialize spec rx.flow) in

  let arp_in_req, arp_in_resp = 
    EthArpTst.demux2_on tst_in ~f:(fun pkt -> Signal.(pkt.snd.oper ==:. 1))
  in

  EthArpTst.apply arp_in_resp ~f:(fun ~valid ~data ->
    let open Signal in

    table_write_port.s.valid <== valid;
    Table.Key.Of_signal.assign table_write_port.s.data.key {Table.Key.ip = data.snd.spa};
    Table.Data.Of_signal.assign table_write_port.s.data.data {Table.Data.mac = data.snd.sha};

    Signal.vdd
  );

  let pkt_out = EthArpTst.filter_comb arp_in_req ~f:(fun pkt ->
    let open Signal in
    pkt.snd.tpa ==: Config.ip_addr_bytes
  ) |>

  EthArpTst.map_comb ~f:(fun pkt ->
    let eth =
      { pkt.fst with
        dest_mac = pkt.fst.src_mac
      ; src_mac = Config.mac_addr_bytes
      }
    in
    let arp =
      { pkt.snd with
        oper = Signal.of_int ~width:16 2
      ; sha = Config.mac_addr_bytes
      ; spa = Config.ip_addr_bytes
      ; tha = pkt.snd.sha
      ; tpa = pkt.snd.spa
      }
    in
    EthArpTst.Data.create eth arp
  ) in

  let eth_out, arp_out = EthArpTst.split_comb pkt_out in
  Eth_flow.create (EthTst.bufferize spec eth_out) (Serializer.serialize spec arp_out)

let create
      (scope : Scope.t)
      spec
      ~(rx : Eth_flow.t)
      ~(tx : Eth_flow.t)
      ~(query : Table.QueryPort.t) =
  
  let table_write_port = Table.WritePort.create_wires () in
  
  Table.hierarchical ~name:"arp_table" ~capacity:Config.arp_table_capacity scope spec ~query_port:query ~write_port:table_write_port;
  
  Eth_flow.connect tx (datapath spec ~rx table_write_port)

let create_from_if (scope : Scope.t) (i : Signal.t I.t) (o : Signal.t O.t) =
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in

  let rx = Eth_flow.t_of_if i.rx o.rx in
  let tx = Eth_flow.t_of_if o.tx i.tx in
  let query = Table.QueryPort.t_of_if i.query o.query in

  create scope spec ~rx ~tx ~query

let hierarchical
      (scope : Scope.t)
      spec
      ~(rx : Eth_flow.t) =
  let module H = Hierarchy.In_scope(I)(O) in

  let clock = Reg_spec.clock spec in
  let clear = Reg_spec.clear spec in

  let tx = Eth_flow.create_wires () in
  let query = Table.QueryPort.create_wires () in

  let rx_i, rx_o = Eth_flow.if_of_t rx in
  let tx_i, tx_o = Eth_flow.if_of_t tx in
  let query_i, query_o = Table.QueryPort.if_of_t query in

  let i = {I.clock; clear; rx = rx_i; tx = tx_o; query = query_i} in
  let o = {O.rx = rx_o; tx = tx_i; query = query_o} in

  let create_fn (scope : Scope.t) (i : Signal.t I.t) = 
    let o = O.Of_signal.wires () in create_from_if scope i o; o
  in

  let o2 = H.hierarchical ~scope ~name:"arp" create_fn i in
  O.Of_signal.assign o o2;

  tx, query
