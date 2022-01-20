open Hardcaml

module Eth_flow = Flow.With_header(Common.EthernetHeader)
module IPv4_flow = Flow.With_header(Common.IPv4Header)

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; eth_rx : 'a Eth_flow.Src.t
    ; eth_tx : 'a Eth_flow.Dst.t
    ; ip_rx : 'a IPv4_flow.Src.t
    ; ip_tx : 'a IPv4_flow.Dst.t
    ; arp_query : 'a Arp.Table.ReadPort.O.t
    }
  [@@deriving sexp_of, hardcaml ~rtlmangle:true]
end

module O = struct
  type 'a t =
    { eth_rx : 'a Eth_flow.Dst.t
    ; eth_tx : 'a Eth_flow.Src.t
    ; ip_rx : 'a IPv4_flow.Dst.t
    ; ip_tx : 'a IPv4_flow.Src.t
    ; arp_query : 'a Arp.Table.ReadPort.I.t
    }
  [@@deriving sexp_of, hardcaml ~rtlmangle:true]
end

let create
      (_scope : Scope.t)
      _spec
      ~(eth_rx : Eth_flow.t)
      ~(ip_rx : IPv4_flow.t) 
      ~(arp_query : Arp.Table.ReadPort.t) =
  let open Signal in
  let _ = arp_query in

  let ip_rx = IPv4_flow.map_hdr ip_rx ~f:(fun ipv4 ->
    { ipv4 with
      ttl = ipv4.ttl -:. 1
    }
  ) in

  Arp.Table.ReadPort.I.iter2 arp_query.i Arp.Table.ReadPort.I.port_widths ~f:(fun p i -> Signal.assign p (Signal.zero i));
  eth_rx, ip_rx

let create_from_if (scope : Scope.t) (i : Signal.t I.t) (o : Signal.t O.t) =
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in

  let eth_rx = Eth_flow.t_of_if i.eth_rx o.eth_rx in
  let eth_tx = Eth_flow.t_of_if o.eth_tx i.eth_tx in
  let ip_rx = IPv4_flow.t_of_if i.ip_rx o.ip_rx in
  let ip_tx = IPv4_flow.t_of_if o.ip_tx i.ip_tx in
  let arp_query = Arp.Table.ReadPort.t_of_if o.arp_query i.arp_query in

  let eth_tx2, ip_tx2 = create scope spec ~eth_rx ~ip_rx ~arp_query in

  Eth_flow.connect eth_tx eth_tx2;
  IPv4_flow.connect ip_tx ip_tx2

let hierarchical
      (scope : Scope.t)
      spec
      ~(eth_rx : Eth_flow.t)
      ~(ip_rx : IPv4_flow.t) 
      ~(arp_query : Arp.Table.ReadPort.t)
      =
  let module H = Hierarchy.In_scope(I)(O) in

  let clock = Reg_spec.clock spec in
  let clear = Reg_spec.clear spec in

  let eth_tx = Eth_flow.create_wires () in
  let ip_tx = IPv4_flow.create_wires () in

  let eth_rx_i, eth_rx_o = Eth_flow.if_of_t eth_rx in
  let eth_tx_i, eth_tx_o = Eth_flow.if_of_t eth_tx in

  let ip_rx_i, ip_rx_o = IPv4_flow.if_of_t ip_rx in
  let ip_tx_i, ip_tx_o = IPv4_flow.if_of_t ip_tx in

  let arp_query_i, arp_query_o = Arp.Table.ReadPort.if_of_t arp_query in

  let i = {I.clock; clear; eth_rx = eth_rx_i; eth_tx = eth_tx_o; ip_rx = ip_rx_i; ip_tx = ip_tx_o; arp_query = arp_query_o} in
  let o = {O.eth_rx = eth_rx_o; eth_tx = eth_tx_i; ip_rx = ip_rx_o; ip_tx = ip_tx_i; arp_query = arp_query_i} in

  let create_fn (scope : Scope.t) (i : Signal.t I.t) = 
    let o = O.Of_signal.wires () in create_from_if scope i o; o
  in

  O.Of_signal.assign o (H.hierarchical ~scope ~name:"ip" create_fn i);

  eth_tx, ip_tx
