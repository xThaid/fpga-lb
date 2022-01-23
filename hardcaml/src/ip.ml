open Hardcaml

module Eth_hdr = Transaction.Make(Common.EthernetHeader)
module IPv4_hdr = Transaction.Make(Common.IPv4Header)
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
    ; arp_query : 'a Arp.Table.QueryPort.O.t
    }
  [@@deriving sexp_of, hardcaml ~rtlmangle:true]
end

module O = struct
  type 'a t =
    { eth_rx : 'a Eth_flow.Dst.t
    ; eth_tx : 'a Eth_flow.Src.t
    ; ip_rx : 'a IPv4_flow.Dst.t
    ; ip_tx : 'a IPv4_flow.Src.t
    ; arp_query : 'a Arp.Table.QueryPort.I.t
    }
  [@@deriving sexp_of, hardcaml ~rtlmangle:true]
end

let calc_checksum (type a) (module B : Comb.S with type t = a) ipv4_hdr =
  let module IPv4_comb = Common.IPv4Header.Make_comb(B) in
  Hashes.one_complement_sum (module B) (IPv4_comb.pack ~rev:true ipv4_hdr)

let egress spec ~(ip_rx : IPv4_flow.t) ~(arp_query : Arp.Table.QueryPort.t) =
  let open Signal in

  let ip_hdr, ip_hdr2 = IPv4_hdr.fork ip_rx.hdr in

  let module Mapper = Transaction.Mapper(Common.IPv4Header)(Arp.Table.Key) in
  Arp.Table.QueryPort.Request.connect arp_query.req (Mapper.map ip_hdr2 ~f:(fun hdr -> {Arp.Table.Key.ip = hdr.dst_ip}));

  let ip_flow = 
    Flow.Base.pipe_source spec ip_rx.flow |>
    Flow.Base.pipe_source spec
  in

  let ip_hdr = 
    IPv4_hdr.map_comb ip_hdr ~f:(fun data -> { data with hdr_checksum = zero 16}) |> 
    IPv4_hdr.map_comb ~f:(fun data -> { data with hdr_checksum = calc_checksum (module Signal) data}) |>
    IPv4_hdr.pipe_source spec
  in

  let module WithArpResp = Transaction.Of_pair(Arp.Table.QueryPort.ResponseData)(Common.IPv4Header) in
  let module WithArpRespFlow = Flow.With_header(WithArpResp.Data) in

  let with_arp_resp = 
    WithArpRespFlow.create (WithArpResp.join_comb arp_query.resp ip_hdr) ip_flow |> 
    WithArpRespFlow.filter spec ~f:(fun hdr -> ~:(hdr.fst.found))
  in

  let with_arp_resp1, with_arp_resp2 = WithArpResp.fork with_arp_resp.hdr in

  let ip_tx = IPv4_flow.create (WithArpResp.snd with_arp_resp1) with_arp_resp.flow in

  let module Mapper = Transaction.Mapper(WithArpResp.Data)(Common.EthernetHeader) in
  let eth_hdr = 
    Mapper.map with_arp_resp2 ~f:(fun data ->
      { Common.EthernetHeader.dest_mac = data.fst.data.mac
      ; src_mac = of_hex ~width:48 "aabbccddeeff"
      ; ether_type = of_int ~width:16 0x0800
      }
    ) |>
    Eth_hdr.bufferize spec
  in

  let flow_out = IPv4_flow.to_flow spec ip_tx in
  Eth_flow.create eth_hdr flow_out

let create
      (_scope : Scope.t)
      spec
      ~(eth_rx : Eth_flow.t)
      ~(ip_rx : IPv4_flow.t) 
      ~(arp_query : Arp.Table.QueryPort.t) =

  let eth_tx = egress spec ~ip_rx ~arp_query in

  let ip_tx = IPv4_flow.from_flow spec eth_rx.flow in
  Eth_hdr.drop eth_rx.hdr;

  eth_tx, ip_tx

let create_from_if (scope : Scope.t) (i : Signal.t I.t) (o : Signal.t O.t) =
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in

  let eth_rx = Eth_flow.t_of_if i.eth_rx o.eth_rx in
  let eth_tx = Eth_flow.t_of_if o.eth_tx i.eth_tx in
  let ip_rx = IPv4_flow.t_of_if i.ip_rx o.ip_rx in
  let ip_tx = IPv4_flow.t_of_if o.ip_tx i.ip_tx in
  let arp_query = Arp.Table.QueryPort.t_of_if o.arp_query i.arp_query in

  let eth_tx2, ip_tx2 = create scope spec ~eth_rx ~ip_rx ~arp_query in

  Eth_flow.connect eth_tx eth_tx2;
  IPv4_flow.connect ip_tx ip_tx2

let hierarchical
      (scope : Scope.t)
      spec
      ~(eth_rx : Eth_flow.t)
      ~(ip_rx : IPv4_flow.t) 
      ~(arp_query : Arp.Table.QueryPort.t)
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

  let arp_query_i, arp_query_o = Arp.Table.QueryPort.if_of_t arp_query in

  let i = {I.clock; clear; eth_rx = eth_rx_i; eth_tx = eth_tx_o; ip_rx = ip_rx_i; ip_tx = ip_tx_o; arp_query = arp_query_o} in
  let o = {O.eth_rx = eth_rx_o; eth_tx = eth_tx_i; ip_rx = ip_rx_o; ip_tx = ip_tx_i; arp_query = arp_query_i} in

  let create_fn (scope : Scope.t) (i : Signal.t I.t) = 
    let o = O.Of_signal.wires () in create_from_if scope i o; o
  in

  O.Of_signal.assign o (H.hierarchical ~scope ~name:"ip" create_fn i);

  eth_tx, ip_tx
