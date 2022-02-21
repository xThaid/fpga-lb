open Hardcaml

module Eth_hdr = Transaction.Make(Common.EthernetHeader)
module IPv4_hdr = Transaction.Make(Common.IPv4Header)
module Eth_flow = Flow.With_header(Common.EthernetHeader)
module IPv4_flow = Flow.With_header(Common.IPv4Header)

module Config = struct
  type 'a t =
    { mac_addr : 'a [@bits 48]
    }
  [@@deriving sexp_of, hardcaml]
end

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; eth_rx : 'a Eth_flow.Src.t
    ; eth_tx : 'a Eth_flow.Dst.t
    ; ip_rx : 'a IPv4_flow.Src.t
    ; ip_tx : 'a IPv4_flow.Dst.t
    ; arp_query : 'a Arp.Table.ReadPort.Dst.t
    ; config : 'a Config.t
    }
  [@@deriving sexp_of, hardcaml ~rtlmangle:true]
end

module O = struct
  type 'a t =
    { eth_rx : 'a Eth_flow.Dst.t
    ; eth_tx : 'a Eth_flow.Src.t
    ; ip_rx : 'a IPv4_flow.Dst.t
    ; ip_tx : 'a IPv4_flow.Src.t
    ; arp_query : 'a Arp.Table.ReadPort.Src.t
    }
  [@@deriving sexp_of, hardcaml ~rtlmangle:true]
end

let calc_checksum (type a) (module B : Comb.S with type t = a) ipv4_hdr =
  let module IPv4_comb = Common.IPv4Header.Make_comb(B) in
  Hashes.one_complement_sum (module B) (IPv4_comb.pack ~rev:true ipv4_hdr)

let pipline_checksum_calculation spec (ipv4_hdr : IPv4_hdr.t) = 
  let open Signal in

  let ipv4_hdr, ipv4_hdr_cpy = IPv4_hdr.fork ipv4_hdr in
  let ipv4_hdr_cpy = IPv4_hdr.pipe_source spec ipv4_hdr_cpy in

  let module Checksum = struct
    type 'a t =
      { checksum : 'a [@bits 16]
      }
    [@@deriving sexp_of, hardcaml]
  end in
  let module ChecksumTst = Transaction.Make(Checksum) in
  let checksum_out = ChecksumTst.create_wires () in

  IPv4_hdr.apply ipv4_hdr ~f:(fun ~valid ~data -> 
    let packed = Common.IPv4Header.Of_signal.pack ~rev:true data in
    let checksum = Hashes.one_complement_sum_pipeline spec ~data:packed ~enable:valid in

    checksum_out.s.valid <== checksum.valid;
    checksum_out.s.data.checksum <== checksum.value;

    checksum_out.d.ready
  );
  
  Transaction.map2 (module IPv4_hdr) (module ChecksumTst) (module IPv4_hdr) ipv4_hdr_cpy (ChecksumTst.pipe_source spec checksum_out)
    ~f:(fun hdr chksum -> {hdr with hdr_checksum = chksum.checksum}) |>
  IPv4_hdr.pipe spec

let egress spec ~(ip_rx : IPv4_flow.t) ~(arp_query : Arp.Table.ReadPort.t) ~(cfg : Signal.t Config.t) =
  let open Signal in

  let ip_hdr, arp_query_req = Transaction.fork_map (module IPv4_hdr) (module Arp.Table.ReadPort.Req) ip_rx.hdr
    ~f:(fun hdr -> {Arp.Table.Key.ip = hdr.dst_ip})
  in
  Arp.Table.ReadPort.Req.connect arp_query.req arp_query_req;

  let ip_flow = 
    Flow.Base.pipe_source spec ip_rx.flow |>
    Flow.Base.pipe_source spec |>
    Flow.Base.pipe spec
  in

  let ip_hdr = 
    IPv4_hdr.map ip_hdr ~f:(fun data -> { data with hdr_checksum = zero 16}) |>
    pipline_checksum_calculation spec
  in

  let module WithArpResp = Transaction.Of_pair(Arp.Table.ReadPort.ResponseData)(Common.IPv4Header) in
  let module WithArpRespFlow = Flow.With_header(WithArpResp.Data) in

  let flow_with_arp_resp = 
    WithArpRespFlow.create (WithArpResp.join arp_query.resp ip_hdr) ip_flow |> 
    WithArpRespFlow.filter spec ~f:(fun hdr -> hdr.fst.found)
  in

  let with_arp_resp, eth_hdr = 
    Transaction.fork_map (module WithArpResp) (module Eth_hdr) flow_with_arp_resp.hdr ~f:(fun data ->
      { Common.EthernetHeader.dest_mac = data.fst.data.mac
      ; src_mac = cfg.mac_addr
      ; ether_type = of_int ~width:16 0x0800
      }
    )
  in

  let ip_tx = IPv4_flow.create (WithArpResp.snd with_arp_resp) flow_with_arp_resp.flow in

  let flow_out = IPv4_flow.to_flow spec ip_tx in
  Eth_flow.create (Eth_hdr.pipe spec eth_hdr) flow_out

let create
      (_scope : Scope.t)
      spec
      ~(eth_rx : Eth_flow.t)
      ~(ip_rx : IPv4_flow.t) 
      ~(arp_query : Arp.Table.ReadPort.t) 
      ~(cfg : Signal.t Config.t) =

  let eth_tx = egress spec ~ip_rx ~arp_query ~cfg in

  let ip_tx = IPv4_flow.from_flow spec eth_rx.flow in
  Eth_hdr.drop eth_rx.hdr;

  eth_tx, ip_tx

let create_from_if (scope : Scope.t) (i : Signal.t I.t) (o : Signal.t O.t) =
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in

  let eth_rx = Eth_flow.t_of_if i.eth_rx o.eth_rx in
  let eth_tx = Eth_flow.t_of_if o.eth_tx i.eth_tx in
  let ip_rx = IPv4_flow.t_of_if i.ip_rx o.ip_rx in
  let ip_tx = IPv4_flow.t_of_if o.ip_tx i.ip_tx in
  let arp_query = Arp.Table.ReadPort.t_of_if o.arp_query i.arp_query in

  let eth_tx2, ip_tx2 = create scope spec ~eth_rx ~ip_rx ~arp_query ~cfg:i.config in

  Eth_flow.connect eth_tx eth_tx2;
  IPv4_flow.connect ip_tx ip_tx2

let hierarchical
      (scope : Scope.t)
      spec
      ~(eth_rx : Eth_flow.t)
      ~(ip_rx : IPv4_flow.t) 
      ~(arp_query : Arp.Table.ReadPort.t)
      ~(cfg : Signal.t Config.t)
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

  let i = {I.clock; clear; eth_rx = eth_rx_i; eth_tx = eth_tx_o; ip_rx = ip_rx_i; ip_tx = ip_tx_o; arp_query = arp_query_o; config = cfg} in
  let o = {O.eth_rx = eth_rx_o; eth_tx = eth_tx_i; ip_rx = ip_rx_o; ip_tx = ip_tx_i; arp_query = arp_query_i} in

  let create_fn (scope : Scope.t) (i : Signal.t I.t) = 
    let o = O.Of_signal.wires () in create_from_if scope i o; o
  in

  O.Of_signal.assign o (H.hierarchical ~scope ~name:"ip" create_fn i);

  eth_tx, ip_tx
