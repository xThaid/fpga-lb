open Hardcaml

module IPv4_hdr = Transaction.Make(Common.IPv4Header)
module IPv4_flow = Flow.With_header(Common.IPv4Header)

module Consts = struct
  let max_vips = 8
  let max_reals = 32
  let ring_size = 32
end

module VIP_map = struct
  module Key = struct
    type 'a t =
      { vip : 'a [@bits 32]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Data = struct
    type 'a t =
      { vip_idx : 'a [@bits 3]
      }
    [@@deriving sexp_of, hardcaml]
  end

  include Hash_table.Make(Key)(Data)
end

module L4_hdr = struct
  type 'a t =
    { src_port : 'a [@bits 16]
    ; dst_port : 'a [@bits 16]
    ; dummy : 'a [@bits 32]
    }
  [@@deriving sexp_of, hardcaml]
end

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; ip_rx : 'a IPv4_flow.Src.t
    ; ip_tx : 'a IPv4_flow.Dst.t
    }
  [@@deriving sexp_of, hardcaml ~rtlmangle:true]
end

module O = struct
  type 'a t =
    { ip_rx : 'a IPv4_flow.Dst.t
    ; ip_tx : 'a IPv4_flow.Src.t
    }
  [@@deriving sexp_of, hardcaml ~rtlmangle:true]
end

let create
      (scope : Scope.t)
      spec
      ~(ip_rx : IPv4_flow.t) =
  let open Signal in

  let ip_rx = IPv4_flow.apply_names ~prefix:"balancer_in_" ip_rx in
  
  let vip_map_query = VIP_map.QueryPort.create_wires () in
  let vim_map_write = VIP_map.WritePort.create_wires () in
  VIP_map.WritePort.I.Of_signal.assign vim_map_write.s (VIP_map.WritePort.I.map VIP_map.WritePort.I.port_widths ~f:zero);

  VIP_map.hierarchical ~name:"vip_map" ~capacity:Consts.max_vips scope spec ~query_port:vip_map_query ~write_port:vim_map_write;

  let ip_rx, ip_rx_fork = IPv4_flow.fork ip_rx in
  let ip_rx = IPv4_flow.apply_names ~prefix:"forked_" ip_rx in
  let encap_flow = IPv4_flow.to_flow spec ip_rx in
  let encap_flow = Flow.Base.apply_names ~prefix:"encap_" encap_flow in

  let ip_hdr, ip_hdr_fork = IPv4_hdr.fork ip_rx_fork.hdr in
  let ip_hdr = IPv4_hdr.apply_names ~prefix:"ip_hdr_" ip_hdr in

  let module Mapper = Transaction.Mapper(Common.IPv4Header)(VIP_map.Key) in
  VIP_map.QueryPort.Request.connect vip_map_query.req (Mapper.map ip_hdr_fork ~f:(fun hdr -> {VIP_map.Key.vip = hdr.dst_ip}));

  let module L4Serializer = Flow.Serializer(L4_hdr) in
  let module L3L4_hdr = Transaction.Of_pair(Common.IPv4Header)(L4_hdr) in

  let l4_hdr = L4Serializer.deserialize spec ip_rx_fork.flow in
  let l3l4_hdr = L3L4_hdr.join_comb (IPv4_hdr.bufferize spec ip_hdr) l4_hdr in

  let module WithVip_mapResp = Transaction.Of_pair(VIP_map.QueryPort.ResponseData)(L3L4_hdr.Data) in
  let module WithVip_mapFlow = Flow.With_header(WithVip_mapResp.Data) in

  let filtered = 
    WithVip_mapFlow.create (WithVip_mapResp.join_comb vip_map_query.resp l3l4_hdr) encap_flow |> 
    WithVip_mapFlow.filter spec ~f:(fun hdr -> ~:(hdr.fst.found))
  in

  let filtered = WithVip_mapFlow.apply_names ~prefix:"filtered" filtered in

  let module Mapper = Transaction.Mapper(WithVip_mapResp.Data)(Common.IPv4Header) in
  let outer_ip_hdr = Mapper.map filtered.hdr ~f:(fun hdr -> 
    hdr.snd.fst
  ) in

  let outer_ip_hdr = IPv4_hdr.apply_names ~prefix:"outer_" outer_ip_hdr in

  let ip_tx = IPv4_flow.create outer_ip_hdr filtered.flow in
  IPv4_flow.apply_names ~prefix:"balancer_out_" ip_tx

let create_from_if (scope : Scope.t) (i : Signal.t I.t) (o : Signal.t O.t) =
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in

  let ip_rx = IPv4_flow.t_of_if i.ip_rx o.ip_rx in
  let ip_tx = IPv4_flow.t_of_if o.ip_tx i.ip_tx in

  IPv4_flow.connect ip_tx (create scope spec ~ip_rx)

let hierarchical
      (scope : Scope.t)
      spec
      ~(ip_rx : IPv4_flow.t)
      =
  let module H = Hierarchy.In_scope(I)(O) in

  let clock = Reg_spec.clock spec in
  let clear = Reg_spec.clear spec in

  let ip_tx = IPv4_flow.create_wires () in

  let ip_rx_i, ip_rx_o = IPv4_flow.if_of_t ip_rx in
  let ip_tx_i, ip_tx_o = IPv4_flow.if_of_t ip_tx in

  let i = {I.clock; clear; ip_rx = ip_rx_i; ip_tx = ip_tx_o} in
  let o = {O.ip_rx = ip_rx_o; ip_tx = ip_tx_i} in

  let create_fn (scope : Scope.t) (i : Signal.t I.t) = 
    let o = O.Of_signal.wires () in create_from_if scope i o; o
  in

  O.Of_signal.assign o (H.hierarchical ~scope ~name:"balancer" create_fn i);

  ip_tx
