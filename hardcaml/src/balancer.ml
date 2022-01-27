open Hardcaml

module IPv4_hdr = Transaction.Make(Common.IPv4Header)
module IPv4_flow = Flow.With_header(Common.IPv4Header)

module Consts = struct
  let max_vips = 8
  let max_reals = 32
  let ring_size = 8
end

module BusAgent = Bus.Agent.Make(struct let addr_len = 3 end)

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; ip_rx : 'a IPv4_flow.Src.t
    ; ip_tx : 'a IPv4_flow.Dst.t
    ; bus : 'a BusAgent.I.t
    }
  [@@deriving sexp_of, hardcaml ~rtlmangle:true]
end

module O = struct
  type 'a t =
    { ip_rx : 'a IPv4_flow.Dst.t
    ; ip_tx : 'a IPv4_flow.Src.t
    ; bus : 'a BusAgent.O.t
    }
  [@@deriving sexp_of, hardcaml ~rtlmangle:true]
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
      { vip_idx : 'a [@bits Bits.address_bits_for Consts.max_vips]
      }
    [@@deriving sexp_of, hardcaml]
  end

  include Container.Hashtbl(Key)(Data)

  module BusAgent = Bus.Agent.Make (
      struct
        let addr_len = 1
      end)

  let create_bus_adapter spec write_port = 
    let open Signal in

    let bus = BusAgent.create_wires () in
    let _, bus_o = BusAgent.if_of_t bus in

    let write = WritePort.I.Of_always.reg spec in
  
    Always.(compile [
      write.valid <-- gnd;
      BusAgent.on_write bus [
        0, (fun data -> [
          write.data.key.vip <-- data;
        ]);
        
        1, (fun data -> [
          write.data.data.vip_idx <-- (sel_bottom data 3);
          write.valid <-- vdd;
        ]);
      ]
    ]);

    WritePort.I.Of_signal.assign (fst (WritePort.if_of_t write_port)) (WritePort.I.Of_always.value write);

    bus_o.waitrequest <== gnd;
    bus_o.readdata <== zero 32;

    bus

end

module L4_hdr_data = struct
  type 'a t =
    { src_port : 'a [@bits 16]
    ; dst_port : 'a [@bits 16]
    ; dummy : 'a [@bits 32]
    }
  [@@deriving sexp_of, hardcaml]
end
module L4_hdr = Transaction.Make(L4_hdr_data)

module HashRingEntry = struct
  type 'a t =
    { real_idx : 'a [@bits Bits.address_bits_for Consts.max_reals]
    }
  [@@deriving sexp_of, hardcaml]
end

module RealInfo = struct
  type 'a t =
    { ip : 'a [@bits 32]
    }
  [@@deriving sexp_of, hardcaml]
end

module HashRings = Container.Ram(struct let capacity = Consts.max_vips * Consts.ring_size end)(HashRingEntry)
module RealsMap = Container.Ram(struct let capacity = Consts.max_reals end)(RealInfo)

module RealLookupReqData = struct
  type 'a t =
    { vip_idx : 'a [@bits Bits.address_bits_for Consts.max_vips]
    ; src_ip : 'a [@bits 32]
    ; src_port : 'a [@bits 16]
    ; dst_port : 'a [@bits 16]
    }
  [@@deriving sexp_of, hardcaml]
end
module RealLookupReq = Transaction.Make(RealLookupReqData)

module RealLookupRespData = struct
  type 'a t =
    { real_ip : 'a [@bits 32]
    }
  [@@deriving sexp_of, hardcaml]
end
module RealLookupResp = Transaction.Make(RealLookupRespData)

let real_lookup (req : RealLookupReq.t) =
  let hash_ring_resp = HashRings.ReadPort.Response.create_wires () in
  let hash_ring_req = Transaction.map (module RealLookupReq) (module HashRings.ReadPort.Request)
    req ~f:(fun req ->
      let open Signal in
      let hash = 
        req.src_ip @: req.src_port @: req.dst_port |>
        split_msb ~exact:true ~part_width:32 |>
        Base.List.fold ~init:(ones 32) ~f:(Hashes.crc32 (module Signal))
      in
      let hash = sel_bottom hash (address_bits_for Consts.ring_size) in
      { HashRings.ReadPort.RequestData.address = concat_msb [req.vip_idx; hash] }
    )
  in
  let hash_ring_read = HashRings.ReadPort.create hash_ring_req hash_ring_resp in

  let real_map_resp = RealsMap.ReadPort.Response.create_wires () in
  let real_map_req = Transaction.map (module HashRings.ReadPort.Response) (module RealsMap.ReadPort.Request)
    hash_ring_resp ~f:(fun resp ->
      { RealsMap.ReadPort.RequestData.address = resp.real_idx }
    )
  in
  let real_map_read = RealsMap.ReadPort.create real_map_req real_map_resp in

  let real_lookup_resp = Transaction.map (module RealsMap.ReadPort.Response) (module RealLookupResp)
    real_map_resp ~f:(fun resp -> { RealLookupRespData.real_ip = resp.ip })
  in

  real_lookup_resp, hash_ring_read, real_map_read

let create
      (scope : Scope.t)
      spec
      ~(ip_rx : IPv4_flow.t) =

  let ip_rx, to_encap_ip_rx = IPv4_flow.fork ip_rx in
  let encap_flow =
    IPv4_flow.to_flow spec to_encap_ip_rx |>
    Flow.Base.pipe_source spec |>
    Flow.Base.pipe_source spec 
  in

  let module L4Serializer = Flow.Serializer(L4_hdr_data) in
  let l4_hdr = L4Serializer.deserialize spec ip_rx.flow in

  let vip_map_query_resp = VIP_map.QueryPort.Response.create_wires () in
  let ip_hdr, vip_map_query_req = Transaction.fork_map (module IPv4_hdr) (module VIP_map.QueryPort.Request)
    ip_rx.hdr ~f:(fun req -> { VIP_map.Key.vip = req.dst_ip }
  ) in
  let vip_map_query = VIP_map.QueryPort.create vip_map_query_req vip_map_query_resp in

  let module PacketInfo = struct
    type 'a t =
      { vip_idx : 'a [@bits Bits.address_bits_for Consts.max_vips]
      ; src_ip : 'a [@bits 32]
      ; src_port : 'a [@bits 16]
      ; dst_port : 'a [@bits 16]
      ; total_len : 'a [@bits 16]
      }
    [@@deriving sexp_of, hardcaml]
    end
  in
  let module PacketInfoTst = Transaction.Make(PacketInfo) in

  let pkt_info = Transaction.map2 (module IPv4_hdr) (module L4_hdr) (module PacketInfoTst)
    (IPv4_hdr.bufferize spec ip_hdr) l4_hdr ~f:(fun ip l4 ->
      { PacketInfo.vip_idx = Signal.zero PacketInfo.port_widths.vip_idx
      ; src_ip = ip.src_ip
      ; src_port = l4.src_port
      ; dst_port = l4.dst_port
      ; total_len = ip.total_length
      }
    )
  in

  let vip_map_query_resp1, vip_map_query_resp2 = VIP_map.QueryPort.Response.fork vip_map_query_resp in

  let module WithVip_mapFlow = Flow.With_header(VIP_map.QueryPort.ResponseData) in
  let encap_flow =
    WithVip_mapFlow.create vip_map_query_resp1 encap_flow |> 
    WithVip_mapFlow.filter spec ~f:(fun resp -> resp.found)
  in

  VIP_map.QueryPort.Response.drop encap_flow.hdr;

  let encap_flow = 
    Flow.Base.pipe_source spec encap_flow.flow |> Flow.Base.pipe_source spec
  in

  let pkt_info = Transaction.filter_map2 (module PacketInfoTst) (module VIP_map.QueryPort.Response) (module PacketInfoTst)
    pkt_info vip_map_query_resp2 ~f:(fun pkt vip_resp ->
      {pkt with vip_idx = vip_resp.data.vip_idx}, vip_resp.found
    )
  in

  let pkt_info, real_lookup_req = Transaction.fork_map (module PacketInfoTst) (module RealLookupReq)
    pkt_info ~f:(fun pkt ->
      { RealLookupReqData.vip_idx = pkt.vip_idx
      ; src_ip = pkt.src_ip
      ; src_port = pkt.src_port
      ; dst_port = pkt.dst_port
      }
    )
  in

  let pkt_info = PacketInfoTst.bufferize spec pkt_info in

  let real_lookup_resp, hash_ring_read, reals_map_read = real_lookup real_lookup_req in

  let outer_ip_hdr = Transaction.map2 (module PacketInfoTst) (module RealLookupResp) (module IPv4_hdr)
    pkt_info real_lookup_resp ~f:(fun pkt resp -> 
      let open Signal in
      { Common.IPv4Header.version = of_int ~width:4 4
      ; ihl = of_int ~width:4 5
      ; dscp = zero 6
      ; ecn = zero 2
      ; total_length = pkt.total_len +:. 20
      ; identification = zero 16
      ; flags = zero 3
      ; fragment_offset = zero 13
      ; ttl = of_int ~width:8 100
      ; protocol = of_int ~width:8 4
      ; hdr_checksum = zero 16
      ; src_ip = of_int ~width:32 0x10101010 (*TODO*)
      ; dst_ip = resp.real_ip
      }
    ) |>
    IPv4_hdr.map_comb ~f:(fun hdr -> {hdr with hdr_checksum = Ip.calc_checksum (module Signal) hdr}) |>
    IPv4_hdr.bufferize spec
  in

  let vip_map_write = VIP_map.WritePort.create_wires () in
  VIP_map.hierarchical ~name:(Scope.name scope "vip_map") ~capacity:Consts.max_vips scope spec ~query_port:vip_map_query ~write_port:vip_map_write;
  let vip_map_bus = VIP_map.create_bus_adapter spec vip_map_write in

  let hash_ring_write = HashRings.WritePort.create_wires () in
  HashRings.hierarchical ~name:(Scope.name scope "hash_ring") scope spec ~read_port:hash_ring_read ~write_port:hash_ring_write;
  let hash_ring_bus = HashRings.create_bus_write_adapter spec hash_ring_write in

  let reals_map_write = RealsMap.WritePort.create_wires () in
  RealsMap.hierarchical ~name:(Scope.name scope "reals_map") scope spec ~read_port:reals_map_read ~write_port:reals_map_write;
  let reals_map_bus = RealsMap.create_bus_write_adapter spec reals_map_write in

  let bus_host = BusAgent.create_wires () in
  let bus_interconnect = Bus.Interconnect.create (Bus.Agent.build (module BusAgent) bus_host) in
  
  Bus.Interconnect.add_agent bus_interconnect (Bus.Agent.build (module VIP_map.BusAgent) vip_map_bus) 0 1;
  Bus.Interconnect.add_agent bus_interconnect (Bus.Agent.build (module HashRings.BusAgent) hash_ring_bus) 2 3;
  Bus.Interconnect.add_agent bus_interconnect (Bus.Agent.build (module RealsMap.BusAgent) reals_map_bus) 4 5;

  Bus.Interconnect.complete_comb bus_interconnect spec;
  
  (IPv4_flow.create outer_ip_hdr encap_flow), bus_host

let create_from_if (scope : Scope.t) (i : Signal.t I.t) (o : Signal.t O.t) =
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in

  let ip_rx = IPv4_flow.t_of_if i.ip_rx o.ip_rx in
  let ip_tx_io = IPv4_flow.t_of_if o.ip_tx i.ip_tx in

  let bus_io = BusAgent.t_of_if i.bus o.bus in

  let ip_tx, bus = create scope spec ~ip_rx in

  IPv4_flow.connect ip_tx_io ip_tx;
  BusAgent.connect bus bus_io

let hierarchical
      (scope : Scope.t)
      spec
      ~(ip_rx : IPv4_flow.t)
      =
  let module H = Hierarchy.In_scope(I)(O) in

  let clock = Reg_spec.clock spec in
  let clear = Reg_spec.clear spec in

  let ip_tx = IPv4_flow.create_wires () in
  let bus = BusAgent.create_wires () in

  let ip_rx_i, ip_rx_o = IPv4_flow.if_of_t ip_rx in
  let ip_tx_i, ip_tx_o = IPv4_flow.if_of_t ip_tx in

  let bus_i, bus_o = BusAgent.if_of_t bus in

  let i = {I.clock; clear; ip_rx = ip_rx_i; ip_tx = ip_tx_o; bus = bus_i} in
  let o = {O.ip_rx = ip_rx_o; ip_tx = ip_tx_i; bus = bus_o} in

  let create_fn (scope : Scope.t) (i : Signal.t I.t) = 
    let o = O.Of_signal.wires () in create_from_if scope i o; o
  in

  O.Of_signal.assign o (H.hierarchical ~scope ~name:"balancer" create_fn i);

  ip_tx, bus
