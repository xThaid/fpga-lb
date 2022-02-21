open Hardcaml

module Eth_flow = Flow.With_header(Common.EthernetHeader)
module Ip_hdr = Transaction.Make(Common.IPv4Header)
module IPv4_flow = Flow.With_header(Common.IPv4Header)

module BusAgent = Bus.Agent.Make(struct let addr_len = 8 end)

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; rx : 'a Flow.AvalonST.I.t
    ; tx : 'a Flow.AvalonST.O.t
    ; bus : 'a BusAgent.I.t
    }
  [@@deriving sexp_of, hardcaml ~rtlmangle:true]
end

module O = struct
  type 'a t = 
  { rx : 'a Flow.AvalonST.O.t
  ; tx : 'a Flow.AvalonST.I.t
  ; bus : 'a BusAgent.O.t
  }
  [@@deriving sexp_of, hardcaml ~rtlmangle:true]
end

module Config = struct
  module ControlFlags = struct
    type 'a t =
      { enabled : 'a [@bits 1]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let ctrl_flags_len = ControlFlags.fold ControlFlags.port_widths ~init:0 ~f:(+)

  module Data = struct
    type 'a t =
      { mac_addr : 'a [@bits 48]

      ; src_ip : 'a [@bits 32]
      ; dst_ip : 'a [@bits 32]
      ; src_port : 'a [@bits 16]
      ; dst_port : 'a [@bits 16]

      ; payload_len : 'a [@bits 16]
      ; tx_period : 'a [@bits 32]
      ; flags : 'a ControlFlags.t
      }
    [@@deriving sexp_of, hardcaml]
  end
  
  module BusAgent = Bus.Agent.Make(struct let addr_len = 3 end)
  
  let create spec = 
    let open Signal in
  
    let bus = BusAgent.create_wires () in
    let _, bus_o = BusAgent.if_of_t bus in
  
    let cfg = Data.Of_always.reg spec in
  
    Always.(compile [
      BusAgent.on_write bus ([
        0, (fun data -> [ cfg.mac_addr <-- sel_top cfg.mac_addr.value 16 @: data]);
        1, (fun data -> [ cfg.mac_addr <-- sel_bottom data 16 @: sel_bottom cfg.mac_addr.value 32]);
        2, (fun data -> [ cfg.src_ip <-- data]);
        3, (fun data -> [ cfg.dst_ip <-- data]);
        4, (fun data -> [ cfg.src_port <-- sel_bottom data 16; cfg.dst_port <-- sel_top data 16]);
        5, (fun data -> [ cfg.payload_len <-- sel_bottom data 16]);
        6, (fun data -> [ cfg.tx_period <-- data]);
        7, (fun data -> [ ControlFlags.Of_always.assign cfg.flags (ControlFlags.Of_signal.unpack ~rev:true (sel_bottom data ctrl_flags_len))])
        ])
    ]);
  
    bus_o.waitrequest <== gnd;
    bus_o.readdata <== zero 32;
  
    Data.Of_always.value cfg, bus
end

let payload_generator spec payload_len = 
  let open Signal in

  let payload_words = srl payload_len 2 in

  let total_pkt_cnt = Always.Variable.reg spec ~width:16 in
  let word_cnt = Always.Variable.reg spec ~width:16 in

  let flow = Flow.Base.create_wires () in

  Always.(compile [
    if_ (Flow.Base.is_fired_last flow) [
      word_cnt <--. 0;
      total_pkt_cnt <-- total_pkt_cnt.value +:. 1;
    ] @@ elif (Flow.Base.is_fired flow) [
      word_cnt <-- word_cnt.value +:. 1;
    ] []
  ]);

  flow.s.data.data <== uresize (mux2 (word_cnt.value ==:. 0) total_pkt_cnt.value word_cnt.value) 32;
  flow.s.data.empty <== zero 2;
  flow.s.data.last <== (word_cnt.value >=: (payload_words -:. 1));
  flow.s.valid <== vdd;

  flow, total_pkt_cnt.value

let load_generator spec (config : Signal.t Config.Data.t) =
  let open Signal in

  let timer_tick = 
    Timer.create 32 spec ~enable:config.flags.enabled ~top:config.tx_period |>
    Timer.Tick.pipe spec
  in

  let tick_to_ip, tick_to_udp = Timer.Tick.fork timer_tick in 

  let module Udp_hdr = Transaction.Make(Common.UDPHeader) in
  let module Udp_flow = Flow.With_header(Common.UDPHeader) in

  let payload_flow, pkt_cnt = payload_generator spec config.payload_len in

  let udp_hdr = Transaction.map (module Timer.Tick) (module Udp_hdr) tick_to_udp
    ~f:(fun _ -> 
      { Common.UDPHeader.src_port = config.src_port +: (uresize (sel_bottom pkt_cnt 8) 16)
      ; dst_port = config.dst_port
      ; length = config.payload_len +:. 8
      ; checksum = zero 16
      }
    )
  in

  let udp_flow = Udp_flow.to_flow spec (Udp_flow.create udp_hdr payload_flow) in

  let ip_hdr = Transaction.map (module Timer.Tick) (module Ip_hdr) tick_to_ip
    ~f:(fun _ -> 
      let open Signal in
      { Common.IPv4Header.version = of_int ~width:4 4
      ; ihl = of_int ~width:4 5
      ; dscp = zero 6
      ; ecn = zero 2
      ; total_length = config.payload_len +:. 28
      ; identification = zero 16
      ; flags = zero 3
      ; fragment_offset = zero 13
      ; ttl = of_int ~width:8 64
      ; protocol = of_int ~width:8 17
      ; hdr_checksum = zero 16
      ; src_ip = config.src_ip
      ; dst_ip = config.dst_ip
      }
      ) |>
    Ip_hdr.pipe spec
  in

  IPv4_flow.create ip_hdr udp_flow

let create
      (scope : Scope.t)
      spec
      ~(rx : Flow.Base.t)
      ~(tx : Flow.Base.t)
      ~(bus : BusAgent.t) =

  let rx_eth_arp = 
    Eth_flow.from_flow spec rx |>
    Eth_flow.filter spec ~f:(fun eth -> Signal.(eth.ether_type ==:. 0x0806)) |>
    Eth_flow.pipe spec
  in

  let config, config_bus = Config.create spec in

  let on_arp_req = Arp.OnArpRequest.create_wires () in
  let arp_tx_eth, arp_query_port = Arp.hierarchical scope spec ~rx:rx_eth_arp ~on_arp_req in
  Arp.OnArpRequest.Resp.connect on_arp_req.resp (Transaction.map (module Arp.OnArpRequest.Req) (module Arp.OnArpRequest.Resp)
    on_arp_req.req ~f:(fun arp_req ->
      Signal.({ Arp.ResponseData.mac = config.mac_addr ; error = arp_req.ip <>: config.src_ip})
    )
  );

  let ip_tx = load_generator spec config in
  let ip_hdr, ip_hdr_for_stats = Ip_hdr.fork ip_tx.hdr in
  let ip_tx = IPv4_flow.create ip_hdr ip_tx.flow in

  let stats = Stat.hierarchical scope spec ~ip_hdr:ip_hdr_for_stats in

  let ip_config = {Ip.Config.mac_addr = config.mac_addr} in
  let ip_eth_rx = Eth_flow.create_empty () in
  let ip_tx_eth, ip_tx_ip = Ip.hierarchical scope spec ~eth_rx:ip_eth_rx ~ip_rx:ip_tx ~arp_query:arp_query_port ~cfg:ip_config in
  IPv4_flow.drop ip_tx_ip;

  let tx_eth = Eth_flow.arbitrate spec [arp_tx_eth; ip_tx_eth] in

  Flow.Base.connect tx (Eth_flow.to_flow spec tx_eth);

  let bus_interconnect = Bus.Interconnect.create (Bus.Agent.build (module BusAgent) bus) in
  Bus.Interconnect.add_agent bus_interconnect (Bus.Agent.build (module Config.BusAgent) config_bus) 0 7;
  Bus.Interconnect.add_agent bus_interconnect (Bus.Agent.build (module Stat.BusAgent) stats) 8 11;
  Bus.Interconnect.complete_comb bus_interconnect spec

let create_from_if (scope : Scope.t) (i : Signal.t I.t) =
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in

  let o = O.Of_signal.wires () in

  let rx = Flow.Base.from_avalonst i.rx o.rx in

  let tx = Flow.Base.create_wires () in
  let tx_i, tx_o = Flow.Base.to_avalonst spec tx in

  let bus = BusAgent.t_of_if i.bus o.bus in

  Flow.AvalonST.I.Of_signal.assign o.tx tx_i;
  Flow.AvalonST.O.Of_signal.assign tx_o i.tx;

  create scope spec ~rx ~tx ~bus;

  o

