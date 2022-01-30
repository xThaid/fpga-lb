open Hardcaml

module Eth_flow = Flow.With_header(Common.EthernetHeader)
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

let create
      (scope : Scope.t)
      spec
      ~(rx : Flow.Base.t)
      ~(tx : Flow.Base.t)
      ~(bus : BusAgent.t) =
  let rx_eth = Eth_flow.from_flow spec rx in

  let eth_flows = Eth_flow.dispatch spec rx_eth ~selector:(fun eth -> 
    let open Signal in
    let sel_arp = eth.ether_type ==:. 0x0806 in
    let sel_ip = eth.ether_type ==:. 0x0800 in

    concat_msb [sel_ip; sel_arp]
  ) in
  let rx_eth_arp = List.nth eth_flows 0 in
  let rx_eth_ip = List.nth eth_flows 1 in

  let arp_tx_eth, arp_query_port = Arp.hierarchical scope spec ~rx:rx_eth_arp in

  let lb_ip_tx = IPv4_flow.create_wires () in

  let ip_tx_eth, ip_tx_ip = Ip.hierarchical scope spec ~eth_rx:rx_eth_ip ~ip_rx:lb_ip_tx ~arp_query:arp_query_port in

  let lb_ip_tx2, balancer_bus = Balancer.hierarchical scope spec ~ip_rx:ip_tx_ip in
  IPv4_flow.connect lb_ip_tx lb_ip_tx2;

  let tx_eth = Eth_flow.arbitrate spec [arp_tx_eth; ip_tx_eth] in

  Flow.Base.connect tx (Eth_flow.to_flow spec tx_eth);

  let bus_interconnect = Bus.Interconnect.create (Bus.Agent.build (module BusAgent) bus) in
  Bus.Interconnect.add_agent bus_interconnect (Bus.Agent.build (module Balancer.BusAgent) balancer_bus) 64 69;
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

