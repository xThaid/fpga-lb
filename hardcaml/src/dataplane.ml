open Hardcaml

module Eth_flow = Transaction.With_flow(Common.EthernetHeader)
module IPv4_flow = Transaction.With_flow(Common.IPv4Header)

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; rx : 'a Flow.AvalonST.I.t
    ; tx : 'a Flow.AvalonST.O.t
    }
  [@@deriving sexp_of, hardcaml ~rtlmangle:true]
end

module O = struct
  type 'a t = 
  { rx : 'a Flow.AvalonST.O.t
  ; tx : 'a Flow.AvalonST.I.t
  }
  [@@deriving sexp_of, hardcaml ~rtlmangle:true]
end

let create
      (scope : Scope.t)
      spec
      ~(rx : Flow.t)
      ~(tx : Flow.t) =

  let rx_eth_flow = Eth_flow.from_flow spec rx in

  let arp_query_port = Arp.Table.ReadPort.create_wires () in
  Arp.Table.ReadPort.I.iter2 arp_query_port.i Arp.Table.ReadPort.I.port_widths ~f:(fun p i -> Signal.assign p (Signal.zero i));

  let arp_tx = Eth_flow.create_wires () in
  Arp.hierarchical scope spec ~rx:rx_eth_flow ~tx:arp_tx ~query:arp_query_port;

  Flow.connect tx (Eth_flow.to_flow spec arp_tx)

let create_from_if (scope : Scope.t) (i : Signal.t I.t) =
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in

  let o = O.Of_signal.wires () in

  let rx = Flow.from_avalonst i.rx o.rx in

  let tx = Flow.create_wires () in
  let tx_i, tx_o = Flow.to_avalonst spec tx in

  Flow.AvalonST.I.Of_signal.assign o.tx tx_i;
  Flow.AvalonST.O.Of_signal.assign tx_o i.tx;

  create scope spec ~rx ~tx;

  o

