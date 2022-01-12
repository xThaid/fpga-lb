open Hardcaml

module Eth_flow = Transaction.With_flow(Common.EthernetHeader)

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; rx : 'a Flow.Source.t
    ; tx : 'a Flow.Dest.t
    }
  [@@deriving sexp_of, hardcaml ~rtlmangle:true]
end

module O = struct
  type 'a t = 
  { rx : 'a Flow.Dest.t
  ; tx : 'a Flow.Source.t
  }
  [@@deriving sexp_of, hardcaml ~rtlmangle:true]
end

let create
      (scope : Scope.t)
      spec
      ~(rx : Flow.t)
      ~(tx : Flow.t) =

  let rx_eth_flow = Eth_flow.from_flow spec rx in

  let arp_query_port = Arp_table.ReadPort.t_of_if 
    (Arp_table.ReadPort.I.Of_signal.wires ()) 
    (Arp_table.ReadPort.O.Of_signal.wires ())
  in
  Arp_table.ReadPort.I.iter2 arp_query_port.i Arp_table.ReadPort.I.port_widths ~f:(fun p i -> Signal.assign p (Signal.zero i));

  let arp_tx = Eth_flow.create_wires () in
  Arp.hierarchical scope spec ~rx:rx_eth_flow ~tx:arp_tx ~query:arp_query_port;

  Flow.connect tx (Eth_flow.to_flow spec arp_tx)

let create_from_if (scope : Scope.t) (i : Signal.t I.t) =
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in

  let o = O.Of_signal.wires () in

  let rx = Flow.t_of_if i.rx o.rx in
  let tx = Flow.t_of_if o.tx i.tx in

  create scope spec ~rx ~tx;

  o

