open Hardcaml

module Eth_flow = Transaction.With_flow(Common.EthernetHeader)

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; rx : 'a Eth_flow.Src.t
    ; tx : 'a Eth_flow.Dst.t
    }
  [@@deriving sexp_of, hardcaml ~rtlmangle:true]
end

module O = struct
  type 'a t =
    { rx : 'a Eth_flow.Dst.t
    ; tx : 'a Eth_flow.Src.t
    }
  [@@deriving sexp_of, hardcaml ~rtlmangle:true]
end

let create
      (_scope : Scope.t)
      _spec
      ~(rx : Eth_flow.t)
      ~(tx : Eth_flow.t) =
  let _, _ = rx, tx in
  ()

let create_from_if (scope : Scope.t) (i : Signal.t I.t) (o : Signal.t O.t) =
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in

  let rx = Eth_flow.t_of_if i.rx o.rx in
  let tx = Eth_flow.t_of_if o.tx i.tx in

  create scope spec ~rx ~tx

let hierarchical
      (scope : Scope.t)
      spec
      ~(rx : Eth_flow.t)
      ~(tx : Eth_flow.t) =
  let module H = Hierarchy.In_scope(I)(O) in

  let clock = Reg_spec.clock spec in
  let clear = Reg_spec.clear spec in

  let rx_i, rx_o = Eth_flow.if_of_t rx in
  let tx_i, tx_o = Eth_flow.if_of_t tx in

  let i = {I.clock; clear; rx = rx_i; tx = tx_o} in
  let o = {O.rx = rx_o; tx = tx_i} in

  let create_fn (scope : Scope.t) (i : Signal.t I.t) = 
    let o = O.Of_signal.wires () in create_from_if scope i o; o
  in

  O.Of_signal.assign o (H.hierarchical ~scope ~name:"ip" create_fn i)
