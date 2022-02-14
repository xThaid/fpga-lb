open Hardcaml

module IPv4_hdr = Transaction.Make(Common.IPv4Header)

module BusAgent = Bus.Agent.Make(struct let addr_len = 2 end)

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; ip_hdr : 'a IPv4_hdr.Src.t
    ; bus : 'a BusAgent.I.t
    }
  [@@deriving sexp_of, hardcaml ~rtlmangle:true]
end

module O = struct
  type 'a t =
    { ip_hdr : 'a IPv4_hdr.Dst.t
    ; bus : 'a BusAgent.O.t
    }
  [@@deriving sexp_of, hardcaml ~rtlmangle:true]
end

module Data = struct
  type 'a t =
    { pkt_cnt : 'a [@bits 32]
    ; bytes_cnt : 'a [@bits 48]
    }
  [@@deriving sexp_of, hardcaml ~rtlmangle:true]
end

let create 
      (_scope : Scope.t)
      spec
      ~(ip_hdr : IPv4_hdr.t)
      =
  let open Signal in

  let stats = Data.Of_always.reg spec in

  IPv4_hdr.apply ip_hdr ~f:(fun ~valid ~data ->
    let valid_d = reg spec valid in
    let pkt_len_d = reg spec data.total_length in

    Always.(compile [
      when_ valid_d [
        stats.pkt_cnt <-- stats.pkt_cnt.value +:. 1;
        stats.bytes_cnt <-- stats.bytes_cnt.value +: (uresize pkt_len_d 48)
      ]
    ]);

    vdd
  );

  let bus = BusAgent.create_wires () in
  let _, bus_o = BusAgent.if_of_t bus in
  BusAgent.read_from spec bus [
    0, stats.pkt_cnt.value;
    1, sel_bottom stats.bytes_cnt.value 32;
    2, uresize (sel_top stats.bytes_cnt.value 16) 32
  ];

  bus_o.waitrequest <== gnd;

  bus

let create_from_if (scope : Scope.t) (i : Signal.t I.t) (o : Signal.t O.t) =
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in

  let ip_hdr = IPv4_hdr.t_of_if i.ip_hdr o.ip_hdr in
  let bus_io = BusAgent.t_of_if i.bus o.bus in

  let bus = create scope spec ~ip_hdr in

  BusAgent.connect bus bus_io

let hierarchical
      (scope : Scope.t)
      spec
      ~(ip_hdr : IPv4_hdr.t)
      =
  let module H = Hierarchy.In_scope(I)(O) in

  let clock = Reg_spec.clock spec in
  let clear = Reg_spec.clear spec in

  let bus = BusAgent.create_wires () in

  let ip_hdr_i, ip_hdr_o = IPv4_hdr.if_of_t ip_hdr in

  let bus_i, bus_o = BusAgent.if_of_t bus in

  let i = {I.clock; clear; ip_hdr = ip_hdr_i; bus = bus_i} in
  let o = {O.ip_hdr = ip_hdr_o; bus = bus_o} in

  let create_fn (scope : Scope.t) (i : Signal.t I.t) = 
    let o = O.Of_signal.wires () in create_from_if scope i o; o
  in

  O.Of_signal.assign o (H.hierarchical ~scope ~name:"pkt_stats" create_fn i);

  bus