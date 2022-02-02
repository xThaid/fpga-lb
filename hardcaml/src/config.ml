open Hardcaml
open Base

module Data = struct
  type 'a t =
    { mac_addr : 'a [@bits 48]
    ; vips : 'a [@bits 32 * 6]
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
      ] @
      List.init 6 ~f:(fun i ->
        let vips = split_msb ~part_width:32 cfg.vips.value in
        2 + i, (fun data -> [ 
          let vips = List.mapi vips ~f:(fun j vip -> if i = j then data else vip) in
          cfg.vips <-- concat_msb vips
        ])
      )
      );
  ]);

  bus_o.waitrequest <== gnd;
  bus_o.readdata <== zero 32;

  Data.Of_always.value cfg, bus
