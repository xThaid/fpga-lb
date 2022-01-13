open Base
open Hardcaml

module Eth_flow = Transaction.With_flow(Common.EthernetHeader)
module EthArpTst = Transaction.Of_pair(Common.EthernetHeader)(Common.ArpPacket)

module Table = struct
  module ReadPort = struct
    module I = struct
      type 'a t =
        { req_valid : 'a
        ; ip : 'a [@bits 32]
        ; resp_ready : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end
    
    module O = struct
      type 'a t =
        { req_ready : 'a
        ; resp_valid : 'a
        ; mac : 'a [@bits 48]
        ; error : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end 
  
    type 'a t = 
      { i : 'a I.t
      ; o : 'a O.t
      }
  
    let t_of_if (i : Signal.t I.t) (o : Signal.t O.t) = {i; o}
    let if_of_t (t : Signal.t t) = t.i, t.o
  end
  
  module WritePort = struct
    module I = struct
      type 'a t =
        { valid : 'a
        ; ip : 'a [@bits 32]
        ; mac : 'a [@bits 48]
        }
      [@@deriving sexp_of, hardcaml]
    end
  
    type 'a t =
      { i : 'a I.t
      }
  
    let t_of_if (i : Signal.t I.t) = {i}
    let if_of_t (t : Signal.t t) = t.i
    let create_wires () = t_of_if (I.Of_signal.wires ())
  end
  
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; query : 'a ReadPort.I.t 
      ; write : 'a WritePort.I.t
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:true]
  end
  
  module O = struct
    type 'a t = 
      { query : 'a ReadPort.O.t
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:true]
  end
  
  module Entry = struct
    type 'a t =
      { valid : 'a
      ; ip : 'a [@bits 32]
      ; mac : 'a [@bits 48]
      }
    [@@deriving sexp_of, hardcaml]
  
    let len = List.fold Names_and_widths.port_widths ~init:0 ~f:(+)
  end
  
  let write_path scope spec (write_port : Signal.t WritePort.I.t) ram_write_port =
    let open Signal in
    let (--) = Scope.naming scope in
  
    let hash = Hashes.crc32 (module Signal) (ones 32) write_port.ip -- "write_hash" in
  
    let entry = Entry.Of_signal.wires () in
    let entry_d = Entry.Of_signal.reg spec entry in
  
    entry.valid <== vdd;
    entry.ip <== write_port.ip;
    entry.mac <== write_port.mac;
  
    ram_write_port.write_address <== reg spec (sel_bottom hash (width ram_write_port.write_address));
    ram_write_port.write_enable <== reg spec write_port.valid;
    ram_write_port.write_data <== Entry.Of_signal.pack entry_d
  
  let query_path scope spec (read_port_in : Signal.t ReadPort.I.t) ram_read_port ram_read_data = 
    let open Signal in
    let (--) = Scope.naming scope in
  
    let hash = Hashes.crc32 (module Signal) (ones 32) read_port_in.ip -- "query_hash" in
  
    let read_port_out = ReadPort.O.Of_signal.wires () in
  
    let entry = Entry.Of_signal.unpack ram_read_data in
  
    let stored_q_valid = Always.Variable.reg spec ~width:1 in
    let resp_valid = Always.Variable.reg spec ~width:1 in
  
    let stall_second_stage = resp_valid.value &: ((~:) read_port_in.resp_ready) in
    let stall_first_stage = stored_q_valid.value &: stall_second_stage in
  
    let store_query = read_port_in.req_valid &: ((~:) stall_first_stage) in
    let process_query = stored_q_valid.value &: ((~:) stall_second_stage) in
  
    let stored_ip = reg spec ~enable:store_query read_port_in.ip in
    let stored_hash = reg spec ~enable:store_query hash in
  
    let processed_ip = reg spec ~enable:process_query stored_ip in
  
    Always.(compile [
      if_ store_query [
        stored_q_valid <--. 1;
      ] @@ elif process_query [
        stored_q_valid <--. 0;
      ] [];
  
      if_ process_query [
        resp_valid <--. 1;
      ] @@ elif read_port_in.resp_ready [
        resp_valid <--. 0;
      ] [];
    ]);
  
    ram_read_port.read_address <== (sel_bottom stored_hash (width ram_read_port.read_address));
    ram_read_port.read_enable <== process_query;
  
    read_port_out.mac <== entry.mac;
    read_port_out.resp_valid <== resp_valid.value;
    read_port_out.error <== (((~:) entry.valid) |: (entry.ip <>: processed_ip));
  
    read_port_out.req_ready <== ((~:) stall_first_stage);
  
    read_port_out
  
  let create ~capacity (scope : Scope.t) (input : Signal.t I.t) =
    let spec = Reg_spec.create ~clock:input.clock ~clear:input.clear () in
  
    let addr_width = Bits.address_bits_for capacity in
  
    let ram_read_port =
      { Signal.read_clock = input.clock
      ; read_address = Signal.wire addr_width
      ; read_enable = Signal.wire 1
      }
    in
  
    let ram_write_port =
      { Signal.write_clock = input.clock
      ; write_address = Signal.wire addr_width
      ; write_enable = Signal.wire 1
      ; write_data = Signal.wire Entry.len
      }
    in
  
    let ram =
      Ram.create_named
        ~name:(Scope.name scope "arp_table_mem")
        ~collision_mode:Read_before_write
        ~size:capacity
        ~write_ports:[|ram_write_port|]
        ~read_ports:[|ram_read_port|]
        ()
    in
  
    write_path scope spec input.write ram_write_port;
    let read_port_out = query_path scope spec input.query ram_read_port (Array.get ram 0) in
  
    {O.query = read_port_out}
  
  let hierarchical
        ~capacity
        (scope : Scope.t)
        spec
        ~(query_port : Signal.t ReadPort.t)
        ~(write_port : Signal.t WritePort.t) =
    let module H = Hierarchy.In_scope(I)(O) in
  
    let clock = Reg_spec.clock spec in
    let clear = Reg_spec.clear spec in
  
    let qp_i, qp_o = ReadPort.if_of_t query_port in
    let wp_i = WritePort.if_of_t write_port in
  
    let i = {I.clock; clear; query = qp_i; write = wp_i} in
    let o = {O.query = qp_o} in
  
    let o2 = H.hierarchical ~scope ~name:"arp_table" (create ~capacity) i in
    O.Of_signal.assign o o2;
  
  module WriteBusAdapter = struct
    module Agent = Bus.Agent.Make (
      struct
        let addr_len = 2
      end)
  
    let create spec ~(bus : Agent.t) ~(write_port : Signal.t WritePort.I.t) =
      let open Signal in
  
      let mac_lo = Always.Variable.reg ~width:32 spec in
      let mac_hi = Always.Variable.reg ~width:16 spec in
      let ip = Always.Variable.reg ~width:32 spec in
      let ready_next = Always.Variable.wire ~default:gnd in
  
      Always.(compile [
          Agent.on_write bus [
            0, (fun data -> [
              mac_lo <-- data;
            ]);
            
            1, (fun data -> [
              mac_hi <-- sel_bottom data 16;
            ]);
  
            2, (fun data -> [
              ip <-- data;
              ready_next <-- vdd;
            ]);
          ]
      ]);
  
      write_port.ip <== ip.value;
      write_port.mac <== mac_hi.value @: mac_lo.value;
      write_port.valid <== reg spec ready_next.value;
  
      let bus_outs = Agent.outputs bus in
  
      bus_outs.waitrequest <== gnd;
      bus_outs.readdata <== zero 32
  
  end
end

module Config = struct
  let arp_table_capacity = 32
  let mac_addr = "aabbccddeeff"
  let mac_addr_bytes = Signal.of_hex ~width:48 mac_addr
  let ip_addr = [10;100;0;1]
  let ip_addr_bytes = List.map ip_addr ~f:(Signal.of_int ~width:8) |> Signal.concat_msb
end

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; rx : 'a Eth_flow.Src.t
    ; tx : 'a Eth_flow.Dst.t
    ; query : 'a Table.ReadPort.I.t
    }
  [@@deriving sexp_of, hardcaml ~rtlmangle:true]
end

module O = struct
  type 'a t = 
    { rx : 'a Eth_flow.Dst.t 
    ; tx : 'a Eth_flow.Src.t
    ; query : 'a Table.ReadPort.O.t
    }
  [@@deriving sexp_of, hardcaml ~rtlmangle:true]
end

let datapath spec ~(rx : Eth_flow.t) (table_write_port : Signal.t Table.WritePort.t) = 
  let module Serializer = Transaction.Serializer(Common.ArpPacket) in

  let tst_in = EthArpTst.join_comb rx.tst (Serializer.deserialize spec rx.flow) in

  let arp_in_req, arp_in_resp = 
    EthArpTst.demux2_on tst_in ~f:(fun pkt -> Signal.(pkt.snd.oper ==:. 1))
  in

  EthArpTst.apply arp_in_resp ~f:(fun ~valid ~data ->
    let open Signal in

    table_write_port.i.valid <== valid;
    table_write_port.i.ip <== data.snd.spa;
    table_write_port.i.mac <== data.snd.sha;

    Signal.vdd
  );

  let pkt_out = EthArpTst.filter_comb arp_in_req ~f:(fun pkt ->
    let open Signal in
    pkt.snd.tpa ==: Config.ip_addr_bytes
  ) |>

  EthArpTst.map_comb ~f:(fun pkt ->
    let eth =
      { pkt.fst with
        dest_mac = pkt.fst.src_mac
      ; src_mac = Config.mac_addr_bytes
      }
    in
    let arp =
      { pkt.snd with
        oper = Signal.of_int ~width:16 2
      ; sha = Config.mac_addr_bytes
      ; spa = Config.ip_addr_bytes
      ; tha = pkt.snd.sha
      ; tpa = pkt.snd.spa
      }
    in
    EthArpTst.Data.create eth arp
  ) in

  let eth_out, arp_out = EthArpTst.split_comb pkt_out in
  Eth_flow.combine spec eth_out (Serializer.serialize spec arp_out)

let create
      (scope : Scope.t)
      spec
      ~(rx : Eth_flow.t)
      ~(tx : Eth_flow.t)
      ~(query : Signal.t Table.ReadPort.t) =
  
  let table_write_port = Table.WritePort.create_wires () in
  
  Table.hierarchical ~capacity:Config.arp_table_capacity scope spec ~query_port:query ~write_port:table_write_port;
  
  Eth_flow.connect tx (datapath spec ~rx table_write_port)

let create_from_if (scope : Scope.t) (i : Signal.t I.t) (o : Signal.t O.t) =
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in

  let rx = Eth_flow.t_of_if i.rx o.rx in
  let tx = Eth_flow.t_of_if o.tx i.tx in
  let query = Table.ReadPort.t_of_if i.query o.query in

  create scope spec ~rx ~tx ~query

let hierarchical
      (scope : Scope.t)
      spec
      ~(rx : Eth_flow.t)
      ~(tx : Eth_flow.t)
      ~(query : Signal.t Table.ReadPort.t) =
  let module H = Hierarchy.In_scope(I)(O) in

  let clock = Reg_spec.clock spec in
  let clear = Reg_spec.clear spec in

  let rx_i, rx_o = Eth_flow.if_of_t rx in
  let tx_i, tx_o = Eth_flow.if_of_t tx in
  let query_i, query_o = Table.ReadPort.if_of_t query in

  let i = {I.clock; clear; rx = rx_i; tx = tx_o; query = query_i} in
  let o = {O.rx = rx_o; tx = tx_i; query = query_o} in

  let create_fn (scope : Scope.t) (i : Signal.t I.t) = 
    let o = O.Of_signal.wires () in create_from_if scope i o; o
  in

  let o2 = H.hierarchical ~scope ~name:"arp" create_fn i in
  O.Of_signal.assign o o2;
