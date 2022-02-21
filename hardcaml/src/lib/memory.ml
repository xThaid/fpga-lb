(** Memories wrapped with dataflow interface. *)

open Base
open Hardcaml

module type RamDesc = sig
  val capacity : int
end

(** A simple RAM that is accessed with request & response transactions. *)
module Ram (Desc : RamDesc) (Data : Interface.S) = struct
  let addr_width = Bits.address_bits_for Desc.capacity

  module ReadPort = struct
    module RequestData = struct
      type 'a t =
        { address : 'a [@bits addr_width]
        }
      [@@deriving sexp_of, hardcaml]
    end

    include Transaction.Req_resp(RequestData)(Data)
  end
  
  module WritePort = struct
    module Data = struct
      type 'a t =
        { address : 'a [@bits addr_width]
        ; data : 'a Data.t
        }
      [@@deriving sexp_of, hardcaml ~rtlmangle:true]
    end

    include Transaction.Make(Data)
  end

  let data_entry_len = List.reduce_exn Data.Names_and_widths.port_widths ~f:(+)
  
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; read : 'a ReadPort.Src.t 
      ; write : 'a WritePort.Src.t
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:true]
  end
  
  module O = struct
    type 'a t = 
      { read : 'a ReadPort.Dst.t
      ; write : 'a WritePort.Dst.t
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:true]
  end

  let write_path (write_port : Signal.t WritePort.Src.t) ram_write_port =
    let open Signal in
  
    ram_write_port.write_address <== write_port.data.address;
    ram_write_port.write_enable <== write_port.valid;
    ram_write_port.write_data <== Data.Of_signal.pack write_port.data.data;

    let write_port_out = WritePort.Dst.Of_signal.wires () in
    write_port_out.ready <== vdd;
    write_port_out
  
  let read_path spec (read_port_i : Signal.t ReadPort.Src.t) ram_read_port ram_read_data = 
    let open Signal in
    
    let read_port_o = ReadPort.Dst.Of_signal.wires () in

    let resp_valid = Always.Variable.reg ~width:1 spec in

    let stall = resp_valid.value &: ~:(read_port_i.resp.ready) in
    let process_query = read_port_i.req.valid &: (~:(stall)) in

    ram_read_port.read_address <== read_port_i.req.data.address;
    ram_read_port.read_enable <== process_query;

    Always.(compile [
      if_ process_query [
        resp_valid <--. 1;
      ] @@ elif read_port_i.resp.ready [
        resp_valid <--. 0;
      ] []
    ]);

    read_port_o.req.ready <== ~:(stall);
    read_port_o.resp.valid <== resp_valid.value;
    Data.Of_signal.assign read_port_o.resp.data (Data.Of_signal.unpack ram_read_data);

    read_port_o
  
  let create (scope : Scope.t) (input : Signal.t I.t) =
    let spec = Reg_spec.create ~clock:input.clock ~clear:input.clear () in
  
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
      ; write_data = Signal.wire data_entry_len
      }
    in
  
    let ram =
      Hardcaml.Ram.create
        ~name:(Scope.name scope "ram")
        ~collision_mode:Read_before_write
        ~size:Desc.capacity
        ~write_ports:[|ram_write_port|]
        ~read_ports:[|ram_read_port|]
        ()
    in
  
    let write_port_out = write_path input.write ram_write_port in 
    let read_port_out = read_path spec input.read ram_read_port (Array.get ram 0) in
  
    {O.read = read_port_out; write = write_port_out}
  
  let hierarchical
        ~name
        (scope : Scope.t)
        spec
        ~(read_port : ReadPort.t)
        ~(write_port : WritePort.t) =
    let module H = Hierarchy.In_scope(I)(O) in
  
    let clock = Reg_spec.clock spec in
    let clear = Reg_spec.clear spec in
  
    let rp_i, rp_o = ReadPort.if_of_t read_port in
    let wp_i, wp_o = WritePort.if_of_t write_port in
  
    let i = {I.clock; clear; read = rp_i; write = wp_i} in
    let o = {O.read = rp_o; write = wp_o} in
  
    O.Of_signal.assign o (H.hierarchical ~scope ~name:name create i)

  let data_words_cnt = (data_entry_len + 31) / 32

  module BusAgent = Bus.Agent.Make (
    struct let addr_len = Bits.address_bits_for (data_words_cnt + 1) end
  )

  let create_bus_write_adapter spec (write_port : WritePort.t) =
    let open Signal in

    let bus = BusAgent.create_wires () in
    let _, bus_o = BusAgent.if_of_t bus in

    let valid = Always.Variable.wire ~default:gnd in
    let address = Always.Variable.wire ~default:(zero (Bits.address_bits_for Desc.capacity)) in
    let data_raw = Always.Variable.reg ~width:(data_words_cnt * 32) spec in
  
    Always.(compile [
      BusAgent.on_write bus ([
        0, fun data -> [
          address <-- sel_bottom data (Bits.address_bits_for Desc.capacity);
          valid <-- vdd;
        ]] @
        List.init data_words_cnt ~f:(fun i ->

        let mask = sll (uresize (ones 32) (data_words_cnt * 32)) (i * 32) in 
        i + 1, fun data -> [
          data_raw <-- ((data_raw.value &: ~:(mask)) |: data);
        ])
      )
    ]);

    write_port.s.valid <== reg spec valid.value;
    write_port.s.data.address <== reg spec address.value;
    Data.Of_signal.assign write_port.s.data.data (Data.Of_signal.unpack (sel_bottom data_raw.value data_entry_len));

    bus_o.waitrequest <== gnd;
    bus_o.readdata <== zero 32;

    bus
end

(** Hash table without collision resolution (entries are replaced by new ones). Uses the
    dataflow request-response pattern to access. *)
module Hashtbl (Desc : RamDesc) (Key : Interface.S) (Data : Interface.S) = struct
  module ReadPort = struct
    module ResponseData = struct
      type 'a t =
        { data : 'a Data.t
        ; found : 'a
        }
      [@@deriving sexp_of, hardcaml ~rtlmangle:true]
    end
    
    include Transaction.Req_resp(Key)(ResponseData)
  end
  
  module WritePort = struct
    module Data = struct
      type 'a t =
        { key : 'a Key.t
        ; data : 'a Data.t
        }
      [@@deriving sexp_of, hardcaml ~rtlmangle:true]
    end

    include Transaction.Make(Data)
  end
  
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; query : 'a ReadPort.Src.t 
      ; write : 'a WritePort.Src.t
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:true]
  end
  
  module O = struct
    type 'a t = 
      { query : 'a ReadPort.Dst.t
      ; write : 'a WritePort.Dst.t
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:true]
  end
  
  module Entry = struct
    type 'a t =
      { valid : 'a
      ; key : 'a Key.t
      ; data : 'a Data.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Mem = Ram(Desc)(Entry)

  let calc_hash key = 
    let open Signal in

    Key.Of_signal.pack key |> 
    split_msb ~exact:true ~part_width:32 |>
    List.fold ~init:(ones 32) ~f:(Hashes.crc32 (module Signal))
  
  let write_path scope spec (write_port_in : Signal.t WritePort.Src.t) mem_write_port =
    let open Signal in
    let (--) = Scope.naming scope in
  
    let write_port_out = WritePort.Dst.Of_signal.wires () in
    let write_port = WritePort.t_of_if write_port_in write_port_out in

    Transaction.map (module WritePort) (module Mem.WritePort)
      write_port ~f:(fun data ->
        let hash = calc_hash data.key -- "write_hash" in
        let addr = sel_bottom hash Mem.addr_width in

        let entry = 
          { Entry.valid = vdd
          ; key = data.key
          ; data = data.data
          }
        in

        { Mem.WritePort.Data.address = addr; data = entry }
      ) |>
    Mem.WritePort.pipe_source spec |>
    Mem.WritePort.connect mem_write_port;

    write_port_out
  
  let read_path scope spec (read_port_in : Signal.t ReadPort.Src.t) (mem_read_port : Mem.ReadPort.t) = 
    let open Signal in
    let (--) = Scope.naming scope in

    let read_port_out = ReadPort.Dst.Of_signal.wires () in
    let read_port = ReadPort.t_of_if read_port_in read_port_out in

    let req, req_cpy = ReadPort.Req.fork read_port.req in
    let req_cpy = ReadPort.Req.pipe_source spec req_cpy |> ReadPort.Req.pipe_source spec in

    Transaction.map (module ReadPort.Req) (module Mem.ReadPort.Req)
      req ~f:(fun key ->
        let hash = calc_hash key -- "read_hash" in
        { Mem.ReadPort.RequestData.address = sel_bottom hash Mem.addr_width}
      ) |>
    Mem.ReadPort.Req.pipe spec |>
    Mem.ReadPort.Req.connect mem_read_port.req;

    Transaction.map2 (module ReadPort.Req) (module Mem.ReadPort.Resp) (module ReadPort.Resp)
      req_cpy mem_read_port.resp ~f:(fun req resp ->
        { ReadPort.ResponseData.found = resp.valid &: (Key.Of_signal.pack resp.key ==: Key.Of_signal.pack req)
        ; data = resp.data
        }
      ) |>
    ReadPort.Resp.connect read_port.resp;
  
    read_port_out
  
  let create ~name (scope : Scope.t) (input : Signal.t I.t) =
    let spec = Reg_spec.create ~clock:input.clock ~clear:input.clear () in
    
    let mem_read_port = Mem.ReadPort.create_wires () in
    let mem_write_port = Mem.WritePort.create_wires () in

    Mem.hierarchical ~name:(name ^ "_ram") scope spec ~read_port:mem_read_port ~write_port:mem_write_port;
  
    let write_port_out = write_path scope spec input.write mem_write_port in 
    let read_port_out = read_path scope spec input.query mem_read_port in
  
    {O.query = read_port_out; write = write_port_out}
  
  let hierarchical
        ~name
        (scope : Scope.t)
        spec
        ~(read_port : ReadPort.t)
        ~(write_port : WritePort.t) =
    let module H = Hierarchy.In_scope(I)(O) in
  
    let clock = Reg_spec.clock spec in
    let clear = Reg_spec.clear spec in
  
    let rp_i, rp_o = ReadPort.if_of_t read_port in
    let wp_i, wp_o = WritePort.if_of_t write_port in
  
    let i = {I.clock; clear; query = rp_i; write = wp_i} in
    let o = {O.query = rp_o; write = wp_o} in
  
    let name = (name ^ "_hashtbl") in
    let o2 = H.hierarchical ~scope ~name (create ~name) i in
    O.Of_signal.assign o o2;

end
