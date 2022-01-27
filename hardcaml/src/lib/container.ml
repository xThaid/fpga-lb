open Base
open Hardcaml

module type RamDesc = sig
  val capacity : int
end

module Ram (Desc : RamDesc) (Data : Interface.S) = struct
  module ReadPort = struct
    module RequestData = struct
      type 'a t =
        { address : 'a [@bits Bits.address_bits_for Desc.capacity]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module Request = Transaction.Make(RequestData)
    module Response = Transaction.Make(Data)
  
    module I = struct
      type 'a t =
        { req : 'a Request.Src.t
        ; resp : 'a Response.Dst.t
        }
    [@@deriving sexp_of, hardcaml ~rtlmangle:true]
    end
  
    module O = struct
      type 'a t =
        { req : 'a Request.Dst.t
        ; resp : 'a Response.Src.t
        }
    [@@deriving sexp_of, hardcaml ~rtlmangle:true]
    end

    type t = 
      { req : Request.t
      ; resp : Response.t
      }

    let t_of_if (i : Signal.t I.t) (o : Signal.t O.t) =
      let req = Request.t_of_if i.req o.req in
      let resp = Response.t_of_if o.resp i.resp in
      {req; resp}

    let if_of_t (t : t) = 
      let i = {I.req = t.req.s; resp = t.resp.d} in
      let o = {O.req = t.req.d; resp = t.resp.s} in
      i, o

    let create req resp = {req; resp}

    let create_wires () = t_of_if (I.Of_signal.wires ()) (O.Of_signal.wires ())
  end
  
  module WritePort = struct
    module Data = struct
      type 'a t =
        { address : 'a [@bits Bits.address_bits_for Desc.capacity]
        ; data : 'a Data.t
        }
      [@@deriving sexp_of, hardcaml]
    end

    module Request = Transaction.Make(Data)
  
    module I = Request.Src
    module O = Request.Dst

    type t = Request.t
  
    let t_of_if (i : Signal.t I.t) (o : Signal.t O.t) =
      Request.t_of_if i o

    let if_of_t (t : t) = 
      Request.if_of_t t

    let create_wires () =
      Request.create_wires ()

  end

  let data_entry_len = List.reduce_exn Data.Names_and_widths.port_widths ~f:(+)
  
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; read : 'a ReadPort.I.t 
      ; write : 'a WritePort.I.t
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:true]
  end
  
  module O = struct
    type 'a t = 
      { read : 'a ReadPort.O.t
      ; write : 'a WritePort.O.t
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:true]
  end

  let write_path (write_port : Signal.t WritePort.I.t) ram_write_port =
    let open Signal in
  
    ram_write_port.write_address <== write_port.data.address;
    ram_write_port.write_enable <== write_port.valid;
    ram_write_port.write_data <== Data.Of_signal.pack write_port.data.data;

    let write_port_out = WritePort.O.Of_signal.wires () in
    write_port_out.ready <== vdd;
    write_port_out
  
  let read_path spec (read_port_i : Signal.t ReadPort.I.t) ram_read_port ram_read_data = 
    let open Signal in
    
    let read_port_o = ReadPort.O.Of_signal.wires () in

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
  
    let addr_width = Bits.address_bits_for Desc.capacity in
  
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
        ~name:(Scope.name scope "mem")
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

module Hashtbl (Key : Interface.S) (Data : Interface.S) = struct

  module QueryPort = struct

    module ResponseData = struct
      type 'a t =
        { data : 'a Data.t
        ; found : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module Request = Transaction.Make(Key)
    module Response = Transaction.Make(ResponseData)
  
    module I = struct
      type 'a t =
        { req : 'a Request.Src.t
        ; resp : 'a Response.Dst.t
        }
    [@@deriving sexp_of, hardcaml ~rtlmangle:true]
    end
  
    module O = struct
      type 'a t =
        { req : 'a Request.Dst.t
        ; resp : 'a Response.Src.t
        }
    [@@deriving sexp_of, hardcaml ~rtlmangle:true]
    end

    type t = 
      { req : Request.t
      ; resp : Response.t
      }
  
    let t_of_if (i : Signal.t I.t) (o : Signal.t O.t) =
      let req = Request.t_of_if i.req o.req in
      let resp = Response.t_of_if o.resp i.resp in
      {req; resp}

    let if_of_t (t : t) = 
      let i = {I.req = t.req.s; resp = t.resp.d} in
      let o = {O.req = t.req.d; resp = t.resp.s} in
      i, o

    let create req resp = {req; resp}

    let create_wires () =
      t_of_if (I.Of_signal.wires ()) (O.Of_signal.wires ())
  end
  
  module WritePort = struct
    module Data = struct
      type 'a t =
        { key : 'a Key.t
        ; data : 'a Data.t
        }
      [@@deriving sexp_of, hardcaml]
    end

    module Request = Transaction.Make(Data)
  
    module I = Request.Src
    module O = Request.Dst

    type t = Request.t
  
    let t_of_if (i : Signal.t I.t) (o : Signal.t O.t) =
      Request.t_of_if i o

    let if_of_t (t : t) = 
      Request.if_of_t t

    let create_wires () =
      Request.create_wires ()

  end
  
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; query : 'a QueryPort.I.t 
      ; write : 'a WritePort.I.t
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:true]
  end
  
  module O = struct
    type 'a t = 
      { query : 'a QueryPort.O.t
      ; write : 'a WritePort.O.t
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
  
    let len = List.fold Names_and_widths.port_widths ~init:0 ~f:(+)
  end

  let calc_hash key = 
    let open Signal in

    Key.Of_signal.pack key |> 
    split_msb ~exact:true ~part_width:32 |>
    List.fold ~init:(ones 32) ~f:(Hashes.crc32 (module Signal))
  
  let write_path scope spec (write_port : Signal.t WritePort.I.t) ram_write_port =
    let open Signal in
    let (--) = Scope.naming scope in
  
    let hash = calc_hash write_port.data.key -- "write_hash" in
  
    let entry = Entry.Of_signal.wires () in
    let entry_d = Entry.Of_signal.reg spec entry in
  
    entry.valid <== vdd;
    Key.Of_signal.assign entry.key write_port.data.key;
    Data.Of_signal.assign entry.data write_port.data.data;
  
    ram_write_port.write_address <== reg spec (sel_bottom hash (width ram_write_port.write_address));
    ram_write_port.write_enable <== reg spec write_port.valid;
    ram_write_port.write_data <== Entry.Of_signal.pack entry_d;

    let write_port_out = WritePort.O.Of_signal.wires () in
    write_port_out.ready <== vdd;
    write_port_out
  
  let query_path scope spec (read_port_in : Signal.t QueryPort.I.t) ram_read_port ram_read_data = 
    let open Signal in
    let (--) = Scope.naming scope in
  
    let hash = calc_hash read_port_in.req.data -- "query_hash" in
  
    let read_port_out = QueryPort.O.Of_signal.wires () in
  
    let entry = Entry.Of_signal.unpack ram_read_data in
  
    let stored_q_valid = Always.Variable.reg spec ~width:1 in
    let resp_valid = Always.Variable.reg spec ~width:1 in
  
    let stall_second_stage = resp_valid.value &: ((~:) read_port_in.resp.ready) in
    let stall_first_stage = stored_q_valid.value &: stall_second_stage in
  
    let store_query = read_port_in.req.valid &: ((~:) stall_first_stage) in
    let process_query = stored_q_valid.value &: ((~:) stall_second_stage) in
  
    let stored_key = Key.Of_signal.reg spec ~enable:store_query read_port_in.req.data in
    let stored_hash = reg spec ~enable:store_query hash in
  
    let processed_key = Key.Of_signal.reg spec ~enable:process_query stored_key in
  
    Always.(compile [
      if_ store_query [
        stored_q_valid <--. 1;
      ] @@ elif process_query [
        stored_q_valid <--. 0;
      ] [];
  
      if_ process_query [
        resp_valid <--. 1;
      ] @@ elif read_port_in.resp.ready [
        resp_valid <--. 0;
      ] [];
    ]);
  
    ram_read_port.read_address <== (sel_bottom stored_hash (width ram_read_port.read_address));
    ram_read_port.read_enable <== process_query;
  
    Data.Of_signal.assign read_port_out.resp.data.data entry.data;
    read_port_out.resp.valid <== resp_valid.value;

    read_port_out.resp.data.found <== (entry.valid &: (Key.Of_signal.pack entry.key ==: Key.Of_signal.pack processed_key));
  
    read_port_out.req.ready <== ((~:) stall_first_stage);
  
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
      Hardcaml.Ram.create
        ~name:(Scope.name scope "hashtbl_mem")
        ~collision_mode:Read_before_write
        ~size:capacity
        ~write_ports:[|ram_write_port|]
        ~read_ports:[|ram_read_port|]
        ()
    in
  
    let write_port_out = write_path scope spec input.write ram_write_port in 
    let read_port_out = query_path scope spec input.query ram_read_port (Array.get ram 0) in
  
    {O.query = read_port_out; write = write_port_out}
  
  let hierarchical
        ~name
        ~capacity
        (scope : Scope.t)
        spec
        ~(query_port : QueryPort.t)
        ~(write_port : WritePort.t) =
    let module H = Hierarchy.In_scope(I)(O) in
  
    let clock = Reg_spec.clock spec in
    let clear = Reg_spec.clear spec in
  
    let qp_i, qp_o = QueryPort.if_of_t query_port in
    let wp_i, wp_o = WritePort.if_of_t write_port in
  
    let i = {I.clock; clear; query = qp_i; write = wp_i} in
    let o = {O.query = qp_o; write = wp_o} in
  
    let o2 = H.hierarchical ~scope ~name:(name ^ "_hashtbl") (create ~capacity) i in
    O.Of_signal.assign o o2;

end
