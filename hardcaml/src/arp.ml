open Hardcaml

module Eth_flow = Flow.With_header(Common.EthernetHeader)
module EthTst = Transaction.Make(Common.EthernetHeader)
module EthArpTst = Transaction.Of_pair(Common.EthernetHeader)(Common.ArpPacket)

module Table = struct
  module Desc = struct
    let capacity = 32
  end

  module Key = struct
    type 'a t =
      { ip : 'a [@bits 32]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Data = struct
    type 'a t =
      { mac : 'a [@bits 48]
      }
    [@@deriving sexp_of, hardcaml]
  end

  include Memory.Hashtbl(Desc)(Key)(Data)
end

module RequestData = struct
  type 'a t = 
    { ip : 'a [@bits 32]
    }
  [@@deriving sexp_of, hardcaml]
end

module ResponseData = struct
  type 'a t = 
    { mac : 'a [@bits 48]
    ; error : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module OnArpRequest = Transaction.Req_resp(RequestData)(ResponseData)

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; rx : 'a Eth_flow.Src.t
    ; tx : 'a Eth_flow.Dst.t
    ; read : 'a Table.ReadPort.Src.t
    ; on_arp_req : 'a OnArpRequest.Dst.t
    }
  [@@deriving sexp_of, hardcaml ~rtlmangle:true]
end

module O = struct
  type 'a t = 
    { rx : 'a Eth_flow.Dst.t 
    ; tx : 'a Eth_flow.Src.t
    ; read : 'a Table.ReadPort.Dst.t
    ; on_arp_req : 'a OnArpRequest.Src.t
    }
  [@@deriving sexp_of, hardcaml ~rtlmangle:true]
end

let datapath spec ~(rx : Eth_flow.t) (table_write_port : Table.WritePort.t) ~(on_arp_req : OnArpRequest.t) = 
  let module Serializer = Flow.Serializer(Common.ArpPacket) in

  let tst_in = EthArpTst.join rx.hdr (Serializer.deserialize spec rx.flow) in

  let arp_in_req, arp_in_resp = 
    EthArpTst.demux2 tst_in ~f:(fun pkt -> Signal.(pkt.snd.oper ==:. 1))
  in

  EthArpTst.apply arp_in_resp ~f:(fun ~valid ~data ->
    let open Signal in

    table_write_port.s.valid <== valid;
    Table.Key.Of_signal.assign table_write_port.s.data.key {Table.Key.ip = data.snd.spa};
    Table.Data.Of_signal.assign table_write_port.s.data.data {Table.Data.mac = data.snd.sha};

    Signal.vdd
  );

  let arp_in_req, on_arp_req_req = Transaction.fork_map (module EthArpTst) (module OnArpRequest.Req)
    arp_in_req ~f:(fun x -> { RequestData.ip = x.snd.tpa })
  in

  let arp_in_req = EthArpTst.pipe_source spec arp_in_req in
  OnArpRequest.Req.connect on_arp_req.req (OnArpRequest.Req.pipe_source spec on_arp_req_req);

  let pkt_out = Transaction.filter_map2 (module EthArpTst) (module OnArpRequest.Resp) (module EthArpTst)
    arp_in_req on_arp_req.resp ~f:(fun pkt resp -> 
      let open Signal in
      let eth =
        { pkt.fst with
          dest_mac = pkt.fst.src_mac
        ; src_mac = resp.mac
        }
      in
      let arp =
        { pkt.snd with
          oper = Signal.of_int ~width:16 2
        ; sha = resp.mac
        ; spa = pkt.snd.tpa
        ; tha = pkt.snd.sha
        ; tpa = pkt.snd.spa
        }
      in
      EthArpTst.Data.create eth arp, ~:(resp.error)
    )
  in

  let eth_out, arp_out = EthArpTst.split pkt_out in
  Eth_flow.create (EthTst.pipe spec eth_out) (Serializer.serialize spec arp_out)

let create
      (scope : Scope.t)
      spec
      ~(rx : Eth_flow.t)
      ~(tx : Eth_flow.t)
      ~(query : Table.ReadPort.t) 
      ~(on_arp_req : OnArpRequest.t)
      =
  
  let table_write_port = Table.WritePort.create_wires () in
  
  Table.hierarchical ~name:"arp_table" scope spec ~read_port:query ~write_port:table_write_port;

  Eth_flow.connect tx (datapath spec ~rx table_write_port ~on_arp_req)

let create_from_if (scope : Scope.t) (i : Signal.t I.t) (o : Signal.t O.t) =
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in

  let rx = Eth_flow.t_of_if i.rx o.rx in
  let tx = Eth_flow.t_of_if o.tx i.tx in
  let query = Table.ReadPort.t_of_if i.read o.read in
  let on_arp_req = OnArpRequest.t_of_if o.on_arp_req i.on_arp_req in

  create scope spec ~rx ~tx ~query ~on_arp_req

let hierarchical
      (scope : Scope.t)
      spec
      ~(rx : Eth_flow.t) 
      ~(on_arp_req : OnArpRequest.t) =
  let module H = Hierarchy.In_scope(I)(O) in

  let clock = Reg_spec.clock spec in
  let clear = Reg_spec.clear spec in

  let tx = Eth_flow.create_wires () in
  let read = Table.ReadPort.create_wires () in

  let rx_i, rx_o = Eth_flow.if_of_t rx in
  let tx_i, tx_o = Eth_flow.if_of_t tx in
  let read_i, read_o = Table.ReadPort.if_of_t read in
  let arp_req_s, arp_req_d = OnArpRequest.if_of_t on_arp_req in

  let i = {I.clock; clear; rx = rx_i; tx = tx_o; read = read_i; on_arp_req = arp_req_d} in
  let o = {O.rx = rx_o; tx = tx_i; read = read_o; on_arp_req = arp_req_s} in

  let create_fn (scope : Scope.t) (i : Signal.t I.t) = 
    let o = O.Of_signal.wires () in create_from_if scope i o; o
  in

  let o2 = H.hierarchical ~scope ~name:"arp" create_fn i in
  O.Of_signal.assign o o2;

  tx, read
