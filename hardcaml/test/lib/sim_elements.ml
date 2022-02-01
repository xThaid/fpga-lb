open Base
open Hardcaml
open Lb_dataplane

module FlowEmitter = struct
  type t =
    { mutable enabled : bool
    ; buff : bytes ref
    ; buff_pos : int ref
    ; transfers : bytes Linked_queue.t
    ; src : Bits.t ref Flow.Base.Src.t
    ; dst : Bits.t ref Flow.Base.Dst.t
    }

  let transfer_done t =
    !(t.buff_pos) >= Bytes.length !(t.buff)

  let comb t = 
    let get_nth_byte n = 
      if n >= Bytes.length !(t.buff) then
        Bits.ones 8
      else
        Bytes.get !(t.buff) n |> Bits.of_char
    in

    if transfer_done t then (
      match Linked_queue.dequeue t.transfers with
      | None -> ()
      | Some b -> t.buff_pos := 0; t.buff := b
    );
    
    let buf_len = Bytes.length !(t.buff) in

    t.src.data.data := List.init 4 ~f:(fun i -> get_nth_byte (!(t.buff_pos) + i)) |> Bits.concat_msb;
    t.src.valid := Bits.of_bool (not (transfer_done t) && t.enabled);
    t.src.data.empty := Bits.of_int ~width:2 (max 0 (4 + !(t.buff_pos) - buf_len));
    t.src.data.last := Bits.of_bool (4 + !(t.buff_pos) >= buf_len)

  let seq t =
    if (Bits.is_vdd !(t.dst.ready)) && not (transfer_done t) && t.enabled then
      t.buff_pos := !(t.buff_pos) + 4

  let create (src : Bits.t ref Flow.Base.Src.t) (dst : Bits.t ref Flow.Base.Dst.t) =
    {enabled = false;
     buff = ref (Bytes.create 0);
     buff_pos = ref 0;
     transfers = Linked_queue.create ();
     src;
     dst;
    }

  let add_transfer t b =
    Linked_queue.enqueue t.transfers b

  let gen_seq_transfer ?(from = 1) n =
    List.init n ~f:(fun i -> Char.unsafe_of_int (i + from)) |> Bytes.of_char_list

end

module AvalonFlowEmitter = struct
  type t =
    { mutable enabled : bool
    ; mutable buff : bytes
    ; mutable buff_pos : int
    ; transfers : bytes Linked_queue.t
    ; i : Bits.t ref Flow.AvalonST.I.t
    ; o : Bits.t ref Flow.AvalonST.O.t
    }

  let transfer_done t =
    t.buff_pos >= Bytes.length t.buff

  let comb t = 
    let get_nth_byte n = 
      if n >= Bytes.length t.buff then
        Bits.ones 8
      else
        Bytes.get t.buff n |> Bits.of_char
    in

    if transfer_done t then (
      match Linked_queue.dequeue t.transfers with
      | None -> ()
      | Some b -> t.buff_pos <- 0; t.buff <- b
    );
    
    let buf_len = Bytes.length t.buff in

    t.i.data := List.init 4 ~f:(fun i -> get_nth_byte (t.buff_pos + i)) |> Bits.concat_msb;
    t.i.valid := Bits.of_bool (not (transfer_done t) && t.enabled);
    t.i.empty := Bits.of_int ~width:2 (max 0 (4 + t.buff_pos - buf_len));
    t.i.endofpacket := Bits.of_bool (4 + t.buff_pos >= buf_len);
    t.i.startofpacket := Bits.of_bool (t.buff_pos = 0)

  let seq t =
    if (Bits.is_vdd !(t.o.ready) && not (transfer_done t) && t.enabled) then
      t.buff_pos <- t.buff_pos + 4

  let create (i : Bits.t ref Flow.AvalonST.I.t) (o : Bits.t ref Flow.AvalonST.O.t) =
    { enabled = false
    ; buff = Bytes.create 0
    ; buff_pos = 0
    ; transfers = Linked_queue.create ()
    ; i
    ; o
    }

  let add_transfer t b =
    Linked_queue.enqueue t.transfers b

end

module FlowConsumer = struct
  type t =
  { mutable enabled : bool
  ; src : Bits.t ref Flow.Base.Src.t
  ; dst : Bits.t ref Flow.Base.Dst.t
  ; buff : Buffer.t
  ; transfers : bytes list ref
  }

  let comb t = 
    t.dst.ready := Bits.of_bool t.enabled

  let seq t =
    if Bits.is_vdd !(t.src.valid) && Bits.is_vdd !(t.dst.ready) then (
      
      let valid_cnt = 4 - Bits.to_int !(t.src.data.empty) in
      let bytes_to_add = Bits.split_msb ~part_width:8 !(t.src.data.data) |>
       List.map ~f:(fun x -> Bits.to_char x) |>
       List.filteri ~f:(fun i _-> i < valid_cnt) |>
       Bytes.of_char_list
      in
      
      Buffer.add_bytes t.buff bytes_to_add;

      if Bits.is_vdd !(t.src.data.last) then (
        t.transfers := (Buffer.contents_bytes t.buff) :: !(t.transfers);
        Buffer.clear t.buff
      )
    )

  let create (src : Bits.t ref Flow.Base.Src.t) (dst : Bits.t ref Flow.Base.Dst.t) =
    {enabled = false;
     src;
     dst;
     buff = Buffer.create 32;
     transfers = ref []
    }

   let reset t =
    Buffer.clear t.buff;
    t.transfers := []

  let expect_data t =
    let print_transfer b =
      let octets = Bytes.to_list b |> List.map ~f:(fun c -> Printf.sprintf "%02x" (Char.to_int c)) in
      let words = List.chunks_of ~length:4 octets |> List.map ~f:String.concat in
      let lines = List.chunks_of ~length:4 words |> List.map ~f:(String.concat ~sep:" ") in
      
      let hexdump = String.concat ~sep:"\n" lines in
      
      Stdio.print_endline (hexdump ^ "\n");
    in

    List.rev !(t.transfers) |> List.iter ~f:print_transfer

end

module AvalonFlowConsumer = struct
  type t =
  { mutable enabled : bool
  ; i : Bits.t ref Flow.AvalonST.I.t
  ; o : Bits.t ref Flow.AvalonST.O.t
  ; buff : Buffer.t
  ; mutable transfers : bytes Linked_queue.t
  }

  let comb t = 
    t.o.ready := Bits.of_bool t.enabled

  let seq t =
    if Bits.is_vdd !(t.i.valid) && Bits.is_vdd !(t.o.ready) then (
      
      let valid_cnt = 4 - Bits.to_int !(t.i.empty) in
      let bytes_to_add = Bits.split_msb ~part_width:8 !(t.i.data) |>
       List.map ~f:(fun x -> Bits.to_char x) |>
       List.filteri ~f:(fun i _-> i < valid_cnt) |>
       Bytes.of_char_list
      in
      
      Buffer.add_bytes t.buff bytes_to_add;

      if Bits.is_vdd !(t.i.endofpacket) then (
        Linked_queue.enqueue t.transfers (Buffer.contents_bytes t.buff);
        Buffer.clear t.buff
      )
    )

  let create (i : Bits.t ref Flow.AvalonST.I.t) (o : Bits.t ref Flow.AvalonST.O.t) =
    {enabled = false;
     i;
     o;
     buff = Buffer.create 32;
     transfers = Linked_queue.create ()
    }

  let take_transfer t =
    Linked_queue.dequeue t.transfers

  let expect_data t =
    let print_transfer b =
      let octets = Bytes.to_list b |> List.map ~f:(fun c -> Printf.sprintf "%02x" (Char.to_int c)) in
      let words = List.chunks_of ~length:4 octets |> List.map ~f:String.concat in
      let lines = List.chunks_of ~length:4 words |> List.map ~f:(String.concat ~sep:" ") in
      
      let hexdump = String.concat ~sep:"\n" lines in
      
      Stdio.print_endline (hexdump ^ "\n");
    in

    Linked_queue.to_list t.transfers |> List.iter ~f:print_transfer

  let expect_data_digest t =
    let transfers_raw = 
      Linked_queue.to_list t.transfers |>
      List.concat_map ~f:(fun transfer -> Bytes.to_list transfer) |>
      Bytes.of_char_list
    in
    let digest = Md5_lib.to_hex (Md5_lib.bytes transfers_raw) in
    Stdio.print_s [%message (digest : string)]

end

module BusHost (A : Bus.Agent.S) = struct
  type bus_query =
    | Write of int * int
    | Read of int

  type t =
    { bus_i : Bits.t ref A.O.t
    ; bus_o : Bits.t ref A.I.t
    ; queries : bus_query Linked_queue.t
    ; mutable pending_query : bus_query Option.t
    ; responses : int Linked_queue.t
    ; mutable save_resp : bool
    }

  let create bus_i bus_o =
    { bus_i
    ; bus_o
    ; queries = Linked_queue.create ()
    ; pending_query = None
    ; responses = Linked_queue.create ()
    ; save_resp = false
    }

  let comb t =
    t.bus_o.read := Bits.gnd;
    t.bus_o.write := Bits.gnd;

    match t.pending_query with
    | None -> ()
    | Some (Read addr) -> (
        t.bus_o.address := Bits.of_int ~width:A.addr_len addr;
        t.bus_o.read := Bits.vdd;
    )
    | Some (Write (addr, data)) -> (
      t.bus_o.address := Bits.of_int ~width:A.addr_len addr;
      t.bus_o.write := Bits.vdd;
      t.bus_o.writedata := Bits.of_int ~width:32 data;
    )

  let seq t = 
    if t.save_resp then (
      Linked_queue.enqueue t.responses (Bits.to_int !(t.bus_i.readdata));
      t.save_resp <- false;
    );

    if Bits.is_gnd !(t.bus_i.waitrequest) then (
      (
      match t.pending_query with
      | Some (Read _) -> (
          t.save_resp <- true;
      )
      | _ -> ()
      );

      t.pending_query <- None
    );

  t.pending_query <- if Option.is_none t.pending_query then
      Linked_queue.dequeue t.queries
    else(
      t.pending_query)

  let schedule_read t addr = 
    Linked_queue.enqueue t.queries (Read addr)

  let schedule_write t addr data =
    Linked_queue.enqueue t.queries (Write (addr, data))

  let expect_responses t =
    let responses = Linked_queue.to_list t.responses in
    Stdio.print_s [%message (responses : int list)]

end

module BusAgent (A : Bus.Agent.S) = struct
  type t =
    { mutable enabled : bool
    ; bus_i : Bits.t ref A.I.t
    ; bus_o : Bits.t ref A.O.t
    ; mutable pending_data : int
    ; on_read : int -> int
    ; on_write : int -> int -> unit
    ; mutable reads : int list
    ; mutable writes : (int * int) list
    }

  let create bus_i bus_o ~on_read ~on_write =
    { enabled = true
    ; bus_i
    ; bus_o
    ; pending_data = 0
    ; on_read
    ; on_write
    ; reads = []
    ; writes = []
    }

  let on_read t addr = 
    t.reads <- addr :: t.reads;
    t.on_read addr

  let on_write t addr data =
    t.writes <- (addr, data) :: t.writes;
    t.on_write addr data

  let comb t =
    t.bus_o.waitrequest := Bits.of_bool (not t.enabled);
    t.bus_o.readdata := Bits.of_int ~width:32 t.pending_data
  
  let seq t =
    let addr_int = (Bits.to_int !(t.bus_i.address)) in

    if t.enabled && Bits.to_bool !(t.bus_i.write) then
      on_write t addr_int (Bits.to_int !(t.bus_i.writedata))
    else if t.enabled && Bits.to_bool !(t.bus_i.read) then (
      t.pending_data <- (on_read t addr_int)
    )

  let expect_accesses t =
    Stdio.print_s [%message (List.rev t.reads : int list) (List.rev t.writes : (int * int) list)]

end

module TransactionConsumer (Data : Interface.S) = struct
  module Transaction = Transaction.Make(Data)

  type t =
    { mutable consumed : Bits.t Data.t list 
    ; mutable enabled : bool
    ; tst_s : Bits.t ref Transaction.Src.t
    ; tst_d : Bits.t ref Transaction.Dst.t
    }

  let create tst_s tst_d = 
    { consumed = []
    ; enabled = false
    ; tst_s
    ; tst_d
    }

  let comb t =
    t.tst_d.ready := Bits.of_bool t.enabled

  let seq t =
    if (Bits.to_bool !(t.tst_s.valid)) && t.enabled then
        t.consumed <- (Data.map t.tst_s.data ~f:(fun x -> !x)) :: t.consumed

  let expect_reads t = 
    let consumed =
      List.rev_map t.consumed ~f:(fun x -> Data.map x ~f:(fun x -> Bits.to_constant x |> Constant.to_hex_string ~signedness:Unsigned))
    in
    Stdio.print_s [%message (consumed : string Data.t list)]
end

module TransactionEmitter (Data : Interface.S) = struct
  module Transaction = Transaction.Make(Data)

  type t =
    { transfers : Bits.t Data.t Linked_queue.t
    ; mutable pending_transfer : Bits.t Data.t Option.t
    ; mutable enabled : bool
    ; tst_s : Bits.t ref Transaction.Src.t
    ; tst_d : Bits.t ref Transaction.Dst.t
    }

  let create tst_s tst_d = 
    { transfers = Linked_queue.create ()
    ; pending_transfer = None
    ; enabled = false
    ; tst_s
    ; tst_d
    }

  let transfer_done t =
    Option.is_none t.pending_transfer

  let comb t =
    t.pending_transfer <- if transfer_done t then Linked_queue.dequeue t.transfers
    else (t.pending_transfer);

    t.tst_s.valid := Bits.gnd;

    match t.pending_transfer with
    | None -> ()
    | Some transfer -> (
      t.tst_s.valid := Bits.of_bool t.enabled;
      Data.iter2 t.tst_s.data transfer ~f:(fun a b -> a := b)
    )

  let seq t =
    if t.enabled && (Bits.to_bool !(t.tst_d.ready)) then
      t.pending_transfer <- None

  let add_transfer t b =
    Linked_queue.enqueue t.transfers b

end

module FlowWithHeaderEmitter (Data : Interface.S) = struct
  module Flow = Flow.With_header(Data)
  module TstEmitter = TransactionEmitter(Data)

  type t =
    { mutable enabled : bool
    ; tst_emit : TstEmitter.t
    ; flow_emit : FlowEmitter.t
    }

  let create (src : Bits.t ref Flow.Src.t) (dst : Bits.t ref Flow.Dst.t) = 
    { enabled = false
    ; tst_emit = TstEmitter.create src.hdr dst.hdr
    ; flow_emit = FlowEmitter.create src.flow dst.flow
    }

  let comb t =
    t.tst_emit.enabled <- t.enabled;
    t.flow_emit.enabled <- t.enabled;

    TstEmitter.comb t.tst_emit;
    FlowEmitter.comb t.flow_emit

  let seq t =
    TstEmitter.seq t.tst_emit;
    FlowEmitter.seq t.flow_emit

  let add_transfer t tst_data flow_data =
    TstEmitter.add_transfer t.tst_emit tst_data;
    FlowEmitter.add_transfer t.flow_emit flow_data;

end

module FlowWithHeaderConsumer (Data : Interface.S) = struct
  module Flow = Flow.With_header(Data)
  module TstConsumer = TransactionConsumer(Data)

  type t =
    { mutable enabled : bool
    ; transfers : (Bits.t Data.t * bytes) Linked_queue.t
    ; tst_cons : TstConsumer.t
    ; flow_cons : FlowConsumer.t
    }

  let create (src : Bits.t ref Flow.Src.t) (dst : Bits.t ref Flow.Dst.t) = 
    { enabled = false
    ; transfers = Linked_queue.create ()
    ; tst_cons = TstConsumer.create src.hdr dst.hdr
    ; flow_cons = FlowConsumer.create src.flow dst.flow
    }

  let comb t =
    t.tst_cons.enabled <- t.enabled;
    t.flow_cons.enabled <- t.enabled;

    TstConsumer.comb t.tst_cons;
    FlowConsumer.comb t.flow_cons

  let seq t =
    TstConsumer.seq t.tst_cons;
    FlowConsumer.seq t.flow_cons

  let expect_transfers t =
    TstConsumer.expect_reads t.tst_cons;
    FlowConsumer.expect_data t.flow_cons

end