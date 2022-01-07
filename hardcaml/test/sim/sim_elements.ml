open Base
open Hardcaml
open Lb_dataplane

module FlowEmitter = struct
  type t =
    { enable : bool ref
    ; buff : bytes ref
    ; buff_pos : int ref
    ; transfers : bytes Linked_queue.t
    ; src : Bits.t ref Flow.Source.t
    ; dst : Bits.t ref Flow.Dest.t
    }

  let comb t = 
    let get_nth_byte n = 
      if n >= Bytes.length !(t.buff) then
        Bits.ones 8
      else
        Bytes.get !(t.buff) n |> Bits.of_char
    in

    if !(t.buff_pos) >= Bytes.length !(t.buff) then (
      match Linked_queue.dequeue t.transfers with
      | None -> ()
      | Some b -> t.buff_pos := 0; t.buff := b
    );
    
    let buf_len = Bytes.length !(t.buff) in

    t.src.data := List.init 4 ~f:(fun i -> get_nth_byte (!(t.buff_pos) + i)) |> Bits.concat_msb;
    t.src.valid := Bits.of_bool (!(t.buff_pos) < buf_len && !(t.enable));
    t.src.empty := Bits.of_int ~width:2 (max 0 (4 + !(t.buff_pos) - buf_len));
    t.src.last := Bits.of_bool (4 + !(t.buff_pos) >= buf_len)

  let seq t =
    let buf_len = Bytes.length !(t.buff) in

    if (Bits.is_vdd !(t.dst.ready)) && !(t.buff_pos) < buf_len && !(t.enable) then
      t.buff_pos := !(t.buff_pos) + 4

  let create (src : Bits.t ref Flow.Source.t) (dst : Bits.t ref Flow.Dest.t) =
    {enable = ref false;
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

module FlowConsumer = struct
  type t =
  { enable : bool ref
  ; src : Bits.t ref Flow.Source.t
  ; dst : Bits.t ref Flow.Dest.t
  ; buff : Buffer.t
  ; transfers : bytes list ref
  }

  let comb t = 
    t.dst.ready := Bits.of_bool !(t.enable)

  let seq t =
    if Bits.is_vdd !(t.src.valid) && Bits.is_vdd !(t.dst.ready) then (
      
      let valid_cnt = 4 - Bits.to_int !(t.src.empty) in
      let bytes_to_add = Bits.split_msb ~part_width:8 !(t.src.data) |>
       List.map ~f:(fun x -> Bits.to_char x) |>
       List.filteri ~f:(fun i _-> i < valid_cnt) |>
       Bytes.of_char_list
      in
      
      Buffer.add_bytes t.buff bytes_to_add;

      if Bits.is_vdd !(t.src.last) then (
        t.transfers := (Buffer.contents_bytes t.buff) :: !(t.transfers);
        Buffer.clear t.buff
      )
    )

  let create (src : Bits.t ref Flow.Source.t) (dst : Bits.t ref Flow.Dest.t) =
    {enable = ref false;
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

module BusHost (A : Bus.Agent.S) = struct
  type bus_query =
    | Write of int * int
    | Read of int

  type t =
    { bus_i : Bits.t ref A.O.t
    ; bus_o : Bits.t ref A.I.t
    ; queries : bus_query Linked_queue.t
    ; mutable pending_query : bus_query Option.t
    ; mutable responses : int list
    ; mutable save_resp : bool
    }

  let create bus_i bus_o =
    { bus_i
    ; bus_o
    ; queries = Linked_queue.create ()
    ; pending_query = None
    ; responses = []
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
      t.responses <- Bits.to_int !(t.bus_i.readdata) :: t.responses;
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
    Stdio.print_s [%message (List.rev t.responses : int list)]

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