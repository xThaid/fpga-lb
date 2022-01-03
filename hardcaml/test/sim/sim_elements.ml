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