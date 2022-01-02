open Base
open Hardcaml
open Lb_dataplane

module FlowEmitter = struct
  type t =
    { enable : bool ref
    ; stream_len : int ref
    ; stream_pos : int ref
    ; src : Bits.t ref Flow.Source.t
    ; dst : Bits.t ref Flow.Dest.t
    }

  let comb t = 
    t.src.data := List.init 4 ~f:(fun i -> Bits.of_int ~width:8 (!(t.stream_pos) + i + 1)) |> Bits.concat_msb;
    t.src.valid := Bits.of_bool (!(t.stream_pos) < !(t.stream_len) && !(t.enable));
    t.src.empty := Bits.of_int ~width:2 (max 0 (4 + !(t.stream_pos) - !(t.stream_len)));
    t.src.last := Bits.of_bool (4 + !(t.stream_pos) >= !(t.stream_len))

  let seq t =
    if (Bits.is_vdd !(t.dst.ready)) && (!(t.stream_pos) < !(t.stream_len) && !(t.enable)) then t.stream_pos := !(t.stream_pos) + 4

  let create (src : Bits.t ref Flow.Source.t) (dst : Bits.t ref Flow.Dest.t) (len : int) =
    {enable = ref false;
     stream_len = ref len;
     stream_pos = ref 0;
     src;
     dst;
    } 

  let reset t ?(n = 0) () =
    t.stream_pos := 0;
    if n <> 0 then
      t.stream_len := n
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
     buff = Buffer.create 16;
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