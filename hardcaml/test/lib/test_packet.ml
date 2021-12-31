open Base
open Hardcaml
open Lb_dataplane
open !Expect_test_helpers_base
module Out_channel = Stdio.Out_channel

module TestHeader = struct 
  type 'a t =
    { field1 : 'a[@bits 48]
    ; field2 : 'a[@bits 48]
    ; field3 : 'a[@bits 16]
    }
  [@@deriving sexp_of, hardcaml]
end

module Byte_with_valid = struct
  module Pre = struct
    include With_valid

    let t = 
      { valid = ("valid", 1)
      ; value = ("value", 8)
      }
    ;;
  end

  include Pre
  include Hardcaml.Interface.Make(Pre)
end

module CircuitBuilder (HeaderData : Interface.S) = struct
  module Endpoint = Stream.Endpoint(Interface.Empty)
  module Header = Packet.Header(TestHeader)
  module Depacketizer = Packet.Depacketizer(Interface.Empty)(TestHeader)

  module I = struct
    type 'a t =
       { clock : 'a
       ; reset : 'a
       ; source : 'a Stream.Source.t [@rtlprefix "source_"]
       ; sink : 'a Stream.Dest.t [@rtlprefix "sink_"]
       }
    [@@deriving sexp_of, hardcaml]
  end
  
  module O = struct
    type 'a t =
      { source : 'a Stream.Dest.t [@rtlprefix "source_"]
      ; sink : 'a Stream.Source.t [@rtlprefix "sink_"]
      ; header : 'a Header.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create_depacketizer () = 
    let clock = Signal.input "clock" 1 in
    let reset = Signal.input "reset" 1 in
    let reg_spec = Reg_spec.create ~clock ~reset () in

    let source = Endpoint.create_named ~prefix:"source_" () in
    let sink = Endpoint.create_named ~prefix:"sink_" () in
    
    let hdr = Header.Of_signal.outputs (Depacketizer.create reg_spec sink source) in

    let outputs = Header.to_list hdr @ Endpoint.sink_outputs sink @ Endpoint.source_outputs source in
    Circuit.create_exn ~name:"depacketizer_test" outputs

end

module Emitter = struct
  type t =
    { stream_len : int
    ; stream_pos : int ref
    ; src : Bits.t ref Stream.Source.t
    ; dst : Bits.t ref Stream.Dest.t
    }
    
    let update_wires t = 
      t.src.data := List.init 4 ~f:(fun i -> Bits.of_int ~width:8 (!(t.stream_pos) + i + 1)) |> Bits.concat_msb;
      t.src.valid := Bits.of_bool (!(t.stream_pos) < t.stream_len);
      t.src.empty := Bits.of_int ~width:2 (max 0 (4 + !(t.stream_pos) - t.stream_len));
      t.src.last := Bits.of_bool (4 + !(t.stream_pos) >= t.stream_len)

    let cycle t =
      if Bits.is_vdd !(t.dst.ready) then t.stream_pos := !(t.stream_pos) + 4;
      update_wires t

    let create (src : Bits.t ref Stream.Source.t) (dst : Bits.t ref Stream.Dest.t) len =
      let t = 
        { stream_len = len
        ; stream_pos = ref 1
        ; src
        ; dst
        } 
      in
      update_wires t;
      t
end

let dump_vcd ~filename sim = 
  let chan = Out_channel.create filename in
  let sim = Vcd.Gtkwave.wrap chan sim in
  Out_channel.close chan;
  sim

let () =
  let module CircuitBuilder = CircuitBuilder (TestHeader) in
  let circuit = CircuitBuilder.create_depacketizer () in
  let sim = Cyclesim.create circuit in
  let sim = Vcd.Gtkwave.gtkwave ~args:"--save=/home/thaid/uni/fpga/fpga-lb/hardcaml/test/lib/stream.gtkw" sim in

  let source_valid = Cyclesim.in_port sim "source_valid" in
  let source_data = Cyclesim.in_port sim "source_data" in
  let source_ready = Cyclesim.out_port sim "source_ready" in
  let sink_ready = Cyclesim.in_port sim "sink_ready" in
  
  let source_data_cnt = ref 0 in
  let update_source () = 
    if Bits.is_vdd !source_ready then (
      source_data := Bits.concat_msb [
        Bits.of_int ~width:8 !source_data_cnt;
        Bits.of_int ~width:8 (!source_data_cnt + 1);
        Bits.of_int ~width:8 (!source_data_cnt + 2);
        Bits.of_int ~width:8 (!source_data_cnt + 3)];
      source_data_cnt := !source_data_cnt + 4;
      source_valid := Bits.vdd
    ) else ()
  in

  sink_ready := Bits.vdd;
  Cyclesim.cycle sim;

  for _ = 0 to 10 do
    update_source ();
    Cyclesim.cycle sim
  done;


