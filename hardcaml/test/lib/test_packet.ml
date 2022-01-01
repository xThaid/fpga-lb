open Base
open Hardcaml
open Hardcaml_waveterm
open Lb_dataplane
open !Expect_test_helpers_base
module Out_channel = Stdio.Out_channel

module TestHeader = struct 
  type 'a t =
    { field1 : 'a [@bits 48]
    ; field2 : 'a [@bits 8]
    ; field3 : 'a [@bits 16]
    }
  [@@deriving sexp_of, hardcaml]
end

module DepacketizerSim (HeaderData : Interface.S) = struct
  module Header = Packet.Header(TestHeader)
  module Depacketizer = Packet.Depacketizer(TestHeader)

  module I = struct
    type 'a t =
       { clock : 'a
       ; reset : 'a
       ; source_tx : 'a Flow.Source.t [@rtlprefix "source_"]
       ; sink_tx : 'a Flow.Dest.t [@rtlprefix "sink_"]
       }
    [@@deriving sexp_of, hardcaml]
  end
  
  module O = struct
    type 'a t =
      { source_rx : 'a Flow.Dest.t [@rtlprefix "source_"]
      ; sink_rx : 'a Flow.Source.t [@rtlprefix "sink_"]
      ; header : 'a Header.t [@rtl_prefix "hdr_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create () = 
    let create_fn (i : Signal.t I.t) : (Signal.t O.t) =
      let spec = Reg_spec.create ~clock:i.clock ~reset:i.reset () in

      let source_rx = Flow.Dest.Of_signal.wires () in
      let sink_rx = Flow.Source.Of_signal.wires () in

      let sink = Flow.Endpoint.create sink_rx i.sink_tx in
      let source = Flow.Endpoint.create i.source_tx source_rx in

      let hdr = Depacketizer.create spec ~sink ~source in
      
      {O.source_rx = source.dst;
       sink_rx = sink.src;
       header = hdr;
      }
    in

    let module Sim = Cyclesim.With_interface(I)(O) in
    Sim.create create_fn

end

module Emitter = struct
  type t =
    { enable : bool ref
    ; stream_len : int
    ; stream_pos : int ref
    ; src : Bits.t ref Flow.Source.t
    ; dst : Bits.t ref Flow.Dest.t
    }

    let comb t = 
      t.src.data := List.init 4 ~f:(fun i -> Bits.of_int ~width:8 (!(t.stream_pos) + i + 1)) |> Bits.concat_msb;
      t.src.valid := Bits.of_bool (!(t.stream_pos) < t.stream_len && !(t.enable));
      t.src.empty := Bits.of_int ~width:2 (max 0 (4 + !(t.stream_pos) - t.stream_len));
      t.src.last := Bits.of_bool (4 + !(t.stream_pos) >= t.stream_len)

    let seq t =
      if (Bits.is_vdd !(t.dst.ready)) && (!(t.stream_pos) < t.stream_len && !(t.enable)) then t.stream_pos := !(t.stream_pos) + 4

    let create (src : Bits.t ref Flow.Source.t) (dst : Bits.t ref Flow.Dest.t) len =
      let t = 
        { enable = ref false
        ; stream_len = len
        ; stream_pos = ref 0
        ; src
        ; dst
        } 
      in
      t
end

let%expect_test "tmp" =
  let module DepacketizerSim = DepacketizerSim(TestHeader) in
  let sim = DepacketizerSim.create () in
  (* let sim = Vcd.Gtkwave.gtkwave ~args:"--save=/home/thaid/uni/fpga/fpga-lb/hardcaml/test/lib/stream.gtkw" sim in *)
  let waves, sim = Waveform.create sim in

  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in

  let emitter = Emitter.create inputs.source_tx outputs.source_rx 31 in

  let cycle () =
    Cyclesim.cycle_check sim;

    Emitter.comb emitter;
    Cyclesim.cycle_before_clock_edge sim;

    Emitter.seq emitter;
    Cyclesim.cycle_at_clock_edge sim;

    Emitter.comb emitter;
    Cyclesim.cycle_after_clock_edge sim;
  in
  let cycle_n n = 
    for _ = 0 to n - 1 do
      cycle ()
    done;
  in

  emitter.enable := false;

  inputs.sink_tx.ready := Bits.gnd;

  cycle_n 2;
  emitter.enable := true;
  cycle_n 2;
  emitter.enable := false;
  cycle_n 2;
  emitter.enable := true;
  cycle_n 3;

  inputs.sink_tx.ready := Bits.vdd;
  cycle_n 3;

  emitter.enable := false;
  cycle_n 1;
  emitter.enable := true;
  cycle_n 1;
  inputs.sink_tx.ready := Bits.gnd;
  cycle_n 2;
  inputs.sink_tx.ready := Bits.vdd;
  cycle_n 4;

  Hardcaml_waveterm.Waveform.expect ~serialize_to:"depacketizer1" waves;

  [%expect {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌──│
    │               ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘  │
    │reset          ││                                                   │
    │               ││───────────────────────────────────────────────────│
    │sink_ready     ││                                                   │
    │               ││───────────────────────────────────────────────────│
    │               ││────────────────────────┬───────┬──────────────────│
    │source_data    ││ 01020304               │050607.│090A0B0C          │
    │               ││────────────────────────┴───────┴──────────────────│
    │               ││───────────────────────────────────────────────────│
    │source_empty   ││ 0                                                 │
    │               ││───────────────────────────────────────────────────│
    │source_last    ││                                                   │
    │               ││───────────────────────────────────────────────────│
    │source_valid   ││                ┌───────────────┐               ┌──│
    │               ││────────────────┘               └───────────────┘  │
    │               ││────────────────────────────────┬──────────────────│
    │field1         ││ 000000000000                   │000000000102      │
    └───────────────┘└───────────────────────────────────────────────────┘
    8cbe57f9c21f50eb17e2698bc06c2245|}]