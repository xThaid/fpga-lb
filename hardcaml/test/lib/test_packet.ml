open Base
open Hardcaml
open Lb_dataplane

module Emitter = struct
  type config = 
    { src : Bits.t ref Flow.Source.t
    ; dst : Bits.t ref Flow.Dest.t
    ; len : int
    }

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

  let create (cfg : config) =
    {enable = ref false;
     stream_len = cfg.len;
     stream_pos = ref 0;
     src = cfg.src;
     dst = cfg.dst;
    } 

  let reset t =
    t.stream_pos := 0
end

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
end

let%expect_test "depacketizer_unaligned" =
  let module DepacketizerSim = DepacketizerSim(TestHeader) in
  let module Sim = Sim.Sim(DepacketizerSim) in
  
  let sim = Sim.create ~name:"depacketizer_unaligned" ~gtkwave:false () in

  let inputs = Sim.inputs sim in
  let outputs = Sim.outputs sim in

  let emitter = Emitter.create {src = inputs.source_tx; dst = outputs.source_rx; len = 31} in

  Sim.add_element sim (module Emitter) emitter;

  emitter.enable := false;

  inputs.sink_tx.ready := Bits.gnd;

  Sim.cycle_n sim 2;
  emitter.enable := true;
  Sim.cycle_n sim 2;
  emitter.enable := false;
  Sim.cycle_n sim 2;
  emitter.enable := true;
  Sim.cycle_n sim 3;

  inputs.sink_tx.ready := Bits.vdd;
  Sim.cycle_n sim 3;

  emitter.enable := false;
  Sim.cycle_n sim 1;
  emitter.enable := true;
  Sim.cycle_n sim 1;
  inputs.sink_tx.ready := Bits.gnd;

  Emitter.reset emitter;
  Sim.cycle_n sim 2;
  inputs.sink_tx.ready := Bits.vdd;

  Sim.cycle_n sim 15;

  Sim.expect_waves sim;

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
    34c3ff3a165c09c1a7efa0b4048f9dce|}]


module HeaderDisassembleSim (HeaderData : Interface.S) = struct
  module Header = Packet.Header(TestHeader)
  module Packetizer = Packet.Packetizer(TestHeader)

  module I = struct
    type 'a t =
        { clock : 'a
        ; reset : 'a
        ; sink_tx : 'a Flow.Dest.t [@rtlprefix "sink_"]
        ; header : 'a Header.t [@rtl_prefix "hdr_"]
        }
    [@@deriving sexp_of, hardcaml]
  end
  
  module O = struct
    type 'a t =
      { sink_rx : 'a Flow.Source.t [@rtlprefix "sink_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create_fn (i : Signal.t I.t) : (Signal.t O.t) =
    let spec = Reg_spec.create ~clock:i.clock ~reset:i.reset () in

    let sink_rx = Flow.Source.Of_signal.wires () in

    let sink = Flow.Endpoint.create sink_rx i.sink_tx in

    Flow.Endpoint.connect sink (Packetizer.header_disassemble spec ~hdr:i.header);
    
    {O.sink_rx = sink.src;}

end

let%expect_test "header_disassemble" =
  let module HeaderDisassembleSim = HeaderDisassembleSim(TestHeader) in
  let module Sim = Sim.Sim(HeaderDisassembleSim) in
  
  let sim = Sim.create ~name:"header_disassemble" ~gtkwave:true () in

  let inputs = Sim.inputs sim in

  inputs.header.data.field1 := (Bits.of_hex ~width:48 "f0f1f2f3f4f5");
  inputs.header.data.field2 := (Bits.of_hex ~width:8 "f6");
  inputs.header.data.field3 := (Bits.of_hex ~width:16 "f7f8");

  inputs.sink_tx.ready := Bits.gnd;
  Sim.cycle_n sim 2;
  inputs.sink_tx.ready := Bits.vdd;
  Sim.cycle_n sim 1;
  inputs.header.valid := Bits.vdd;
  Sim.cycle_n sim 1;
  inputs.header.valid := Bits.gnd;
  Sim.cycle_n sim 1;
  inputs.sink_tx.ready := Bits.gnd;
  Sim.cycle_n sim 1;
  inputs.sink_tx.ready := Bits.vdd;
  Sim.cycle_n sim 1;
  inputs.sink_tx.ready := Bits.gnd;
  Sim.cycle_n sim 1;
  inputs.sink_tx.ready := Bits.vdd;

  inputs.header.data.field2 := (Bits.of_hex ~width:8 "a6");
  inputs.header.valid := Bits.vdd;
  Sim.cycle_n sim 1;
  inputs.header.valid := Bits.gnd;

  Sim.cycle_n sim 10;

  Sim.expect_waves sim;

  [%expect {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌──│
    │               ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘  │
    │reset          ││                                                   │
    │               ││───────────────────────────────────────────────────│
    │               ││───────────────────────────────────────────────────│
    │field1         ││ F0F1F2F3F4F5                                      │
    │               ││───────────────────────────────────────────────────│
    │               ││───────────────────────────────────────────────────│
    │field2         ││ F6                                                │
    │               ││───────────────────────────────────────────────────│
    │               ││───────────────────────────────────────────────────│
    │field3         ││ F7F8                                              │
    │               ││───────────────────────────────────────────────────│
    │sink_ready     ││                ┌───────────────────────┐       ┌──│
    │               ││────────────────┘                       └───────┘  │
    │valid          ││                        ┌───────┐                  │
    │               ││────────────────────────┘       └──────────────────│
    │               ││────────────────────────────────┬───────┬──────────│
    └───────────────┘└───────────────────────────────────────────────────┘
    3b8290e81e70b62b635fba12fab45640|}]

(*
module PacketizerSim (HeaderData : Interface.S) = struct
  module Header = Packet.Header(TestHeader)
  module Packetizer = Packet.Packetizer(TestHeader)

  module I = struct
    type 'a t =
        { clock : 'a
        ; reset : 'a
        ; source_tx : 'a Flow.Source.t [@rtlprefix "source_"]
        ; sink_tx : 'a Flow.Dest.t [@rtlprefix "sink_"]
        ; header : 'a Header.t [@rtl_prefix "hdr_"]
        }
    [@@deriving sexp_of, hardcaml]
  end
  
  module O = struct
    type 'a t =
      { source_rx : 'a Flow.Dest.t [@rtlprefix "source_"]
      ; sink_rx : 'a Flow.Source.t [@rtlprefix "sink_"]
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

      Flow.Endpoint.connect sink (Packetizer.create spec ~hdr:i.header ~source);
      
      {O.source_rx = source.dst;
        sink_rx = sink.src;
      }
    in

    let module Sim = Cyclesim.With_interface(I)(O) in
    Sim.create create_fn

end
*)