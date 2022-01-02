open Base
open Hardcaml
open Lb_dataplane
open Sim_elements

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
  module Packetizer = Packet.Packetizer(TestHeader)

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

    let hdr, sink2 = Packetizer.create_depacketizer spec ~source in

    Flow.Endpoint.connect sink sink2;
    
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

  let emitter = FlowEmitter.create inputs.source_tx outputs.source_rx 31 in
  let consumer = FlowConsumer.create outputs.sink_rx inputs.sink_tx in

  Sim.add_element sim (module FlowEmitter) emitter;
  Sim.add_element sim (module FlowConsumer) consumer;

  emitter.enable := false;

  consumer.enable := false;

  Sim.cycle_n sim 2;
  emitter.enable := true;
  Sim.cycle_n sim 2;
  emitter.enable := false;
  Sim.cycle_n sim 2;
  emitter.enable := true;
  Sim.cycle_n sim 3;

  consumer.enable := true;
  Sim.cycle_n sim 3;

  emitter.enable := false;
  Sim.cycle_n sim 1;
  emitter.enable := true;
  Sim.cycle_n sim 1;
  consumer.enable := false;

  FlowEmitter.reset emitter ();
  Sim.cycle_n sim 2;
  consumer.enable := true;
  Sim.cycle_n sim 7;

  FlowEmitter.reset emitter ~n:29 ();
  Sim.cycle_n sim 9;
  consumer.enable := false;
  FlowEmitter.reset emitter ~n:28 ();
  Sim.cycle_n sim 5;
  consumer.enable := true;

  Sim.cycle_n sim 10;

  FlowConsumer.expect_data consumer;

  Sim.expect_waves sim;

  [%expect {|
    0a0b0c0d 0e0f1011 12131415 16171819
    1a1b1c1d 1e1f

    0a0b0c0d 0e0f1011 12131415 16171819
    1a1b1c1d 1e1f

    0a0b0c0d 0e0f1011 12131415 16171819
    1a1b1c1d

    0a0b0c0d 0e0f1011 12131415 16171819
    1a1b1c

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
    62792abc1d15743bbc6a5a2857ad0bcf|}]


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
  
  let sim = Sim.create ~name:"header_disassemble" ~gtkwave:false () in

  let inputs = Sim.inputs sim in
  let outputs = Sim.outputs sim in

  let consumer = FlowConsumer.create outputs.sink_rx inputs.sink_tx in

  Sim.add_element sim (module FlowConsumer) consumer;

  inputs.header.data.field1 := (Bits.of_hex ~width:48 "f0f1f2f3f4f5");
  inputs.header.data.field2 := (Bits.of_hex ~width:8 "f6");
  inputs.header.data.field3 := (Bits.of_hex ~width:16 "f7f8");

  consumer.enable := false;
  Sim.cycle_n sim 2;
  consumer.enable := true;
  Sim.cycle_n sim 1;
  inputs.header.valid := Bits.vdd;
  Sim.cycle_n sim 1;
  inputs.header.valid := Bits.gnd;
  Sim.cycle_n sim 1;
  consumer.enable := false;
  Sim.cycle_n sim 1;
  consumer.enable := true;
  Sim.cycle_n sim 1;
  consumer.enable := false;
  Sim.cycle_n sim 1;
  consumer.enable := true;

  inputs.header.data.field2 := (Bits.of_hex ~width:8 "a6");
  inputs.header.valid := Bits.vdd;
  Sim.cycle_n sim 1;
  inputs.header.valid := Bits.gnd;

  Sim.cycle_n sim 10;

  FlowConsumer.expect_data consumer;
  Sim.expect_waves sim;

  [%expect {|
    f0f1f2f3 f4f5f6f7 f8

    f0f1f2f3 f4f5a6f7 f8

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

  let create_fn (i : Signal.t I.t) : (Signal.t O.t) =
    let spec = Reg_spec.create ~clock:i.clock ~reset:i.reset () in

    let source_rx = Flow.Dest.Of_signal.wires () in
    let sink_rx = Flow.Source.Of_signal.wires () in

    let sink = Flow.Endpoint.create sink_rx i.sink_tx in
    let source = Flow.Endpoint.create i.source_tx source_rx in

    Flow.Endpoint.connect sink (Packetizer.create_packetizer spec ~hdr:i.header ~source);
    
    {O.source_rx = source.dst;
      sink_rx = sink.src;
    }
end

let%expect_test "packetizer_unaligned" =
  let module PacketizerSim = PacketizerSim(TestHeader) in
  let module Sim = Sim.Sim(PacketizerSim) in
  
  let sim = Sim.create ~name:"packetizer_unaligned" ~gtkwave:false () in

  let inputs = Sim.inputs sim in
  let outputs = Sim.outputs sim in

  let emitter = FlowEmitter.create inputs.source_tx outputs.source_rx 24 in
  let consumer = FlowConsumer.create outputs.sink_rx inputs.sink_tx in

  Sim.add_element sim (module FlowEmitter) emitter;
  Sim.add_element sim (module FlowConsumer) consumer;

  emitter.enable := false;

  inputs.header.data.field1 := (Bits.of_hex ~width:48 "f0f1f2f3f4f5");
  inputs.header.data.field2 := (Bits.of_hex ~width:8 "f6");
  inputs.header.data.field3 := (Bits.of_hex ~width:16 "f7f8");

  Sim.cycle_n sim 2;
  inputs.header.valid := Bits.vdd;
  Sim.cycle_n sim 2;
  consumer.enable := true;
  Sim.cycle_n sim 1;
  emitter.enable := true;
  inputs.header.valid := Bits.gnd;
  Sim.cycle_n sim 7;
  inputs.header.valid := Bits.vdd;
  inputs.header.data.field2 := (Bits.of_hex ~width:8 "a6");

  Sim.cycle_n sim 1;
  inputs.header.valid := Bits.gnd;
  FlowEmitter.reset emitter ();
  Sim.cycle_n sim 1;
  consumer.enable := false;
  Sim.cycle_n sim 1;
  consumer.enable := true;
  Sim.cycle_n sim 1;
  consumer.enable := false;
  Sim.cycle_n sim 2;
  consumer.enable := true;
  Sim.cycle_n sim 1;
  consumer.enable := false;
  Sim.cycle_n sim 1;
  consumer.enable := true;
  Sim.cycle_n sim 5;

  FlowEmitter.reset emitter ();
  emitter.enable := false;
  inputs.header.valid := Bits.vdd;
  Sim.cycle_n sim 6;
  emitter.enable := true;

  Sim.cycle_n sim 15;

  FlowConsumer.expect_data consumer;
  Sim.expect_waves sim;

  [%expect {|
    f0f1f2f3 f4f5f6f7 f8010203 04050607
    08090a0b 0c0d0e0f 10111213 14151617
    18

    f0f1f2f3 f4f5a6f7 f8010203 04050607
    08090a0b 0c0d0e0f 10111213 14151617
    18

    f0f1f2f3 f4f5a6f7 f8010203 04050607
    08090a0b 0c0d0e0f 10111213 14151617
    18

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
    │sink_ready     ││                                ┌──────────────────│
    │               ││────────────────────────────────┘                  │
    │               ││───────────────────────────────────────────────────│
    │source_data    ││ 01020304                                          │
    │               ││───────────────────────────────────────────────────│
    └───────────────┘└───────────────────────────────────────────────────┘
    717a57024c58de79b1eb122d769ec15c|}]
