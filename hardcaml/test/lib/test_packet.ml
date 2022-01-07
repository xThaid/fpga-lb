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

module PacketizerFullSim (HeaderData : Interface.S) = struct
  module Packetizer = Packet.Packetizer(HeaderData)
  module Header = Packetizer.Header 

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
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create_fn (_scope : Scope.t) (i : Signal.t I.t) : (Signal.t O.t) =
    let spec = Reg_spec.create ~clock:i.clock ~reset:i.reset () in

    let source_rx = Flow.Dest.Of_signal.wires () in
    let sink_rx = Flow.Source.Of_signal.wires () in

    let sink = Flow.Endpoint.create sink_rx i.sink_tx in
    let source = Flow.Endpoint.create i.source_tx source_rx in

    let hdr, inter_flow = Packetizer.create_depacketizer spec ~source in

    let hdr_transf = Header.create_empty () in

    let out_flow = Packetizer.create_packetizer spec ~hdr:hdr_transf ~source:inter_flow in

    Flow.Endpoint.connect sink out_flow;

    Signal.assign hdr_transf.s.valid hdr.s.valid;
    Signal.assign hdr.d.ready hdr_transf.d.ready;
    (* HeaderData.Of_signal.pack hdr.data |> Signal.negate |> HeaderData.Of_signal.unpack |> HeaderData.Of_signal.assign hdr_transf.data; *)
    HeaderData.Of_signal.assign hdr_transf.s.data @@ HeaderData.Of_signal.unpack ~rev:true (Signal.of_hex ~width:72 "b0b1b2b3b4b5b6b7b8");
    
    {O.source_rx = source.dst;
      sink_rx = sink.src;
    }
end

let%expect_test "packetizer_full_unaligned" =
  let module PacketizerFullSim = PacketizerFullSim(TestHeader) in
  let module Sim = Sim.Sim(PacketizerFullSim) in
  
  let sim = Sim.create ~name:"packetizer_full_unaligned" ~gtkwave:false () in

  let inputs = Sim.inputs sim in
  let outputs = Sim.outputs sim in

  let emitter = FlowEmitter.create inputs.source_tx outputs.source_rx in
  let consumer = FlowConsumer.create outputs.sink_rx inputs.sink_tx in

  Sim.add_element sim (module FlowEmitter) emitter;
  Sim.add_element sim (module FlowConsumer) consumer;

  FlowEmitter.add_transfer emitter (FlowEmitter.gen_seq_transfer 31);
  FlowEmitter.add_transfer emitter (FlowEmitter.gen_seq_transfer 31);
  FlowEmitter.add_transfer emitter (FlowEmitter.gen_seq_transfer 29);
  FlowEmitter.add_transfer emitter (FlowEmitter.gen_seq_transfer 28);

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
  Sim.cycle_n sim 5;

  Sim.cycle_n sim 2;
  consumer.enable := true;
  Sim.cycle_n sim 7;

  Sim.cycle_n sim 9;
  consumer.enable := false;
  Sim.cycle_n sim 5;
  consumer.enable := true;

  Sim.cycle_n sim 20;

  FlowConsumer.expect_data consumer;

  [%expect {|
    b0b1b2b3 b4b5b6b7 b80a0b0c 0d0e0f10
    11121314 15161718 191a1b1c 1d1e1f

    b0b1b2b3 b4b5b6b7 b80a0b0c 0d0e0f10
    11121314 15161718 191a1b1c 1d1e1f

    b0b1b2b3 b4b5b6b7 b80a0b0c 0d0e0f10
    11121314 15161718 191a1b1c 1d

    b0b1b2b3 b4b5b6b7 b80a0b0c 0d0e0f10
    11121314 15161718 191a1b1c |}]

let%expect_test "packetizer_full_fast_unaligned" =
  let module PacketizerFullSim = PacketizerFullSim(TestHeader) in
  let module Sim = Sim.Sim(PacketizerFullSim) in
  
  let sim = Sim.create ~name:"packetizer_full_unaligned" ~gtkwave:false () in

  let inputs = Sim.inputs sim in
  let outputs = Sim.outputs sim in

  let emitter = FlowEmitter.create inputs.source_tx outputs.source_rx in
  let consumer = FlowConsumer.create outputs.sink_rx inputs.sink_tx in

  Sim.add_element sim (module FlowEmitter) emitter;
  Sim.add_element sim (module FlowConsumer) consumer;

  FlowEmitter.add_transfer emitter (FlowEmitter.gen_seq_transfer 31);
  FlowEmitter.add_transfer emitter (FlowEmitter.gen_seq_transfer ~from:100 44);
  FlowEmitter.add_transfer emitter (FlowEmitter.gen_seq_transfer 29);

  emitter.enable := true;

  consumer.enable := true;

  Sim.cycle_n sim 45;

  FlowConsumer.expect_data consumer;

  [%expect {|
    b0b1b2b3 b4b5b6b7 b80a0b0c 0d0e0f10
    11121314 15161718 191a1b1c 1d1e1f

    b0b1b2b3 b4b5b6b7 b86d6e6f 70717273
    74757677 78797a7b 7c7d7e7f 80818283
    84858687 88898a8b 8c8d8e8f

    b0b1b2b3 b4b5b6b7 b80a0b0c 0d0e0f10
    11121314 15161718 191a1b1c 1d |}]

module DepacketizerSim (HeaderData : Interface.S) = struct
  module Packetizer = Packet.Packetizer(HeaderData)
  module Header = Packetizer.Header

  module I = struct
    type 'a t =
       { clock : 'a
       ; reset : 'a
       ; source_tx : 'a Flow.Source.t [@rtlprefix "source_"]
       ; sink_tx : 'a Flow.Dest.t [@rtlprefix "sink_"]
       ; header : 'a Header.Dst.t [@rtlprefix "hdr_"]
       }
    [@@deriving sexp_of, hardcaml]
  end
  
  module O = struct
    type 'a t =
      { source_rx : 'a Flow.Dest.t [@rtlprefix "source_"]
      ; sink_rx : 'a Flow.Source.t [@rtlprefix "sink_"]
      ; header : 'a Header.Src.t [@rtlprefix "hdr_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create_fn (_scope : Scope.t) (i : Signal.t I.t) : (Signal.t O.t) =
    let spec = Reg_spec.create ~clock:i.clock ~reset:i.reset () in

    let source_rx = Flow.Dest.Of_signal.wires () in
    let sink_rx = Flow.Source.Of_signal.wires () in

    let sink = Flow.Endpoint.create sink_rx i.sink_tx in
    let source = Flow.Endpoint.create i.source_tx source_rx in

    let hdr, sink2 = Packetizer.create_depacketizer spec ~source in

    Header.Dst.Of_signal.assign hdr.d i.header;

    Flow.Endpoint.connect sink sink2;
    
    {O.source_rx = source.dst;
      sink_rx = sink.src;
      header = hdr.s;
    }
end

let%expect_test "depacketizer_unaligned" =
  let module DepacketizerSim = DepacketizerSim(TestHeader) in
  let module Sim = Sim.Sim(DepacketizerSim) in
  
  let sim = Sim.create ~name:"depacketizer_unaligned" ~gtkwave:false () in

  let inputs = Sim.inputs sim in
  let outputs = Sim.outputs sim in

  let emitter = FlowEmitter.create inputs.source_tx outputs.source_rx in
  let consumer = FlowConsumer.create outputs.sink_rx inputs.sink_tx in

  Sim.add_element sim (module FlowEmitter) emitter;
  Sim.add_element sim (module FlowConsumer) consumer;

  FlowEmitter.add_transfer emitter (FlowEmitter.gen_seq_transfer 31);
  FlowEmitter.add_transfer emitter (FlowEmitter.gen_seq_transfer 31);
  FlowEmitter.add_transfer emitter (FlowEmitter.gen_seq_transfer 29);
  FlowEmitter.add_transfer emitter (FlowEmitter.gen_seq_transfer 28);

  emitter.enable := false;
  inputs.header.ready := Bits.vdd;
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

  Sim.cycle_n sim 2;
  consumer.enable := true;
  Sim.cycle_n sim 7;

  Sim.cycle_n sim 9;
  consumer.enable := false;
  Sim.cycle_n sim 5;
  consumer.enable := true;

  Sim.cycle_n sim 10;

  FlowConsumer.expect_data consumer;

  Sim.expect_trace_digest sim;

  [%expect {|
    0a0b0c0d 0e0f1011 12131415 16171819
    1a1b1c1d 1e1f

    0a0b0c0d 0e0f1011 12131415 16171819
    1a1b1c1d 1e1f

    0a0b0c0d 0e0f1011 12131415 16171819
    1a1b1c1d

    0a0b0c0d 0e0f1011 12131415 16171819
    1a1b1c

    ad27b526bd51e212826b07b4d58dd844|}]

module PacketizerSim (HeaderData : Interface.S) = struct
  module Packetizer = Packet.Packetizer(HeaderData)
  module Header = Packetizer.Header 

  module I = struct
    type 'a t =
        { clock : 'a
        ; reset : 'a
        ; source_tx : 'a Flow.Source.t [@rtlprefix "source_"]
        ; sink_tx : 'a Flow.Dest.t [@rtlprefix "sink_"]
        ; header : 'a Header.Src.t [@rtlprefix "hdr_"]
        }
    [@@deriving sexp_of, hardcaml]
  end
  
  module O = struct
    type 'a t =
      { source_rx : 'a Flow.Dest.t [@rtlprefix "source_"]
      ; sink_rx : 'a Flow.Source.t [@rtlprefix "sink_"]
      ; header : 'a Header.Dst.t [@rtlprefix "hdr_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create_fn (_scope : Scope.t) (i : Signal.t I.t) : (Signal.t O.t) =
    let spec = Reg_spec.create ~clock:i.clock ~reset:i.reset () in

    let source_rx = Flow.Dest.Of_signal.wires () in
    let sink_rx = Flow.Source.Of_signal.wires () in

    let sink = Flow.Endpoint.create sink_rx i.sink_tx in
    let source = Flow.Endpoint.create i.source_tx source_rx in
    
    let hdr = Header.create i.header (Header.Dst.Of_signal.wires ()) in

    Flow.Endpoint.connect sink (Packetizer.create_packetizer spec ~hdr ~source);
    
    {O.source_rx = source.dst
    ; sink_rx = sink.src
    ; header = hdr.d
    }
end

let%expect_test "packetizer_unaligned" =
  let module PacketizerSim = PacketizerSim(TestHeader) in
  let module Sim = Sim.Sim(PacketizerSim) in
  
  let sim = Sim.create ~name:"packetizer_unaligned" ~gtkwave:false () in

  let inputs = Sim.inputs sim in
  let outputs = Sim.outputs sim in

  let emitter = FlowEmitter.create inputs.source_tx outputs.source_rx in
  let consumer = FlowConsumer.create outputs.sink_rx inputs.sink_tx in

  Sim.add_element sim (module FlowEmitter) emitter;
  Sim.add_element sim (module FlowConsumer) consumer;

  FlowEmitter.add_transfer emitter (FlowEmitter.gen_seq_transfer 24);
  FlowEmitter.add_transfer emitter (FlowEmitter.gen_seq_transfer 24);
  FlowEmitter.add_transfer emitter (FlowEmitter.gen_seq_transfer 24);

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

  emitter.enable := false;
  inputs.header.valid := Bits.vdd;
  Sim.cycle_n sim 6;
  emitter.enable := true;

  Sim.cycle_n sim 20;

  FlowConsumer.expect_data consumer;
  Sim.expect_trace_digest sim;

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

    1f3ac8a5136edb664a285382159327ce|}]
