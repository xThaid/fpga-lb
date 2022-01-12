open Base
open Hardcaml
open Lb_dataplane
open Sim_elements

module TestData = struct 
  type 'a t =
    { field1 : 'a [@bits 48]
    ; field2 : 'a [@bits 8]
    ; field3 : 'a [@bits 16]
    }
  [@@deriving sexp_of, sexp, hardcaml]
end

module TestDataAligned = struct 
  type 'a t =
    { field1 : 'a [@bits 48]
    ; field2 : 'a [@bits 8]
    ; field3 : 'a [@bits 16]
    ; field4 : 'a [@bits 24]
    }
  [@@deriving sexp_of, hardcaml]
end

module SerializerSim (Data : Interface.S) = struct
  module Serializer = Transaction.Serializer(Data)
  module Transaction = Transaction.Make(Data)

  module I = struct
    type 'a t =
        { clock : 'a
        ; reset : 'a
        ; sink_tx : 'a Flow.Dest.t [@rtlprefix "sink_"]
        ; tst : 'a Transaction.Src.t [@rtlprefix "tst_"]
        }
    [@@deriving sexp_of, hardcaml]
  end
  
  module O = struct
    type 'a t =
      { sink_rx : 'a Flow.Source.t [@rtlprefix "sink_"]
      ; tst : 'a Transaction.Dst.t [@rtlprefix "tst_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create_fn (_scope : Scope.t) (i : Signal.t I.t) : (Signal.t O.t) =
    let spec = Reg_spec.create ~clock:i.clock ~reset:i.reset () in

    let sink_rx = Flow.Source.Of_signal.wires () in

    let sink = Flow.t_of_if sink_rx i.sink_tx in

    let tst = Transaction.t_of_if i.tst (Transaction.Dst.Of_signal.wires ()) in

    Flow.connect sink (Serializer.serialize spec tst);
    
    {O.sink_rx = sink.src; tst = Transaction.outputs tst}

end

let%expect_test "transaction_serializer" =
  let module SerializerSim = SerializerSim(TestData) in
  let module Sim = Sim.Sim(SerializerSim) in
  
  let sim = Sim.create ~name:"transaction_serializer" ~gtkwave:false () in

  let inputs = Sim.inputs sim in
  let outputs = Sim.outputs sim in

  let consumer = FlowConsumer.create outputs.sink_rx inputs.sink_tx in

  Sim.add_element sim (module FlowConsumer) consumer;

  inputs.tst.data.field1 := (Bits.of_hex ~width:48 "f0f1f2f3f4f5");
  inputs.tst.data.field2 := (Bits.of_hex ~width:8 "f6");
  inputs.tst.data.field3 := (Bits.of_hex ~width:16 "f7f8");

  consumer.enabled <- false;
  Sim.cycle_n sim 2;
  consumer.enabled <- true;
  Sim.cycle_n sim 1;
  inputs.tst.valid := Bits.vdd;
  Sim.cycle_n sim 1;
  inputs.tst.valid := Bits.gnd;
  Sim.cycle_n sim 1;
  consumer.enabled <- false;
  Sim.cycle_n sim 1;
  consumer.enabled <- true;
  Sim.cycle_n sim 1;
  consumer.enabled <- false;
  Sim.cycle_n sim 1;
  consumer.enabled <- true;

  inputs.tst.data.field2 := (Bits.of_hex ~width:8 "a6");
  inputs.tst.valid := Bits.vdd;
  Sim.cycle_n sim 2;
  inputs.tst.valid := Bits.gnd;

  Sim.cycle_n sim 10;

  FlowConsumer.expect_data consumer;
  Sim.expect_trace_digest sim;

  [%expect {|
    f0f1f2f3 f4f5f6f7 f8

    f0f1f2f3 f4f5a6f7 f8

    546bd74e9554515f4c427ff9f212ad57|}]

let%expect_test "transaction_serializer_aligned" =
  let module SerializerSim = SerializerSim(TestDataAligned) in
  let module Sim = Sim.Sim(SerializerSim) in
  
  let sim = Sim.create ~name:"transaction_serializer_aligned" ~gtkwave_name:"transaction_serializer" ~gtkwave:false () in

  let inputs = Sim.inputs sim in
  let outputs = Sim.outputs sim in

  let consumer = FlowConsumer.create outputs.sink_rx inputs.sink_tx in

  Sim.add_element sim (module FlowConsumer) consumer;

  inputs.tst.data.field1 := (Bits.of_hex ~width:48 "f0f1f2f3f4f5");
  inputs.tst.data.field2 := (Bits.of_hex ~width:8 "f6");
  inputs.tst.data.field3 := (Bits.of_hex ~width:16 "f7f8");
  inputs.tst.data.field4 := (Bits.of_hex ~width:24 "f7f8f9");

  consumer.enabled <- false;
  Sim.cycle_n sim 2;
  consumer.enabled <- true;
  Sim.cycle_n sim 1;
  inputs.tst.valid := Bits.vdd;
  Sim.cycle_n sim 1;
  inputs.tst.valid := Bits.gnd;
  Sim.cycle_n sim 1;
  consumer.enabled <- false;
  Sim.cycle_n sim 1;
  consumer.enabled <- true;
  Sim.cycle_n sim 1;
  consumer.enabled <- false;
  Sim.cycle_n sim 1;
  consumer.enabled <- true;

  inputs.tst.data.field2 := (Bits.of_hex ~width:8 "a6");
  inputs.tst.valid := Bits.vdd;
  Sim.cycle_n sim 2;
  inputs.tst.valid := Bits.gnd;

  Sim.cycle_n sim 10;

  FlowConsumer.expect_data consumer;
  Sim.expect_trace_digest sim;

  [%expect {|
    f0f1f2f3 f4f5f6f7 f8f7f8f9

    f0f1f2f3 f4f5a6f7 f8f7f8f9

    4ef8d54e9f727ffdfedb1d2d8044c508|}]


module DeserializerSim (Data : Interface.S) = struct
  module Serializer = Transaction.Serializer(Data)
  module Transaction = Transaction.Make(Data)

  module I = struct
    type 'a t =
        { clock : 'a
        ; reset : 'a
        ; source_tx : 'a Flow.Source.t [@rtlprefix "source_"]
        ; tst : 'a Transaction.Dst.t [@rtlprefix "tst_"]
        }
    [@@deriving sexp_of, hardcaml]
  end
  
  module O = struct
    type 'a t =
      { source_rx : 'a Flow.Dest.t [@rtlprefix "source_"]
      ; tst : 'a Transaction.Src.t [@rtlprefix "tst_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create_fn (_scope : Scope.t) (i : Signal.t I.t) : (Signal.t O.t) =
    let spec = Reg_spec.create ~clock:i.clock ~reset:i.reset () in

    let source = Flow.t_of_if i.source_tx (Flow.Dest.Of_signal.wires ()) in

    let tst = Serializer.deserialize spec source in
    let tst_s, tst_d = Transaction.if_of_t tst in
    Signal.(tst_d.ready <== i.tst.ready);

    {O.source_rx = source.dst; tst = tst_s}

end

let%expect_test "transaction_deserializer" =
  let module DeserializerSim = DeserializerSim(TestData) in
  let module Sim = Sim.Sim(DeserializerSim) in
  let module Consumer = TransactionConsumer(TestData) in
  
  let sim = Sim.create ~name:"transaction_deserializer" ~gtkwave:false () in

  let inputs = Sim.inputs sim in
  let outputs = Sim.outputs sim in

  let emitter = FlowEmitter.create inputs.source_tx outputs.source_rx in
  let consumer = Consumer.create outputs.tst inputs.tst in

  Sim.add_element sim (module FlowEmitter) emitter;
  Sim.add_element sim (module Consumer) consumer;

  FlowEmitter.add_transfer emitter (FlowEmitter.gen_seq_transfer 22);
  FlowEmitter.add_transfer emitter (FlowEmitter.gen_seq_transfer ~from:32 9);
  FlowEmitter.add_transfer emitter (FlowEmitter.gen_seq_transfer ~from:48 9);
  FlowEmitter.add_transfer emitter (FlowEmitter.gen_seq_transfer ~from:64 9);
  FlowEmitter.add_transfer emitter (FlowEmitter.gen_seq_transfer ~from:80 9);
  FlowEmitter.add_transfer emitter (FlowEmitter.gen_seq_transfer ~from:96 24);
  FlowEmitter.add_transfer emitter (FlowEmitter.gen_seq_transfer ~from:112 9);
  FlowEmitter.add_transfer emitter (FlowEmitter.gen_seq_transfer ~from:128 9);

  emitter.enabled <- true;
  consumer.enabled <- true;

  Sim.cycle_n sim 4;

  emitter.enabled <- false;
  Sim.cycle_n sim 2;
  emitter.enabled <- true;
  Sim.cycle_n sim 5;
  emitter.enabled <- false;
  Sim.cycle_n sim 1;
  emitter.enabled <- true;
  Sim.cycle_n sim 2;
  consumer.enabled <- false;
  Sim.cycle_n sim 1;
  emitter.enabled <- false;
  Sim.cycle_n sim 2;
  consumer.enabled <- true;
  Sim.cycle_n sim 3;
  emitter.enabled <- true;
  Sim.cycle_n sim 4;
  consumer.enabled <- false;
  Sim.cycle_n sim 3;
  consumer.enabled <- true;
  Sim.cycle_n sim 6;
  consumer.enabled <- false;
  Sim.cycle_n sim 7;
  consumer.enabled <- true;

  Sim.cycle_n sim 15;

  Consumer.expect_reads consumer;
  Sim.expect_trace_digest sim;

  [%expect {|
    (consumed
     (((field1 010203040506) (field2 07) (field3 0809))
      ((field1 202122232425) (field2 26) (field3 2728))
      ((field1 303132333435) (field2 36) (field3 3738))
      ((field1 404142434445) (field2 46) (field3 4748))
      ((field1 505152535455) (field2 56) (field3 5758))
      ((field1 606162636465) (field2 66) (field3 6768))
      ((field1 707172737475) (field2 76) (field3 7778))
      ((field1 808182838485) (field2 86) (field3 8788))))
    898e55e1d82bf24b7a30f8a33080f83a|}]


module WithFlowSim (Data : Interface.S) = struct
  module With_flow = Transaction.With_flow(Data)
  module Transaction = Transaction.Make(Data)

  module I = struct
    type 'a t =
        { clock : 'a
        ; reset : 'a
        ; source_tx : 'a Flow.Source.t [@rtlprefix "source_"]
        ; sink_tx : 'a Flow.Dest.t [@rtlprefix "sink_"]
        ; tst_in : 'a Transaction.Src.t [@rtlprefix "tstin_"]
        ; tst_out : 'a Transaction.Dst.t [@rtlprefix "tstout_"]
        }
    [@@deriving sexp_of, hardcaml]
  end
  
  module O = struct
    type 'a t =
      { source_rx : 'a Flow.Dest.t [@rtlprefix "source_"]
      ; sink_rx : 'a Flow.Source.t [@rtlprefix "sink_"]
      ; tst_in : 'a Transaction.Dst.t [@rtlprefix "tstin_"]
      ; tst_out : 'a Transaction.Src.t [@rtlprefix "tstout_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create_fn (_scope : Scope.t) (i : Signal.t I.t) : (Signal.t O.t) =
    let spec = Reg_spec.create ~clock:i.clock ~reset:i.reset () in

    let source = Flow.t_of_if i.source_tx (Flow.Dest.Of_signal.wires ()) in
    let tst_in = Transaction.t_of_if i.tst_in (Transaction.Dst.Of_signal.wires ()) in
    let combined = With_flow.combine spec tst_in source in

    Transaction.Dst.Of_signal.assign (Transaction.outputs combined.tst) i.tst_out;
    Flow.Dest.Of_signal.assign combined.flow.dst i.sink_tx;

    { O.source_rx = source.dst
    ; sink_rx = combined.flow.src
    ; tst_in = (Transaction.outputs tst_in)
    ; tst_out = (Transaction.inputs combined.tst);
    }

end
    
let%expect_test "transaction_with_flow" =
  let module WithFlowSim = WithFlowSim(TestData) in
  let module Sim = Sim.Sim(WithFlowSim) in
  let module Emitter = TransactionEmitter(TestData) in
  let module Consumer = TransactionConsumer(TestData) in
  
  let sim = Sim.create ~name:"transaction_with_flow" ~gtkwave:false () ~trace:false in

  let inputs = Sim.inputs sim in
  let outputs = Sim.outputs sim in

  let flow_emitter = FlowEmitter.create inputs.source_tx outputs.source_rx in
  let flow_consumer = FlowConsumer.create outputs.sink_rx inputs.sink_tx in

  let tst_emitter = Emitter.create inputs.tst_in outputs.tst_in in
  let tst_consumer = Consumer.create outputs.tst_out inputs.tst_out in

  Sim.add_element sim (module FlowEmitter) flow_emitter;
  Sim.add_element sim (module FlowConsumer) flow_consumer;

  Sim.add_element sim (module Emitter) tst_emitter;
  Sim.add_element sim (module Consumer) tst_consumer;

  FlowEmitter.add_transfer flow_emitter (FlowEmitter.gen_seq_transfer 21);
  FlowEmitter.add_transfer flow_emitter (FlowEmitter.gen_seq_transfer ~from:32 9);
  FlowEmitter.add_transfer flow_emitter (FlowEmitter.gen_seq_transfer ~from:48 18);
  FlowEmitter.add_transfer flow_emitter (FlowEmitter.gen_seq_transfer ~from:80 33);
  FlowEmitter.add_transfer flow_emitter (FlowEmitter.gen_seq_transfer ~from:128 16);

  let create_data data = TestData.t_of_sexp String.t_of_sexp (Parsexp.Single.parse_string_exn data) in

  Emitter.add_transfer tst_emitter (create_data "((field1 05060708090a) (field2 0b) (field3 0c0d))");
  Emitter.add_transfer tst_emitter (create_data "((field1 202122232425) (field2 26) (field3 2728))");
  Emitter.add_transfer tst_emitter (create_data "((field1 303132333435) (field2 36) (field3 3738))");
  Emitter.add_transfer tst_emitter (create_data "((field1 404142434445) (field2 46) (field3 4748))");
  Emitter.add_transfer tst_emitter (create_data "((field1 505152535455) (field2 56) (field3 5758))");

  Sim.cycle_n sim 1;
  flow_emitter.enabled <- true;
  flow_consumer.enabled <- true;
  tst_consumer.enabled <- true;

  Sim.cycle_n sim 2;
  tst_emitter.enabled <- true;
  Sim.cycle_n sim 2;
  flow_consumer.enabled <- false;
  Sim.cycle_n sim 2;
  flow_consumer.enabled <- true;
  tst_consumer.enabled <- false;
  Sim.cycle_n sim 8;
  tst_consumer.enabled <- true;
  Sim.cycle_n sim 1;
  flow_emitter.enabled <- false;
  tst_consumer.enabled <- false;
  Sim.cycle_n sim 1;
  flow_emitter.enabled <- true;
  Sim.cycle_n sim 7;
  tst_consumer.enabled <- true;
  Sim.cycle_n sim 10;
  flow_emitter.enabled <- false;
  Sim.cycle_n sim 2;
  flow_emitter.enabled <- true;

  Sim.cycle_n sim 10;
  
  Consumer.expect_reads tst_consumer;
  FlowConsumer.expect_data flow_consumer;
  Sim.expect_trace_digest sim;

  [%expect {|
    (consumed
     (((field1 05060708090a) (field2 0b) (field3 0c0d))
      ((field1 202122232425) (field2 26) (field3 2728))
      ((field1 303132333435) (field2 36) (field3 3738))
      ((field1 404142434445) (field2 46) (field3 4748))
      ((field1 505152535455) (field2 56) (field3 5758))))
    01020304 05060708 090a0b0c 0d0e0f10
    11121314 15

    20212223 24252627 28

    30313233 34353637 38393a3b 3c3d3e3f
    4041

    50515253 54555657 58595a5b 5c5d5e5f
    60616263 64656667 68696a6b 6c6d6e6f
    70

    80818283 84858687 88898a8b 8c8d8e8f

    063db41ba61d522b9d9ffb66cb52043e|}]


module PacketizerFullSim (HeaderData : Interface.S) = struct
  module Header = Transaction.Make(HeaderData)
  module Serializer = Transaction.Serializer(HeaderData)

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

    let sink = Flow.t_of_if sink_rx i.sink_tx in
    let source = Flow.t_of_if i.source_tx source_rx in

    let f1, f2 = Flow.split spec ~hdr_length:Header.data_len ~source in
    let tst = Serializer.deserialize spec f1 in

    let hdr_transf = Header.map_comb tst ~f:(fun _ ->
      HeaderData.Of_signal.unpack ~rev:true (Signal.of_hex ~width:72 "b0b1b2b3b4b5b6b7b8")
    ) in

    let f1 = Serializer.serialize spec hdr_transf in
    let out_flow = Flow.join spec ~hdr_length:Header.data_len ~source1:f1 ~source2:f2 in

    Flow.connect sink out_flow;
    
    {O.source_rx = source.dst;
      sink_rx = sink.src;
    }
end

let%expect_test "packetizer_full_unaligned" =
  let module PacketizerFullSim = PacketizerFullSim(TestData) in
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

  emitter.enabled <- false;

  consumer.enabled <- false;

  Sim.cycle_n sim 2;
  emitter.enabled <- true;
  Sim.cycle_n sim 2;
  emitter.enabled <- false;
  Sim.cycle_n sim 2;
  emitter.enabled <- true;
  Sim.cycle_n sim 3;

  consumer.enabled <- true;
  Sim.cycle_n sim 3;

  emitter.enabled <- false;
  Sim.cycle_n sim 1;
  emitter.enabled <- true;
  Sim.cycle_n sim 1;
  consumer.enabled <- false;
  Sim.cycle_n sim 5;

  Sim.cycle_n sim 2;
  consumer.enabled <- true;
  Sim.cycle_n sim 7;

  Sim.cycle_n sim 9;
  consumer.enabled <- false;
  Sim.cycle_n sim 5;
  consumer.enabled <- true;

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
  let module PacketizerFullSim = PacketizerFullSim(TestData) in
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

  emitter.enabled <- true;

  consumer.enabled <- true;

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
  module Header = Transaction.Make(HeaderData)
  module Serializer = Transaction.Serializer(HeaderData)

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

    let sink = Flow.t_of_if sink_rx i.sink_tx in
    let source = Flow.t_of_if i.source_tx source_rx in

    let f1, sink2 = Flow.split spec ~hdr_length:Header.data_len ~source in
    let hdr = Serializer.deserialize spec f1 in
    let hdr_s, hdr_d = Header.if_of_t hdr in

    Header.Dst.Of_signal.assign hdr_d i.header;

    Flow.connect sink sink2;
    
    {O.source_rx = source.dst;
      sink_rx = sink.src;
      header = hdr_s;
    }
end

let%expect_test "depacketizer_unaligned" =
  let module DepacketizerSim = DepacketizerSim(TestData) in
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

  emitter.enabled <- false;
  inputs.header.ready := Bits.vdd;
  consumer.enabled <- false;

  Sim.cycle_n sim 2;
  emitter.enabled <- true;
  Sim.cycle_n sim 2;
  emitter.enabled <- false;
  Sim.cycle_n sim 2;
  emitter.enabled <- true;
  Sim.cycle_n sim 3;

  consumer.enabled <- true;
  Sim.cycle_n sim 3;

  emitter.enabled <- false;
  Sim.cycle_n sim 1;
  emitter.enabled <- true;
  Sim.cycle_n sim 1;
  consumer.enabled <- false;

  Sim.cycle_n sim 2;
  consumer.enabled <- true;
  Sim.cycle_n sim 7;

  Sim.cycle_n sim 9;
  consumer.enabled <- false;
  Sim.cycle_n sim 5;
  consumer.enabled <- true;

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
  module Header = Transaction.Make(HeaderData)
  module Serializer = Transaction.Serializer(HeaderData)

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

    let sink = Flow.t_of_if sink_rx i.sink_tx in
    let source = Flow.t_of_if i.source_tx source_rx in
    
    let hdr = Header.t_of_if i.header (Header.Dst.Of_signal.wires ()) in

    let f1 = Serializer.serialize spec hdr in
    let out_flow = Flow.join spec ~hdr_length:Header.data_len ~source1:f1 ~source2:source in
    Flow.connect sink out_flow;
    
    {O.source_rx = source.dst
    ; sink_rx = sink.src
    ; header = Header.outputs hdr
    }
end

let%expect_test "packetizer_unaligned" =
  let module PacketizerSim = PacketizerSim(TestData) in
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

  emitter.enabled <- false;

  inputs.header.data.field1 := (Bits.of_hex ~width:48 "f0f1f2f3f4f5");
  inputs.header.data.field2 := (Bits.of_hex ~width:8 "f6");
  inputs.header.data.field3 := (Bits.of_hex ~width:16 "f7f8");

  Sim.cycle_n sim 2;
  inputs.header.valid := Bits.vdd;
  Sim.cycle_n sim 2;
  consumer.enabled <- true;
  Sim.cycle_n sim 1;
  emitter.enabled <- true;
  inputs.header.valid := Bits.gnd;
  Sim.cycle_n sim 7;
  inputs.header.valid := Bits.vdd;
  inputs.header.data.field2 := (Bits.of_hex ~width:8 "a6");

  Sim.cycle_n sim 1;
  inputs.header.valid := Bits.gnd;
  Sim.cycle_n sim 1;
  consumer.enabled <- false;
  Sim.cycle_n sim 1;
  consumer.enabled <- true;
  Sim.cycle_n sim 1;
  consumer.enabled <- false;
  Sim.cycle_n sim 2;
  consumer.enabled <- true;
  Sim.cycle_n sim 1;
  consumer.enabled <- false;
  Sim.cycle_n sim 1;
  consumer.enabled <- true;
  Sim.cycle_n sim 5;

  emitter.enabled <- false;
  inputs.header.valid := Bits.vdd;
  Sim.cycle_n sim 6;
  emitter.enabled <- true;

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
