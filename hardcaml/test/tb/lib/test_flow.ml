open Base
open Hardcaml
open Lb_dataplane
open Lb_dataplane_test_lib
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
  module Serializer = Flow.Serializer(Data)
  module Transaction = Transaction.Make(Data)

  module I = struct
    type 'a t =
        { clock : 'a
        ; reset : 'a
        ; sink_tx : 'a Flow.Base.Dst.t [@rtlprefix "sink_"]
        ; tst : 'a Transaction.Src.t [@rtlprefix "tst_"]
        }
    [@@deriving sexp_of, hardcaml]
  end
  
  module O = struct
    type 'a t =
      { sink_rx : 'a Flow.Base.Src.t [@rtlprefix "sink_"]
      ; tst : 'a Transaction.Dst.t [@rtlprefix "tst_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create_fn (_scope : Scope.t) (i : Signal.t I.t) : (Signal.t O.t) =
    let spec = Reg_spec.create ~clock:i.clock ~reset:i.reset () in

    let sink_rx = Flow.Base.Src.Of_signal.wires () in

    let sink = Flow.Base.t_of_if sink_rx i.sink_tx in

    let tst = Transaction.t_of_if i.tst (Transaction.Dst.Of_signal.wires ()) in

    Flow.Base.connect sink (Serializer.serialize spec tst);
    
    {O.sink_rx = sink.s; tst = Transaction.outputs tst}

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
  module Serializer = Flow.Serializer(Data)
  module Transaction = Transaction.Make(Data)

  module I = struct
    type 'a t =
        { clock : 'a
        ; reset : 'a
        ; source_tx : 'a Flow.Base.Src.t [@rtlprefix "source_"]
        ; tst : 'a Transaction.Dst.t [@rtlprefix "tst_"]
        }
    [@@deriving sexp_of, hardcaml]
  end
  
  module O = struct
    type 'a t =
      { source_rx : 'a Flow.Base.Dst.t [@rtlprefix "source_"]
      ; tst : 'a Transaction.Src.t [@rtlprefix "tst_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create_fn (_scope : Scope.t) (i : Signal.t I.t) : (Signal.t O.t) =
    let spec = Reg_spec.create ~clock:i.clock ~reset:i.reset () in

    let source = Flow.Base.t_of_if i.source_tx (Flow.Base.Dst.Of_signal.wires ()) in

    let tst = Serializer.deserialize spec source in
    let tst_s, tst_d = Transaction.if_of_t tst in
    Signal.(tst_d.ready <== i.tst.ready);

    {O.source_rx = source.d; tst = tst_s}

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

let%expect_test "transaction_deserializer_aligned" =
  let module DeserializerSim = DeserializerSim(TestDataAligned) in
  let module Sim = Sim.Sim(DeserializerSim) in
  let module Consumer = TransactionConsumer(TestDataAligned) in
  
  let sim = Sim.create ~name:"transaction_deserializer_aligned" ~gtkwave:false () in

  let inputs = Sim.inputs sim in
  let outputs = Sim.outputs sim in

  let emitter = FlowEmitter.create inputs.source_tx outputs.source_rx in
  let consumer = Consumer.create outputs.tst inputs.tst in

  Sim.add_element sim (module FlowEmitter) emitter;
  Sim.add_element sim (module Consumer) consumer;

  FlowEmitter.add_transfer emitter (FlowEmitter.gen_seq_transfer ~from:0 22);
  FlowEmitter.add_transfer emitter (FlowEmitter.gen_seq_transfer ~from:32 12);
  FlowEmitter.add_transfer emitter (FlowEmitter.gen_seq_transfer ~from:48 12);
  FlowEmitter.add_transfer emitter (FlowEmitter.gen_seq_transfer ~from:64 12);
  FlowEmitter.add_transfer emitter (FlowEmitter.gen_seq_transfer ~from:80 12);
  FlowEmitter.add_transfer emitter (FlowEmitter.gen_seq_transfer ~from:96 24);
  FlowEmitter.add_transfer emitter (FlowEmitter.gen_seq_transfer ~from:112 12);
  FlowEmitter.add_transfer emitter (FlowEmitter.gen_seq_transfer ~from:128 15);

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
     (((field1 000102030405) (field2 06) (field3 0708) (field4 090a0b))
      ((field1 202122232425) (field2 26) (field3 2728) (field4 292a2b))
      ((field1 303132333435) (field2 36) (field3 3738) (field4 393a3b))
      ((field1 404142434445) (field2 46) (field3 4748) (field4 494a4b))
      ((field1 505152535455) (field2 56) (field3 5758) (field4 595a5b))
      ((field1 606162636465) (field2 66) (field3 6768) (field4 696a6b))
      ((field1 707172737475) (field2 76) (field3 7778) (field4 797a7b))
      ((field1 808182838485) (field2 86) (field3 8788) (field4 898a8b))))
    2e8e4dd57c90dc09289f4a41cd50d3f3|}]

module WithHeaderSim (Data : Interface.S) = struct
  module With_flow = Flow.With_header(Data)
  module Transaction = Transaction.Make(Data)

  module I = struct
    type 'a t =
        { clock : 'a
        ; reset : 'a
        ; source_tx : 'a Flow.Base.Src.t [@rtlprefix "source_"]
        ; sink_tx : 'a Flow.Base.Dst.t [@rtlprefix "sink_"]
        ; tst_in : 'a Transaction.Src.t [@rtlprefix "tstin_"]
        ; tst_out : 'a Transaction.Dst.t [@rtlprefix "tstout_"]
        }
    [@@deriving sexp_of, hardcaml]
  end
  
  module O = struct
    type 'a t =
      { source_rx : 'a Flow.Base.Dst.t [@rtlprefix "source_"]
      ; sink_rx : 'a Flow.Base.Src.t [@rtlprefix "sink_"]
      ; tst_in : 'a Transaction.Dst.t [@rtlprefix "tstin_"]
      ; tst_out : 'a Transaction.Src.t [@rtlprefix "tstout_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create_fn (_scope : Scope.t) (i : Signal.t I.t) : (Signal.t O.t) =
    let spec = Reg_spec.create ~clock:i.clock ~reset:i.reset () in

    let source = Flow.Base.t_of_if i.source_tx (Flow.Base.Dst.Of_signal.wires ()) in
    let tst_in = Transaction.t_of_if i.tst_in (Transaction.Dst.Of_signal.wires ()) in
    let combined = fst (With_flow.weak_barrier spec (With_flow.create tst_in source)) in

    Transaction.Dst.Of_signal.assign (Transaction.outputs combined.hdr) i.tst_out;
    Flow.Base.Dst.Of_signal.assign combined.flow.d i.sink_tx;

    { O.source_rx = source.d
    ; sink_rx = combined.flow.s
    ; tst_in = (Transaction.outputs tst_in)
    ; tst_out = (Transaction.inputs combined.hdr);
    }

end
    
let%expect_test "flow_with_hdr_synchronize" =
  let module WithHeaderSim = WithHeaderSim(TestData) in
  let module Sim = Sim.Sim(WithHeaderSim) in
  let module Emitter = TransactionEmitter(TestData) in
  let module Consumer = TransactionConsumer(TestData) in
  
  let sim = Sim.create ~name:"flow_with_hdr_synchronize" ~gtkwave:false () ~trace:false in

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
  FlowEmitter.add_transfer flow_emitter (FlowEmitter.gen_seq_transfer ~from:160 16);
  FlowEmitter.add_transfer flow_emitter (FlowEmitter.gen_seq_transfer ~from:192 16);

  let create_data data =
    Parsexp.Single.parse_string_exn data |>
    TestData.t_of_sexp String.t_of_sexp |>
    TestData.map2 TestData.port_widths ~f:(fun width x -> Bits.of_hex ~width x)
  in

  Emitter.add_transfer tst_emitter (create_data "((field1 05060708090a) (field2 0b) (field3 0c0d))");
  Emitter.add_transfer tst_emitter (create_data "((field1 202122232425) (field2 26) (field3 2728))");
  Emitter.add_transfer tst_emitter (create_data "((field1 303132333435) (field2 36) (field3 3738))");
  Emitter.add_transfer tst_emitter (create_data "((field1 404142434445) (field2 46) (field3 4748))");
  Emitter.add_transfer tst_emitter (create_data "((field1 505152535455) (field2 56) (field3 5758))");
  Emitter.add_transfer tst_emitter (create_data "((field1 606162636465) (field2 66) (field3 6768))");
  Emitter.add_transfer tst_emitter (create_data "((field1 707172737475) (field2 76) (field3 7778))");

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

  Sim.cycle_n sim 30;
  
  Consumer.expect_reads tst_consumer;
  FlowConsumer.expect_data flow_consumer;

  [%expect {|
    (consumed
     (((field1 05060708090a) (field2 0b) (field3 0c0d))
      ((field1 202122232425) (field2 26) (field3 2728))
      ((field1 303132333435) (field2 36) (field3 3738))
      ((field1 404142434445) (field2 46) (field3 4748))
      ((field1 505152535455) (field2 56) (field3 5758))
      ((field1 606162636465) (field2 66) (field3 6768))
      ((field1 707172737475) (field2 76) (field3 7778))))
    01020304 05060708 090a0b0c 0d0e0f10
    11121314 15

    20212223 24252627 28

    30313233 34353637 38393a3b 3c3d3e3f
    4041

    50515253 54555657 58595a5b 5c5d5e5f
    60616263 64656667 68696a6b 6c6d6e6f
    70

    80818283 84858687 88898a8b 8c8d8e8f

    a0a1a2a3 a4a5a6a7 a8a9aaab acadaeaf

    c0c1c2c3 c4c5c6c7 c8c9cacb cccdcecf|}]


module PacketizerFullSim (HeaderData : Interface.S) = struct
  module Header = Transaction.Make(HeaderData)
  module Serializer = Flow.Serializer(HeaderData)

  module I = struct
    type 'a t =
       { clock : 'a
       ; reset : 'a
       ; source_tx : 'a Flow.Base.Src.t [@rtlprefix "source_"]
       ; sink_tx : 'a Flow.Base.Dst.t [@rtlprefix "sink_"]
       }
    [@@deriving sexp_of, hardcaml]
  end
  
  module O = struct
    type 'a t =
      { source_rx : 'a Flow.Base.Dst.t [@rtlprefix "source_"]
      ; sink_rx : 'a Flow.Base.Src.t [@rtlprefix "sink_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create_fn (_scope : Scope.t) (i : Signal.t I.t) : (Signal.t O.t) =
    let spec = Reg_spec.create ~clock:i.clock ~reset:i.reset () in

    let source_rx = Flow.Base.Dst.Of_signal.wires () in
    let sink_rx = Flow.Base.Src.Of_signal.wires () in

    let sink = Flow.Base.t_of_if sink_rx i.sink_tx in
    let source = Flow.Base.t_of_if i.source_tx source_rx in

    let f1, f2 = Flow.Base.split spec ~hdr_length:Header.data_len ~source in
    let tst = Serializer.deserialize spec f1 in

    let hdr_transf = Header.map tst ~f:(fun _ ->
      HeaderData.Of_signal.unpack ~rev:true (Signal.of_hex ~width:Header.data_len "b0b1b2b3b4b5b6b7b8b9babb")
    ) in

    let f1 = Serializer.serialize spec hdr_transf in
    let out_flow = Flow.Base.join spec ~hdr_length:Header.data_len ~source1:f1 ~source2:f2 in

    Flow.Base.connect sink out_flow;
    
    {O.source_rx = source.d;
      sink_rx = sink.s;
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

  Sim.cycle_n sim 25;

  FlowConsumer.expect_data consumer;

  [%expect {|
    b3b4b5b6 b7b8b9ba bb0a0b0c 0d0e0f10
    11121314 15161718 191a1b1c 1d1e1f

    b3b4b5b6 b7b8b9ba bb0a0b0c 0d0e0f10
    11121314 15161718 191a1b1c 1d1e1f

    b3b4b5b6 b7b8b9ba bb0a0b0c 0d0e0f10
    11121314 15161718 191a1b1c 1d

    b3b4b5b6 b7b8b9ba bb0a0b0c 0d0e0f10
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
    b3b4b5b6 b7b8b9ba bb0a0b0c 0d0e0f10
    11121314 15161718 191a1b1c 1d1e1f

    b3b4b5b6 b7b8b9ba bb6d6e6f 70717273
    74757677 78797a7b 7c7d7e7f 80818283
    84858687 88898a8b 8c8d8e8f

    b3b4b5b6 b7b8b9ba bb0a0b0c 0d0e0f10
    11121314 15161718 191a1b1c 1d |}]

let%expect_test "packetizer_full_fast_aligned" =
  let module PacketizerFullSim = PacketizerFullSim(TestDataAligned) in
  let module Sim = Sim.Sim(PacketizerFullSim) in
  
  let sim = Sim.create ~name:"packetizer_full_unaligned_aligned" ~gtkwave:false () in

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
    b0b1b2b3 b4b5b6b7 b8b9babb 0d0e0f10
    11121314 15161718 191a1b1c 1d1e1f

    b0b1b2b3 b4b5b6b7 b8b9babb 70717273
    74757677 78797a7b 7c7d7e7f 80818283
    84858687 88898a8b 8c8d8e8f

    b0b1b2b3 b4b5b6b7 b8b9babb 0d0e0f10
    11121314 15161718 191a1b1c 1d |}]

module DepacketizerSim (HeaderData : Interface.S) = struct
  module Header = Transaction.Make(HeaderData)
  module Serializer = Flow.Serializer(HeaderData)

  module I = struct
    type 'a t =
       { clock : 'a
       ; reset : 'a
       ; source_tx : 'a Flow.Base.Src.t [@rtlprefix "source_"]
       ; sink_tx : 'a Flow.Base.Dst.t [@rtlprefix "sink_"]
       ; header : 'a Header.Dst.t [@rtlprefix "hdr_"]
       }
    [@@deriving sexp_of, hardcaml]
  end
  
  module O = struct
    type 'a t =
      { source_rx : 'a Flow.Base.Dst.t [@rtlprefix "source_"]
      ; sink_rx : 'a Flow.Base.Src.t [@rtlprefix "sink_"]
      ; header : 'a Header.Src.t [@rtlprefix "hdr_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create_fn (_scope : Scope.t) (i : Signal.t I.t) : (Signal.t O.t) =
    let spec = Reg_spec.create ~clock:i.clock ~reset:i.reset () in

    let source_rx = Flow.Base.Dst.Of_signal.wires () in
    let sink_rx = Flow.Base.Src.Of_signal.wires () in

    let sink = Flow.Base.t_of_if sink_rx i.sink_tx in
    let source = Flow.Base.t_of_if i.source_tx source_rx in

    let f1, sink2 = Flow.Base.split spec ~hdr_length:Header.data_len ~source in
    let hdr = Serializer.deserialize spec f1 in
    let hdr_s, hdr_d = Header.if_of_t hdr in

    Header.Dst.Of_signal.assign hdr_d i.header;

    Flow.Base.connect sink sink2;
    
    {O.source_rx = source.d;
      sink_rx = sink.s;
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
  module Serializer = Flow.Serializer(HeaderData)

  module I = struct
    type 'a t =
        { clock : 'a
        ; reset : 'a
        ; source_tx : 'a Flow.Base.Src.t [@rtlprefix "source_"]
        ; sink_tx : 'a Flow.Base.Dst.t [@rtlprefix "sink_"]
        ; header : 'a Header.Src.t [@rtlprefix "hdr_"]
        }
    [@@deriving sexp_of, hardcaml]
  end
  
  module O = struct
    type 'a t =
      { source_rx : 'a Flow.Base.Dst.t [@rtlprefix "source_"]
      ; sink_rx : 'a Flow.Base.Src.t [@rtlprefix "sink_"]
      ; header : 'a Header.Dst.t [@rtlprefix "hdr_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create_fn (_scope : Scope.t) (i : Signal.t I.t) : (Signal.t O.t) =
    let spec = Reg_spec.create ~clock:i.clock ~reset:i.reset () in

    let source_rx = Flow.Base.Dst.Of_signal.wires () in
    let sink_rx = Flow.Base.Src.Of_signal.wires () in

    let sink = Flow.Base.t_of_if sink_rx i.sink_tx in
    let source = Flow.Base.t_of_if i.source_tx source_rx in
    
    let hdr = Header.t_of_if i.header (Header.Dst.Of_signal.wires ()) in

    let f1 = Serializer.serialize spec hdr in
    let out_flow = Flow.Base.join spec ~hdr_length:Header.data_len ~source1:f1 ~source2:source in
    Flow.Base.connect sink out_flow;
    
    {O.source_rx = source.d
    ; sink_rx = sink.s
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

module FlowWithHeaderArbiterSim (Data : Interface.S) = struct
  module With_header = Flow.With_header(Data)
  module Header = Transaction.Make(Data)

  module I = struct
    type 'a t =
        { clock : 'a
        ; reset : 'a
        ; source1 : 'a With_header.Src.t [@rtlprefix "source1_"]
        ; source2 : 'a With_header.Src.t [@rtlprefix "source2_"]
        ; source3 : 'a With_header.Src.t [@rtlprefix "source3_"]
        ; sink : 'a With_header.Dst.t [@rtlprefix "sink_"]
        }
    [@@deriving sexp_of, hardcaml]
  end
  
  module O = struct
    type 'a t =
      { source1 : 'a With_header.Dst.t [@rtlprefix "source1_"]
      ; source2 : 'a With_header.Dst.t [@rtlprefix "source2_"]
      ; source3 : 'a With_header.Dst.t [@rtlprefix "source3_"]
      ; sink : 'a With_header.Src.t [@rtlprefix "sink_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create_fn (_scope : Scope.t) (i : Signal.t I.t) : (Signal.t O.t) =
    let spec = Reg_spec.create ~clock:i.clock ~reset:i.reset () in

    let source1 = With_header.t_of_if i.source1 (With_header.Dst.Of_signal.wires ()) in
    let source2 = With_header.t_of_if i.source2 (With_header.Dst.Of_signal.wires ()) in
    let source3 = With_header.t_of_if i.source3 (With_header.Dst.Of_signal.wires ()) in
    let sink = With_header.t_of_if (With_header.Src.Of_signal.wires ()) i.sink in

    With_header.connect sink (With_header.arbitrate spec [source1; source2; source3]);

    { O.source1 = snd (With_header.if_of_t source1)
    ; source2 = snd (With_header.if_of_t source2)
    ; source3 = snd (With_header.if_of_t source3)
    ; sink = fst (With_header.if_of_t sink)
    }

end
    
let%expect_test "flow_with_hdr_arbitrate" =
  let module FlowWithHeaderArbiterSim = FlowWithHeaderArbiterSim(TestData) in
  let module Sim = Sim.Sim(FlowWithHeaderArbiterSim) in
  let module Emitter = FlowWithHeaderEmitter(TestData) in
  let module Consumer = FlowWithHeaderConsumer(TestData) in
  
  let sim = Sim.create ~name:"flow_with_hdr_arbitrate" ~gtkwave:false () ~trace:false in

  let inputs = Sim.inputs sim in
  let outputs = Sim.outputs sim in

  let source1 = Emitter.create inputs.source1 outputs.source1 in
  let source2 = Emitter.create inputs.source2 outputs.source2 in
  let source3 = Emitter.create inputs.source3 outputs.source3 in
  let sink = Consumer.create outputs.sink inputs.sink in

  Sim.add_element sim (module Emitter) source1;
  Sim.add_element sim (module Emitter) source2;
  Sim.add_element sim (module Emitter) source3;
  Sim.add_element sim (module Consumer) sink;

  let create_data data =
    Parsexp.Single.parse_string_exn data |>
    TestData.t_of_sexp String.t_of_sexp |>
    TestData.map2 TestData.port_widths ~f:(fun width x -> Bits.of_hex ~width x)
  in

  Emitter.add_transfer source1 (create_data "((field1 000000000000) (field2 00) (field3 0000))") (FlowEmitter.gen_seq_transfer ~from:0 16);
  Emitter.add_transfer source1 (create_data "((field1 111111111111) (field2 11) (field3 1111))") (FlowEmitter.gen_seq_transfer ~from:16 15);
  Emitter.add_transfer source1 (create_data "((field1 777777777777) (field2 77) (field3 7777))") (FlowEmitter.gen_seq_transfer ~from:112 16);
  Emitter.add_transfer source1 (create_data "((field1 888888888888) (field2 88) (field3 8888))") (FlowEmitter.gen_seq_transfer ~from:128 15);
  
  Emitter.add_transfer source2 (create_data "((field1 222222222222) (field2 22) (field3 2222))") (FlowEmitter.gen_seq_transfer ~from:32 15);
  Emitter.add_transfer source2 (create_data "((field1 333333333333) (field2 33) (field3 3333))") (FlowEmitter.gen_seq_transfer ~from:48 12);
  Emitter.add_transfer source2 (create_data "((field1 444444444444) (field2 44) (field3 4444))") (FlowEmitter.gen_seq_transfer ~from:64 16);

  Emitter.add_transfer source3 (create_data "((field1 555555555555) (field2 55) (field3 5555))") (FlowEmitter.gen_seq_transfer ~from:80 13);
  Emitter.add_transfer source3 (create_data "((field1 666666666666) (field2 66) (field3 6666))") (FlowEmitter.gen_seq_transfer ~from:96 14);

  Sim.cycle_n sim 1;
  source1.enabled <- true;
  source2.enabled <- true;
  source3.enabled <- true;
  sink.enabled <- true;

  Sim.cycle_n sim 6;
  sink.enabled <- false;
  Sim.cycle_n sim 3;
  sink.enabled <- true;
  Sim.cycle_n sim 4;

  source1.enabled <- false;
  Sim.cycle_n sim 5;
  source1.enabled <- true;
  Sim.cycle_n sim 2;
  source1.enabled <- false;

  Sim.cycle_n sim 6;
  sink.enabled <- false;
  Sim.cycle_n sim 3;
  sink.enabled <- true;
  Sim.cycle_n sim 4;

  Sim.cycle_n sim 4;
  sink.enabled <- false;
  Sim.cycle_n sim 3;
  sink.enabled <- true;
  Sim.cycle_n sim 6;
  
  Sim.cycle_n sim 5;
  source1.enabled <- true;
  source2.enabled <- false;
  Emitter.add_transfer source2 (create_data "((field1 222222222222) (field2 22) (field3 2222))") (FlowEmitter.gen_seq_transfer ~from:32 15);
  Emitter.add_transfer source2 (create_data "((field1 333333333333) (field2 33) (field3 3333))") (FlowEmitter.gen_seq_transfer ~from:48 12);

  source3.enabled <- false;
  Emitter.add_transfer source3 (create_data "((field1 555555555555) (field2 55) (field3 5555))") (FlowEmitter.gen_seq_transfer ~from:80 13);
  Emitter.add_transfer source3 (create_data "((field1 666666666666) (field2 66) (field3 6666))") (FlowEmitter.gen_seq_transfer ~from:96 14);

  Sim.cycle_n sim 20;
  source2.enabled <- true;
  source3.enabled <- true;

  Sim.cycle_n sim 50;
  
  Consumer.expect_transfers sink;

  [%expect {|
    (consumed
     (((field1 000000000000) (field2 00) (field3 0000))
      ((field1 222222222222) (field2 22) (field3 2222))
      ((field1 111111111111) (field2 11) (field3 1111))
      ((field1 333333333333) (field2 33) (field3 3333))
      ((field1 555555555555) (field2 55) (field3 5555))
      ((field1 444444444444) (field2 44) (field3 4444))
      ((field1 666666666666) (field2 66) (field3 6666))
      ((field1 777777777777) (field2 77) (field3 7777))
      ((field1 888888888888) (field2 88) (field3 8888))
      ((field1 222222222222) (field2 22) (field3 2222))
      ((field1 555555555555) (field2 55) (field3 5555))
      ((field1 333333333333) (field2 33) (field3 3333))
      ((field1 666666666666) (field2 66) (field3 6666))))
    00010203 04050607 08090a0b 0c0d0e0f

    20212223 24252627 28292a2b 2c2d2e

    10111213 14151617 18191a1b 1c1d1e

    30313233 34353637 38393a3b

    50515253 54555657 58595a5b 5c

    40414243 44454647 48494a4b 4c4d4e4f

    60616263 64656667 68696a6b 6c6d

    70717273 74757677 78797a7b 7c7d7e7f

    80818283 84858687 88898a8b 8c8d8e

    20212223 24252627 28292a2b 2c2d2e

    50515253 54555657 58595a5b 5c

    30313233 34353637 38393a3b

    60616263 64656667 68696a6b 6c6d|}]


module FlowWithHeaderDispatcherSim = struct
  module With_header = Flow.With_header(TestData)
  module Header = Transaction.Make(TestData)

  module I = struct
    type 'a t =
        { clock : 'a
        ; reset : 'a
        ; sink1 : 'a With_header.Dst.t [@rtlprefix "sink1_"]
        ; sink2 : 'a With_header.Dst.t [@rtlprefix "sink2_"]
        ; sink3 : 'a With_header.Dst.t [@rtlprefix "sink3_"]
        ; source : 'a With_header.Src.t [@rtlprefix "source_"]
        }
    [@@deriving sexp_of, hardcaml]
  end
  
  module O = struct
    type 'a t =
      { sink1 : 'a With_header.Src.t [@rtlprefix "sink1_"]
      ; sink2 : 'a With_header.Src.t [@rtlprefix "sink2_"]
      ; sink3 : 'a With_header.Src.t [@rtlprefix "sink3_"]
      ; source : 'a With_header.Dst.t [@rtlprefix "source_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create_fn (_scope : Scope.t) (i : Signal.t I.t) : (Signal.t O.t) =
    let spec = Reg_spec.create ~clock:i.clock ~reset:i.reset () in

    let sink1 = With_header.t_of_if (With_header.Src.Of_signal.wires ()) i.sink1 in
    let sink2 = With_header.t_of_if (With_header.Src.Of_signal.wires ()) i.sink2 in
    let sink3 = With_header.t_of_if (With_header.Src.Of_signal.wires ()) i.sink3 in
    let source = With_header.t_of_if i.source (With_header.Dst.Of_signal.wires ()) in

    let sinks = With_header.dispatch spec source ~selector:(fun data ->
      let open Signal in
      let s1 = data.field1 <=:. 256 in
      let s2 = (data.field1 >:. 256) &: (data.field2 ==:. 16) in 
      let s3 = (data.field1 >:. 256) &: (data.field3 ==:. 8) in

      concat_msb [s3; s2; s1]
    ) in

    With_header.connect sink1 (List.nth_exn sinks 0);
    With_header.connect sink2 (List.nth_exn sinks 1);
    With_header.connect sink3 (List.nth_exn sinks 2);

    { O.sink1 = fst (With_header.if_of_t sink1)
    ; sink2 = fst (With_header.if_of_t sink2)
    ; sink3 = fst (With_header.if_of_t sink3)
    ; source = snd (With_header.if_of_t source)
    }

end
    
let%expect_test "flow_with_hdr_dispatcher" =
  let module Sim = Sim.Sim(FlowWithHeaderDispatcherSim) in
  let module Emitter = FlowWithHeaderEmitter(TestData) in
  let module Consumer = FlowWithHeaderConsumer(TestData) in
  
  let sim = Sim.create ~name:"flow_with_hdr_dispatcher" ~gtkwave:false () ~trace:false in

  let inputs = Sim.inputs sim in
  let outputs = Sim.outputs sim in

  let sink1 = Consumer.create outputs.sink1 inputs.sink1 in
  let sink2 = Consumer.create outputs.sink2 inputs.sink2 in
  let sink3 = Consumer.create outputs.sink3 inputs.sink3 in
  let source = Emitter.create inputs.source outputs.source in

  Sim.add_element sim (module Consumer) sink1;
  Sim.add_element sim (module Consumer) sink2;
  Sim.add_element sim (module Consumer) sink3;
  Sim.add_element sim (module Emitter) source;

  let create_data data =
    Parsexp.Single.parse_string_exn data |>
    TestData.t_of_sexp String.t_of_sexp |>
    TestData.map2 TestData.port_widths ~f:(fun width x -> Bits.of_hex ~width x)
  in

  Emitter.add_transfer source (create_data "((field1 000000000000) (field2 12) (field3 3123))") (FlowEmitter.gen_seq_transfer ~from:0 16);
  Emitter.add_transfer source (create_data "((field1 111111111111) (field2 10) (field3 1111))") (FlowEmitter.gen_seq_transfer ~from:16 15);
  Emitter.add_transfer source (create_data "((field1 777777777777) (field2 77) (field3 7777))") (FlowEmitter.gen_seq_transfer ~from:112 16);
  Emitter.add_transfer source (create_data "((field1 000000000000) (field2 10) (field3 8888))") (FlowEmitter.gen_seq_transfer ~from:128 15);
  Emitter.add_transfer source (create_data "((field1 111111111111) (field2 10) (field3 1111))") (FlowEmitter.gen_seq_transfer ~from:32 12);
  Emitter.add_transfer source (create_data "((field1 111111111111) (field2 11) (field3 0008))") (FlowEmitter.gen_seq_transfer ~from:48 13);
  Emitter.add_transfer source (create_data "((field1 000000000000) (field2 11) (field3 0008))") (FlowEmitter.gen_seq_transfer ~from:64 14);
  Emitter.add_transfer source (create_data "((field1 000000000000) (field2 55) (field3 5555))") (FlowEmitter.gen_seq_transfer ~from:80 15);
  Emitter.add_transfer source (create_data "((field1 000000000000) (field2 66) (field3 6666))") (FlowEmitter.gen_seq_transfer ~from:96 20);

  Sim.cycle_n sim 1;

  source.enabled <- true;
  sink1.enabled <- true;
  sink2.enabled <- true;
  sink3.enabled <- true;

  Sim.cycle_n sim 6;
  sink1.enabled <- false;
  Sim.cycle_n sim 3;
  sink1.enabled <- true;
  Sim.cycle_n sim 4;

  source.enabled <- false;
  Sim.cycle_n sim 5;
  source.enabled <- true;
  Sim.cycle_n sim 2;
  source.enabled <- false;

  Sim.cycle_n sim 6;
  sink2.enabled <- false;
  Sim.cycle_n sim 3;
  sink2.enabled <- true;
  Sim.cycle_n sim 4;

  Sim.cycle_n sim 4;
  sink3.enabled <- false;
  Sim.cycle_n sim 3;
  sink3.enabled <- true;
  Sim.cycle_n sim 6;
  
  Sim.cycle_n sim 5;
  source.enabled <- true;

  Sim.cycle_n sim 50;
  
  Consumer.expect_transfers sink1;
  Consumer.expect_transfers sink2;
  Consumer.expect_transfers sink3;

  [%expect {|
    (consumed
     (((field1 000000000000) (field2 12) (field3 3123))
      ((field1 000000000000) (field2 10) (field3 8888))
      ((field1 000000000000) (field2 11) (field3 0008))
      ((field1 000000000000) (field2 55) (field3 5555))
      ((field1 000000000000) (field2 66) (field3 6666))))
    00010203 04050607 08090a0b 0c0d0e0f

    80818283 84858687 88898a8b 8c8d8e

    40414243 44454647 48494a4b 4c4d

    50515253 54555657 58595a5b 5c5d5e

    60616263 64656667 68696a6b 6c6d6e6f
    70717273

    (consumed
     (((field1 111111111111) (field2 10) (field3 1111))
      ((field1 111111111111) (field2 10) (field3 1111))))
    10111213 14151617 18191a1b 1c1d1e

    20212223 24252627 28292a2b

    (consumed (((field1 111111111111) (field2 11) (field3 0008))))
    30313233 34353637 38393a3b 3c|}]
