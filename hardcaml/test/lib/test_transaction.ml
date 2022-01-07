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
  [@@deriving sexp_of, hardcaml]
end

module SerializerSim (Data : Interface.S) = struct
  module Serializer = Transaction.Serializer(Data)
  module Transaction = Transaction.Transaction(Data)

  module I = struct
    type 'a t =
        { clock : 'a
        ; reset : 'a
        ; sink_tx : 'a Flow.Dest.t [@rtlprefix "sink_"]
        ; tst : 'a Transaction.t [@rtl_prefix "tst_"]
        }
    [@@deriving sexp_of, hardcaml]
  end
  
  module O = struct
    type 'a t =
      { sink_rx : 'a Flow.Source.t [@rtlprefix "sink_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create_fn (_scope : Scope.t) (i : Signal.t I.t) : (Signal.t O.t) =
    let spec = Reg_spec.create ~clock:i.clock ~reset:i.reset () in

    let sink_rx = Flow.Source.Of_signal.wires () in

    let sink = Flow.Endpoint.create sink_rx i.sink_tx in

    Flow.Endpoint.connect sink (Serializer.serialize spec i.tst);
    
    {O.sink_rx = sink.src;}

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

  consumer.enable := false;
  Sim.cycle_n sim 2;
  consumer.enable := true;
  Sim.cycle_n sim 1;
  inputs.tst.valid := Bits.vdd;
  Sim.cycle_n sim 1;
  inputs.tst.valid := Bits.gnd;
  Sim.cycle_n sim 1;
  consumer.enable := false;
  Sim.cycle_n sim 1;
  consumer.enable := true;
  Sim.cycle_n sim 1;
  consumer.enable := false;
  Sim.cycle_n sim 1;
  consumer.enable := true;

  inputs.tst.data.field2 := (Bits.of_hex ~width:8 "a6");
  inputs.tst.valid := Bits.vdd;
  Sim.cycle_n sim 1;
  inputs.tst.valid := Bits.gnd;

  Sim.cycle_n sim 10;

  FlowConsumer.expect_data consumer;
  Sim.expect_trace_digest sim;

  [%expect {|
    f0f1f2f3 f4f5f6f7 f8

    f0f1f2f3 f4f5a6f7 f8

    3b8290e81e70b62b635fba12fab45640|}]
