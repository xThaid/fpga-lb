open Base
open Hardcaml
open Lb_dataplane
open !Expect_test_helpers_base

module TestHeader = struct 
  type 'a t =
    { field1 : 'a[@bits 48]
    ; field2 : 'a[@bits 48]
    ; field3 : 'a[@bits 16]
    }
  [@@deriving sexp_of, hardcaml]
end

let create_circuit () = 
  let module Endpoint = Stream.Endpoint(Interface.Empty) in
  let module Depacketizer = Packet.Depacketizer(Interface.Empty)(TestHeader) in
  let module Header = Packet.Header(TestHeader) in

  let clock = Signal.input "clock" 1 in
  let reset = Signal.input "reset" 1 in
  let reg_spec = Reg_spec.create ~clock ~reset () in

  let source = Endpoint.create_named ~prefix:"source_" () in
  let sink = Endpoint.create_named ~prefix:"sink_" () in
  
  let hdr = Header.Of_signal.outputs (Depacketizer.create reg_spec sink source) in

  let outputs = Header.to_list hdr @ Endpoint.sink_outputs sink @ Endpoint.source_outputs source in
  Circuit.create_exn ~name:"depacketizer_test" outputs

let%expect_test "tt" =
  let circuit = create_circuit () in
  let output_mode = Rtl.Output_mode.To_file("/home/thaid/uni/fpga/fpga-lb/hardcaml/bin/test.v") in
  Rtl.output ~output_mode Verilog circuit;
  let result = 3 in
  print_s [%message (result : int)];
  [%expect {||}]
