open Hardcaml
open Lb_dataplane
module DataplaneCircuit = Circuit.With_interface (Dataplane.I) (Dataplane.O)
module LoadGeneratorCircuit = Circuit.With_interface (Load_generator.I) (Load_generator.O)

let gen_dataplane () = 
  let scope = Scope.create () in
  let circuit = DataplaneCircuit.create_exn ~name:"lb_dataplane" (Dataplane.create_from_if scope) in
  let output_mode = Rtl.Output_mode.To_file("out/dataplane.v") in

  Rtl.output ~output_mode ~database:(Scope.circuit_database scope) Verilog circuit

let gen_load_generator () = 
  let scope = Scope.create ~flatten_design:true () in
  let circuit = LoadGeneratorCircuit.create_exn ~name:"load_generator" (Load_generator.create_from_if scope) in
  let output_mode = Rtl.Output_mode.To_file("out/load_generator.v") in

  Rtl.output ~output_mode ~database:(Scope.circuit_database scope) Verilog circuit

let () = 
  gen_dataplane ();
  gen_load_generator ()
