open Hardcaml
open Lb_dataplane
module DataplaneCircuit = Circuit.With_interface (Dataplane.I) (Dataplane.O)

let scope = Scope.create ()
let circuit = DataplaneCircuit.create_exn ~name:"dataplane" (Dataplane.create_from_if scope)
let output_mode = Rtl.Output_mode.To_file("bin/dataplane.v")

let () = Rtl.output ~output_mode ~database:(Scope.circuit_database scope) Verilog circuit
