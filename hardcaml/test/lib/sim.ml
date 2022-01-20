open Base
open Hardcaml

module type SimElement = sig
  type t

  val comb : t -> unit
  val seq : t -> unit 
end

module type SimElement_instance = sig
  module SimElement : SimElement
  val this : SimElement.t
end

module type S = sig
  module I : Interface.S
  module O : Interface.S

  val create_fn : Scope.t -> Signal.t I.t -> Signal.t O.t
end

module Sim (S : S) = struct
  type t =
    { name : string
    ; elements : (module SimElement_instance) list ref
    ; sim : (Bits.t ref S.I.t, Bits.t ref S.O.t) Cyclesim.t
    }

  let create ~name ?(gtkwave = false) ?gtkwave_name ?(verilator = false) ?(trace = false) () = 
    let module CSim = Cyclesim.With_interface (S.I)(S.O) in
    let module VSim = Hardcaml_verilator.With_interface (S.I)(S.O) in

    let scope = Scope.create ~flatten_design:true () in
    let create_fn = S.create_fn scope in

    let sim = 
      if verilator then
        VSim.create ~clock_names:[ "clock" ] create_fn
      else
        CSim.create ~config:(Cyclesim.Config.trace trace) create_fn
    in

    let gtkwave_name = match gtkwave_name with
      | None -> name
      | Some name -> name
    in

    let sim = if gtkwave then
       Vcd.Gtkwave.gtkwave ~args:("--save=../../../../test/res/" ^ gtkwave_name ^ ".gtkw") sim
    else
      sim
    in

    {name;
     elements = ref [];
     sim;
    }

  let add_element (type a) t (module Q : SimElement with type t = a) inst =
    let elem = 
      (module struct
        module SimElement = Q
        let this = inst
      end : SimElement_instance)
    in
    t.elements := elem :: !(t.elements)

  let inputs t = Cyclesim.inputs t.sim
  let outputs t = Cyclesim.outputs t.sim

  let cycle t =
    let elements_seq () = List.iter !(t.elements) ~f:(fun (module I : SimElement_instance) -> I.SimElement.seq I.this) in
    let elements_comb () = List.iter !(t.elements) ~f:(fun (module I : SimElement_instance) -> I.SimElement.comb I.this) in

    Cyclesim.cycle_check t.sim;

    elements_comb ();
    Cyclesim.cycle_before_clock_edge t.sim;

    elements_seq ();
    Cyclesim.cycle_at_clock_edge t.sim;

    Cyclesim.cycle_after_clock_edge t.sim

  let cycle_n t n = 
    for _ = 0 to n - 1 do
      cycle t
    done

  let expect_trace_digest t =
    let digest = Cyclesim.digest t.sim in
    Stdio.print_endline (Sexp.to_string_mach (Hardcaml.Cyclesim.Digest.sexp_of_t !(digest)))

  let output_verilog t =
    let module C = Circuit.With_interface (S.I) (S.O) in

    let scope = Scope.create () in
    let circuit = C.create_exn ~name:t.name (S.create_fn scope) in

    let output_mode = Rtl.Output_mode.To_file("../../../bin/" ^ t.name ^ ".v") in

    Rtl.output ~output_mode ~database:(Scope.circuit_database scope) Verilog circuit

end
