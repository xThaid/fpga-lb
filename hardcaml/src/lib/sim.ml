open Base
open Hardcaml
open Hardcaml_waveterm

module type SimElement = sig
  type config
  type t

  val create : config -> t
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

  val create_fn : Signal.t I.t -> Signal.t O.t
end

module Sim (S : S) = struct
  type t =
    { name : string
    ; elements : (module SimElement_instance) list ref
    ; sim : (Bits.t ref S.I.t, Bits.t ref S.O.t) Cyclesim.t
    ; waves : Waveform.t
    }

  let create ~name ?(gtkwave = false) () = 
    let module CSim = Cyclesim.With_interface (S.I)(S.O) in
    let sim = CSim.create S.create_fn in

    let sim = if gtkwave then
       Vcd.Gtkwave.gtkwave ~args:("--save=../../../test/res/" ^ name ^ ".gtkw") sim
    else
      sim
    in

    let waves, sim = Waveform.create sim in

    {name;
     elements = ref [];
     sim;
     waves;
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

    elements_comb ();
    Cyclesim.cycle_after_clock_edge t.sim

  let cycle_n t n = 
    for _ = 0 to n - 1 do
      cycle t
    done

  let expect_waves t =
    Hardcaml_waveterm.Waveform.expect ~serialize_to:"header_disassemble_unaligned" t.waves

end