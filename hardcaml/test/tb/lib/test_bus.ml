open !Base
open Hardcaml
open Lb_dataplane
open Lb_dataplane_test_lib

module InterconnectSim = struct
  module BusHost = Bus.Agent.Make (struct let addr_len = 4 end)
  module BusAgent2 = Bus.Agent.Make (struct let addr_len = 2 end)
  module BusAgent3 = Bus.Agent.Make (struct let addr_len = 3 end)

  module I = struct
    type 'a t =
        { clock : 'a
        ; reset : 'a
        ; host : 'a BusHost.I.t
        }
    [@@deriving sexp_of, hardcaml ~rtlmangle:true]
  end
  
  module O = struct
    type 'a t =
      { host : 'a BusHost.O.t
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:true]
  end

  let create_agent (type a) (module A : Bus.Agent.S with type t = a) spec agent off enabled =
    let open Signal in

    let i = A.inputs agent in
    let o = A.outputs agent in

    let readdata = Always.Variable.reg ~width:32 spec in

    Always.(compile [
      when_ (i.read) [
        readdata <-- ((uresize i.address 32) +:. off);
      ]
    ]);

    o.waitrequest <== (Signal.of_bool (not enabled));
    o.readdata <== readdata.value

  let create_fn (_scope : Scope.t) (i : Signal.t I.t) : (Signal.t O.t) =
    let module B = Bus.Interconnect.Builder in

    let spec = Reg_spec.create ~clock:i.clock ~reset:i.reset () in

    let host = BusHost.create i.host (BusHost.O.Of_signal.wires ()) in

    let builder = B.create (Bus.Agent.build (module BusHost) host) in

    let agent_a = B.add_agent builder (module BusAgent2) 0 3 in
    let agent_b = B.add_agent builder (module BusAgent2) 4 6 in
    let agent_c = B.add_agent builder (module BusAgent3) 7 13 in

    create_agent (module BusAgent2) spec agent_a 100 true;
    create_agent (module BusAgent2) spec agent_b 1000 true;
    create_agent (module BusAgent3) spec agent_c 10000 true;

    B.complete_comb spec builder;

    {O.host = BusHost.outputs host
    }

end

let%expect_test "bus_interconnect_comb" =
  let module Sim = Sim.Sim (InterconnectSim) in
  let module Host = Sim_elements.BusHost (InterconnectSim.BusHost) in
  let module Agent2 = Sim_elements.BusAgent (InterconnectSim.BusAgent2) in
  let module Agent3 = Sim_elements.BusAgent (InterconnectSim.BusAgent3) in
    
  let sim = Sim.create ~name:"bus_interconnect_comb" ~gtkwave:false () ~trace:true in

  let inputs = Sim.inputs sim in
  let outputs = Sim.outputs sim in

  let host = Host.create outputs.host inputs.host in

  Sim.add_element sim (module Host) host;

  Sim.cycle sim;

  Host.schedule_read host 3;
  Host.schedule_read host 4;
  Host.schedule_write host 0 4;
  Host.schedule_read host 6;
  Host.schedule_read host 13;
  Host.schedule_read host 14;

  Sim.cycle_n sim 15;

  Host.expect_responses host;

  Sim.expect_trace_digest sim;

  [%expect {|
    ("List.rev t.responses" (103 1000 1002 10006 102))
    8e5a6a8872e00ee48a0d34035a559e37 |}]


