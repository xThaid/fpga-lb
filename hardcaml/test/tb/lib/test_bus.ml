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

  let create_agent (type a) (module A : Bus.Agent.S with type t = a) spec off enabled =
    let open Signal in

    let agent = A.create_wires () in
    let i, o = A.if_of_t agent in

    let readdata = Always.Variable.reg ~width:32 spec in

    Always.(compile [
      when_ (i.read) [
        readdata <-- ((uresize i.address 32) +:. off);
      ]
    ]);

    o.waitrequest <== (Signal.of_bool (not enabled));
    o.readdata <== readdata.value;

    Bus.Agent.build (module A) agent

  let create_fn (_scope : Scope.t) (i : Signal.t I.t) : (Signal.t O.t) =
    let module IC = Bus.Interconnect in

    let spec = Reg_spec.create ~clock:i.clock ~reset:i.reset () in

    let host = BusHost.t_of_if i.host (BusHost.O.Of_signal.wires ()) in

    let ic = IC.create (Bus.Agent.build (module BusHost) host) in

    let agent_a = create_agent (module BusAgent2) spec 100 true in
    let agent_b = create_agent (module BusAgent2) spec 1000 true in
    let agent_c = create_agent (module BusAgent3) spec 10000 true in

    IC.add_agent ic agent_a 0 3;
    IC.add_agent ic agent_b 4 6;
    IC.add_agent ic agent_c 7 13;

    IC.complete_comb ic spec;

    {O.host = snd (BusHost.if_of_t host)
    }

end

let%expect_test "bus_interconnect_comb" =
  let module Sim = Sim.Sim (InterconnectSim) in
  let module Host = Sim_elements.BusHost (InterconnectSim.BusHost) in
  let module Agent2 = Sim_elements.BusAgent (InterconnectSim.BusAgent2) in
  let module Agent3 = Sim_elements.BusAgent (InterconnectSim.BusAgent3) in
    
  let sim = Sim.create ~name:"bus_interconnect_comb" ~gtkwave:false () ~trace:false in

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
  Host.schedule_read host 13;

  Sim.cycle_n sim 15;

  Host.expect_responses host;

  Sim.expect_trace_digest sim;

  [%expect {|
    (responses (103 1000 1002 10006 10006))
    4fbba1d862eaafed48d8769ced8fe220 |}]


