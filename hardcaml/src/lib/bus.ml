open Base
open Hardcaml

module Agent = struct
  module type Desc = sig
    val addr_len : int
  end

  module type S = sig
    include Desc

    type t

    module I : sig
      type 'a t =
        { address : 'a
        ; writedata : 'a
        ; read : 'a
        ; write : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O : sig
      type 'a t =
        { waitrequest : 'a
        ; readdata : 'a
        }
    [@@deriving sexp_of, hardcaml]
    end

    val create : Signal.t I.t -> Signal.t O.t -> t
    val create_empty : unit -> t

    val inputs : t -> Signal.t I.t
    val outputs : t -> Signal.t O.t

    val on_write : t -> (int * (Signal.t -> Always.t list)) list -> Always.t

  end

  module type Inst = sig
    module Agent : S
    val this : Agent.t
  end

  let build (type a) (module Agent : S with type t = a) (inst : a) =
    (module struct
      module Agent = Agent
      let this = inst
    end : Inst)

  module Make (D : Desc) : S = struct
    include D

    module I = struct
      type 'a t =
        { address : 'a [@bits D.addr_len]
        ; writedata : 'a [@bits 32]
        ; read : 'a
        ; write : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end
  
    module O = struct
      type 'a t =
        { waitrequest : 'a
        ; readdata : 'a [@bits 32]
        }
    [@@deriving sexp_of, hardcaml]
    end

    type t =
      { i : Signal.t I.t
      ; o : Signal.t O.t
      }

    let create (i : Signal.t I.t) (o : Signal.t O.t) = {i; o}
    let create_empty () = create (I.Of_signal.wires ()) (O.Of_signal.wires ())

    let inputs t = t.i
    let outputs t = t.o

    let on_write (t : t) (cases : (int * (Signal.t -> Always.t list)) list) =
      let open Signal in
  
      Always.(proc [
        when_ (t.i.write &: ((~:) t.o.waitrequest)) [
          switch t.i.address (
            List.map cases ~f:(fun (addr, ff) -> Signal.of_int ~width:D.addr_len addr, (ff t.i.writedata))
          )
        ]
      ])
  end
end

module Interconnect = struct
  module Builder = struct
    module AgentInfo = struct
      type t =
        { inst : (module Agent.Inst)
        ; addr_base : int
        ; addr_end : int
        }
    end

    type t = 
      { host : (module Agent.Inst)
      ; mutable agents : AgentInfo.t list
      }

    let create host = 
      { host
      ; agents = []
      }

    let add_agent (type a) t (module A : Agent.S with type t = a) addr_base addr_end =
      let agent = A.create_empty () in
      let agent_ins = A.I.Of_signal.apply_names ~prefix:(Int.to_string addr_base) (A.inputs agent) in
      let agent_out = A.O.Of_signal.apply_names ~prefix:(Int.to_string addr_base) (A.outputs agent) in
      let agent = A.create agent_ins agent_out in

      let inst = Agent.build (module A) agent in

      let agent_info = {AgentInfo.inst = inst; addr_base; addr_end} in

      t.agents <- agent_info :: t.agents;

      agent

    let complete_comb spec t = 
      let open Signal in

      let module HostInst = (val t.host) in
      let module HostAgent = HostInst.Agent in

      let agent_cnt = List.length t.agents in
      let sorted = List.sort t.agents ~compare:(fun x y -> Int.compare x.addr_base y.addr_base) in

      let sel_onehot = List.init agent_cnt ~f:(fun _ -> wire 1) in
      let sel = onehot_to_binary (concat_lsb sel_onehot) -- "sel" in
      let sel_d = reg spec sel -- "sel_d" in

      let host = HostInst.this in
      let host_in = HostAgent.inputs host in
      let host_out = HostAgent.outputs host in

      List.iteri sorted ~f:(fun i info ->
        let module Inst = (val info.inst) in
        let module Agent = Inst.Agent in

        let ins = Agent.inputs Inst.this in

        (List.nth_exn sel_onehot i) <== ((host_in.address <=:. info.addr_end) &: (host_in.address >=:. info.addr_base));
        
        let selected = sel ==:. i in

        ins.writedata <== host_in.writedata;
        ins.address <== sel_bottom (host_in.address -:. info.addr_base) Agent.addr_len;
        ins.write <== (host_in.write &: selected);
        ins.read <== (host_in.read &: selected);
      );

      let slaves_readdata = List.map sorted ~f:(fun info -> let module I = (val info.inst) in (I.Agent.outputs I.this).readdata) in
      let slaves_waitrequest = List.map sorted ~f:(fun info -> let module I = (val info.inst) in (I.Agent.outputs I.this).waitrequest) in

      host_out.waitrequest <== mux sel slaves_waitrequest;
      host_out.readdata <== mux sel_d slaves_readdata 

  end
end
