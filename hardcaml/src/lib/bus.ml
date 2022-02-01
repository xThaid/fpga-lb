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

    val t_of_if : Signal.t I.t -> Signal.t O.t -> t
    val if_of_t : t -> Signal.t I.t * Signal.t O.t
    val create_wires : unit -> t

    val connect : t -> t -> unit

    val on_write : t -> (int * (Signal.t -> Always.t list)) list -> Always.t
    val read_from : Signal.register -> t -> (int * Signal.t) list -> unit

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

    let t_of_if (i : Signal.t I.t) (o : Signal.t O.t) = {i; o}
    let if_of_t (t : t) =  t.i, t.o
    let create_wires () = t_of_if (I.Of_signal.wires ()) (O.Of_signal.wires ())

    let connect t1 t2 = 
      I.Of_signal.assign t1.i t2.i;
      O.Of_signal.assign t2.o t1.o

    let on_write (t : t) (cases : (int * (Signal.t -> Always.t list)) list) =
      let open Signal in
  
      Always.(proc [
        when_ (t.i.write &: ((~:) t.o.waitrequest)) [
          switch t.i.address (
            List.map cases ~f:(fun (addr, ff) -> Signal.of_int ~width:D.addr_len addr, (ff t.i.writedata))
          )
        ]
      ])

    let read_from spec t (cases : (int * Signal.t) list) = 
      let open Signal in

      let readdata = Always.Variable.wire ~default:(zero 32) in

      Always.(compile [
        when_ (t.i.read &: ((~:) t.o.waitrequest)) [
          switch t.i.address (
            List.map cases ~f:(fun (addr, data) -> Signal.of_int ~width:D.addr_len addr, [readdata <-- data])
          )
        ]
      ]);

      t.o.readdata <== reg spec readdata.value

  end
end

module Interconnect = struct
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

  let add_agent t agent addr_base addr_end =
    t.agents <- {AgentInfo.inst = agent; addr_base; addr_end} :: t.agents

  let complete_comb t spec = 
    let open Signal in

    let module HostInst = (val t.host) in
    let module HostAgent = HostInst.Agent in

    let agent_cnt = List.length t.agents in
    let sorted = List.sort t.agents ~compare:(fun x y -> Int.compare x.addr_base y.addr_base) in

    let sel_onehot = List.init agent_cnt ~f:(fun _ -> wire 1) in
    let sel = onehot_to_binary (concat_lsb sel_onehot) in
    let sel_d = reg spec sel in

    let host = HostInst.this in
    let host_in, host_out = HostAgent.if_of_t host in

    List.iteri sorted ~f:(fun i info ->
      let module Inst = (val info.inst) in
      let module Agent = Inst.Agent in

      let ins, _ = Agent.if_of_t Inst.this in

      let selected = (host_in.address <=:. info.addr_end) &: (host_in.address >=:. info.addr_base) in
      (List.nth_exn sel_onehot i) <== selected;

      ins.writedata <== host_in.writedata;
      ins.address <== sel_bottom (host_in.address -:. info.addr_base) Agent.addr_len;
      ins.write <== (host_in.write &: selected);
      ins.read <== (host_in.read &: selected);
    );

    let slaves_readdata = List.map sorted ~f:(fun info -> let module I = (val info.inst) in (snd (I.Agent.if_of_t I.this)).readdata) in
    let slaves_waitrequest = List.map sorted ~f:(fun info -> let module I = (val info.inst) in (snd (I.Agent.if_of_t I.this)).waitrequest) in

    if List.length t.agents = 1 then (
      host_out.waitrequest <== List.hd_exn slaves_waitrequest;
      host_out.readdata <== List.hd_exn slaves_readdata
    ) else (
      host_out.waitrequest <== mux sel slaves_waitrequest;
      host_out.readdata <== mux sel_d slaves_readdata
    )

end
