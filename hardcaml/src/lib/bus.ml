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

    let create_empty () = {i = I.Of_signal.wires (); o = O.Of_signal.wires ()}

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
      { addr_base : int
      ; addr_end : int
      ; mutable agents : AgentInfo.t list
      }

    let create addr_base addr_end = 
      { addr_base
      ; addr_end
      ; agents = []
      }

    let add_agent t (module A : Agent.S) addr_base addr_end =

      let agent = A.create_empty () in
      let inst = Agent.build (module A) agent in

      let agent_info = {AgentInfo.inst = inst; addr_base; addr_end} in

      t.agents <- agent_info :: t.agents;

      inst

    let complete t = 
      let open Signal in

      let module MasterAgent = Agent.Make (struct let addr_len = num_bits_to_represent t.addr_end end) in

      let master = MasterAgent.create_empty () in
      let master_inst = Agent.build (module MasterAgent) master in

      master_inst

  end
end