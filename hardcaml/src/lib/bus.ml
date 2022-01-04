open Base
open Hardcaml

module type AgentDesc = sig
  val addr_len : int
end

module Agent (Desc : AgentDesc) = struct
  module I = struct
    type 'a t =
      { address : 'a [@bits Desc.addr_len]
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
  
  type 'a t = 
    { i : 'a I.t
    ; o : 'a O.t
    }

  let on_write (t : 'a t) (cases : (int * (Signal.t -> Always.t list)) list) =
    let open Signal in

    Always.(proc [
      when_ (t.i.write &: ((~:) t.o.waitrequest)) [
        switch t.i.address (
          List.map cases ~f:(fun (addr, ff) -> Signal.of_int ~width:Desc.addr_len addr, (ff t.i.writedata))
        )
      ]
    ])

end
