
module Source = struct 
  type 'a t =
    { valid : 'a
    ; last : 'a
    ; data : 'a [@bits 32]
    ; empty : 'a [@bits 2]
    }
  [@@deriving sexp_of, hardcaml]
end

module Dest = struct 
  type 'a t =
    { ready : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module Endpoint = struct 
  type 'a t = 
  { src : 'a Source.t
  ; dst : 'a Dest.t
  }

  let create src dst = 
    { src;
      dst;
    }

  let create_empty () =
    { src = Source.Of_signal.wires ();
      dst = Dest.Of_signal.wires ();
    }
end


