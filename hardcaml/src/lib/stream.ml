open !Base
open Hardcaml

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

module Endpoint (Params : Interface.S) = struct 
  module SourceToSink = struct 
    type 'a t =
      { valid : 'a
      ; last : 'a
      ; data : 'a[@bits 32]
      ; empty : 'a[@bits 2]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module SinkToSource = struct 
    type 'a t =
      { ready : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  type 'a t = 
  { src2si : 'a SourceToSink.t
  ; si2src : 'a SinkToSource.t
  ; params : 'a Params.t
  }

  let create () = 
    { src2si = SourceToSink.Of_signal.wires ();
      si2src = SinkToSource.Of_signal.wires ();
      params = Params.Of_signal.wires ();
    }

  let create_named ?(prefix = "") ?(suffix = "") () = 
    let wires = create () in
    { src2si = SourceToSink.Of_signal.apply_names wires.src2si ~prefix ~suffix;
      si2src = SinkToSource.Of_signal.apply_names wires.si2src ~prefix ~suffix;
      params = Params.Of_signal.apply_names wires.params ~prefix ~suffix;
    }

  let source_outputs t = 
    SinkToSource.to_list t.si2src

  let sink_outputs t = 
    (SourceToSink.to_list t.src2si) @ (Params.to_list t.params)

  let connect sink source = 
    SourceToSink.Of_signal.(sink.src2si <== source.src2si);
    SinkToSource.Of_signal.(source.si2src <== sink.si2src);
    Params.Of_signal.(sink.params <== source.params)

  let ( <== ) = connect
end


