open Base
open Hardcaml

module type S = sig
  module D : Interface.S

  module Src : sig
    type 'a t =
      { valid : 'a
      ; data : 'a D.t
      }
  [@@deriving sexp_of, hardcaml]
  end

  module Dst : sig
    type 'a t =
      { ready : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  type t
  type data_type

  val create_wires : unit -> t
  val create : valid:Signal.t -> data:data_type -> t

  val valid : t -> Signal.t
  val data : t -> data_type
  val ready : t -> Signal.t

  val fork : t -> t * t
end

module Make (Data : Interface.S) = struct
  module D = Data

  module Src = struct
    type 'a t =
      { valid : 'a
      ; data : 'a D.t
      }
  [@@deriving sexp_of, hardcaml]
  end

  module Dst = struct
    type 'a t =
      { ready : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  type t =
    { s : Signal.t Src.t 
    ; d : Signal.t Dst.t
    }

  type data_type = Signal.t D.t

  let data_len = List.reduce_exn Data.Names_and_widths.port_widths ~f:(+)

  let t_of_if (s : Signal.t Src.t) (d : Signal.t Dst.t) = {s; d}
  let if_of_t (t : t) =  t.s, t.d
  let inputs (t : t) = t.s
  let outputs (t : t) = t.d
  let create_wires () = t_of_if (Src.Of_signal.wires ()) (Dst.Of_signal.wires ())
  let create ~valid ~data = t_of_if {Src.valid; data} (Dst.Of_signal.wires ())
  let create_empty () = create ~valid:Signal.gnd ~data:(Data.Of_signal.of_int 0)

  let apply_names ?prefix ?suffix t =
    let src = Src.Of_signal.apply_names ?prefix ?suffix t.s in
    let dst = Dst.Of_signal.apply_names ?prefix ?suffix t.d in
    t_of_if src dst

  let connect t1 t2 =
    Src.Of_signal.assign t1.s t2.s;
    Dst.Of_signal.assign t2.d t1.d

  let is_fired t = Signal.(t.s.valid &: t.d.ready)
  let is_stalled t = Signal.(t.s.valid &: ~:(t.d.ready))
  let ready t = t.d.ready
  let valid t = t.s.valid
  let data t = t.s.data

  let map_comb t ~f =
    t_of_if {Src.valid = t.s.valid; data = f t.s.data} t.d
  let filter_comb t ~f =
    let open Signal in
    let filtered = f t.s.data in
    let new_tst = create ~valid:(t.s.valid &: filtered) ~data:t.s.data in
    t.d.ready <== (new_tst.d.ready |: ~:(filtered));
    new_tst

  let gate t ~enable =
    let open Signal in
    let tst = create ~valid:(t.s.valid &: enable) ~data:t.s.data in
    t.d.ready <== (tst.d.ready &: enable);
    tst
  
  let drop t =
    let open Signal in
    t.d.ready <== vdd

  let fork t =
    let open Signal in

    let tst1 = create_wires () in
    let tst2 = create_wires () in
    tst1.s.valid <== (t.s.valid &: tst2.d.ready);
    tst2.s.valid <== (t.s.valid &: tst1.d.ready);
    t.d.ready <== (tst1.d.ready &: tst2.d.ready);

    Data.Of_signal.assign tst1.s.data t.s.data;
    Data.Of_signal.assign tst2.s.data t.s.data;
    
    tst1, tst2

  let demux n tst ~f = 
    let open Signal in

    let sel = f tst.s.data in

    if (Signal.width sel) <> (Signal.address_bits_for n) then
      raise_s [%message "sel signal in demultiplexer has incorrect width"];

    let tsts = List.init n ~f:(fun i ->
      create ~valid:(tst.s.valid &: (sel ==:. i)) ~data:tst.s.data
    ) in

    tst.d.ready <== mux sel (List.map tsts ~f:(fun t -> t.d.ready));

    tsts

  let demux2_on tst ~f =
    let tsts = demux 2 tst ~f in
    List.nth_exn tsts 1, List.nth_exn tsts 0

  let apply tst ~(f : (valid:Signal.t -> data:Signal.t D.t -> Signal.t)) =
    Signal.(tst.d.ready <== (f ~valid:tst.s.valid ~data:tst.s.data))

  let bufferize spec ?(ready_ahead = false) tst_in =
    let open Signal in
  
    let module Buffer = Fifos.Buffer(Data) in
  
    let tst_out = create_wires () in 
  
    let buffer_in = Buffer.I.Of_signal.wires () in
    buffer_in.wr_enable <== tst_in.s.valid;
    buffer_in.rd_enable <== tst_out.d.ready;
    Data.Of_signal.assign buffer_in.wr_data tst_in.s.data;
  
    let buffer_out = Buffer.create spec buffer_in in
    tst_in.d.ready <== (if ready_ahead then buffer_out.ready_next else buffer_out.ready);
    tst_out.s.valid <== buffer_out.rd_valid;
    Data.Of_signal.assign tst_out.s.data buffer_out.rd_data;
  
    tst_out

  let bufferized_gate spec ?(enable_in=Signal.vdd) ?(enable_out=Signal.vdd) t= 
    let open Signal in
    let tst = create ~valid:(t.s.valid &: (reg spec enable_in)) ~data:t.s.data in
    t.d.ready <== reg spec (tst.d.ready &: enable_in);
    let tst = bufferize spec ~ready_ahead:true tst in
    gate tst ~enable:(reg spec enable_out)

  let pipe_source spec tst_in = 
    let open Signal in

    let tst_out = create_wires () in
    Src.Of_signal.assign tst_out.s (Src.Of_signal.reg ~enable:tst_in.d.ready spec tst_in.s); 
    tst_in.d.ready <== (tst_out.d.ready |: ~:(tst_out.s.valid));
    tst_out

end

module Of_pair (FstData : Interface.S) (SndData : Interface.S) = struct
  module Fst = Make(FstData)
  module Snd = Make(SndData)

  module Data = struct
    type 'a t =
      { fst : 'a FstData.t
      ; snd : 'a SndData.t
      }
    [@@deriving sexp_of, hardcaml]

    let create fst snd = {fst; snd}
  end

  include Make(Data)
  
  let join_comb (fst : Fst.t) (snd : Snd.t) =
    let open Signal in

    let res = create ~valid:((Fst.valid fst) &: (Snd.valid snd)) ~data:{Data.fst = (Fst.data fst); snd = (Snd.data snd)} in

    assign (Fst.ready fst) ((Snd.valid snd) &: ready res);
    assign (Snd.ready snd) ((Fst.valid fst) &: ready res);

    res

  let split_comb (tst : t) =
    let open Signal in

    let fst = Fst.create_wires () in
    let snd = Snd.create_wires () in

    assign (Fst.valid fst) (valid tst &: Snd.ready snd);    
    assign (Snd.valid snd) (valid tst &: Fst.ready fst);
    assign (ready tst) ((Fst.ready fst) &: (Snd.ready snd));

    let data = data tst in
    Fst.D.Of_signal.assign (Fst.data fst) data.fst;
    Snd.D.Of_signal.assign (Snd.data snd) data.snd;

    fst, snd

  let fst tst =
    let t1, t2 = split_comb tst in
    Signal.assign (Snd.ready t2) Signal.vdd;
    t1

  let snd tst =
    let t1, t2 = split_comb tst in
    Signal.assign (Fst.ready t1) Signal.vdd;
    t2

end

module Two_way (ReqData : Interface.S) (RespData : Interface.S) = struct
  module Req = Make(ReqData)
  module Resp = Make(RespData)

  module Src = struct
    type 'a t =
      { req : 'a Req.Src.t
      ; resp : 'a Resp.Dst.t
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:true]
  end

  module Dst = struct
    type 'a t =
      { req : 'a Req.Dst.t
      ; resp : 'a Resp.Src.t
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:true]
  end

  type t = 
    { req : Req.t
    ; resp : Resp.t
    }

  let t_of_if (s : Signal.t Src.t) (d : Signal.t Dst.t) =
    let req = Req.t_of_if s.req d.req in
    let resp = Resp.t_of_if d.resp s.resp in
    {req; resp}

  let if_of_t (t : t) = 
    let s = {Src.req = t.req.s; resp = t.resp.d} in
    let d = {Dst.req = t.req.d; resp = t.resp.s} in
    s, d

  let create req resp = {req; resp}
  let create_wires () = t_of_if (Src.Of_signal.wires ()) (Dst.Of_signal.wires ())

end

let map (type from_t) (type to_t) (type from_dt) (type to_dt)
      (module From : S with type t = from_t and type data_type = from_dt)
      (module To : S with type t = to_t and type data_type = to_dt)
      tst
      ~f =
  let out = To.create ~valid:(From.valid tst) ~data:(f (From.data tst)) in
  Signal.assign (From.ready tst) (To.ready out);
  out

let map2 (type from1_t) (type from2_t) (type to_t) (type from1_dt) (type from2_dt) (type to_dt)
      (module From1 : S with type t = from1_t and type data_type = from1_dt)
      (module From2 : S with type t = from2_t and type data_type = from2_dt)
      (module To : S with type t = to_t and type data_type = to_dt)
      (t1 : from1_t)
      (t2 : from2_t)
      ~f =
  let open Signal in
  let out = To.create ~valid:((From1.valid t1) &: (From2.valid t2)) ~data:(f (From1.data t1) (From2.data t2)) in
  assign (From1.ready t1) ((From2.valid t2) &: (To.ready out));
  assign (From2.ready t2) ((From1.valid t1) &: (To.ready out));
  out

let map3 (type from1_t) (type from2_t) (type from3_t) (type to_t) (type from1_dt) (type from2_dt) (type from3_dt) (type to_dt)
      (module From1 : S with type t = from1_t and type data_type = from1_dt)
      (module From2 : S with type t = from2_t and type data_type = from2_dt)
      (module From3 : S with type t = from3_t and type data_type = from3_dt)
      (module To : S with type t = to_t and type data_type = to_dt)
      (t1 : from1_t)
      (t2 : from2_t)
      (t3 : from3_t)
      ~f =
  let open Signal in
  let out = To.create ~valid:((From1.valid t1) &: (From2.valid t2) &: (From3.valid t3)) ~data:(f (From1.data t1) (From2.data t2) (From3.data t3)) in
  assign (From1.ready t1) ((From2.valid t2) &: (From3.valid t3) &: (To.ready out));
  assign (From2.ready t2) ((From1.valid t1) &: (From3.valid t3) &: (To.ready out));
  assign (From3.ready t3) ((From1.valid t1) &: (From2.valid t2) &: (To.ready out));
  out

let fork_map (type from_t) (type to_t) (type from_dt) (type to_dt)
      (module From : S with type t = from_t and type data_type = from_dt)
      (module To : S with type t = to_t and type data_type = to_dt)
      tst
      ~f = 
  let tst1, tst2 = From.fork tst in
  tst1, map (module From) (module To) tst2 ~f
  
let filter_map2 (type from1_t) (type from2_t) (type to_t) (type from1_dt) (type from2_dt) (type to_dt)
      (module From1 : S with type t = from1_t and type data_type = from1_dt)
      (module From2 : S with type t = from2_t and type data_type = from2_dt)
      (module To : S with type t = to_t and type data_type = to_dt)
      (t1 : from1_t)
      (t2 : from2_t)
      ~f =
  let open Signal in

  let new_data, filter = f (From1.data t1) (From2.data t2) in

  let out = To.create ~valid:((From1.valid t1) &: (From2.valid t2) &: filter) ~data:new_data in
  assign (From1.ready t1) ((From2.valid t2) &: (To.ready out));
  assign (From2.ready t2) ((From1.valid t1) &: (To.ready out));
  out
