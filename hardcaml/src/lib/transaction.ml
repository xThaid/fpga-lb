open Base
open Hardcaml

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

  let data_len = List.reduce_exn Data.Names_and_widths.port_widths ~f:(+)

  let t_of_if (s : Signal.t Src.t) (d : Signal.t Dst.t) = {s; d}
  let if_of_t (t : t) =  t.s, t.d
  let inputs (t : t) = t.s
  let outputs (t : t) = t.d
  let create_wires () = t_of_if (Src.Of_signal.wires ()) (Dst.Of_signal.wires ())
  let create ~valid ~data = t_of_if {Src.valid; data} (Dst.Of_signal.wires ())

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

  let demux n sel tst = 
    let open Signal in

    if (Signal.width sel) <> (Signal.address_bits_for n) then
      raise_s [%message "sel signal in demultiplexer has incorrect width"];

    let tsts = List.init n ~f:(fun i ->
      create ~valid:(tst.s.valid &: (sel ==:. i)) ~data:tst.s.data
    ) in

    tst.d.ready <== mux sel (List.map tsts ~f:(fun t -> t.d.ready));

    tsts

  let demux2_on tst ~f =
    let sel = f tst.s.data in
    let tsts = demux 2 sel tst in
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

  let bufferized_gate spec t ~enable = 
    let open Signal in
    let tst = create ~valid:(t.s.valid &: (reg spec enable)) ~data:t.s.data in
    t.d.ready <== reg spec (tst.d.ready &: enable);
    bufferize spec ~ready_ahead:true tst

  let pipe_source spec tst_in = 
    let open Signal in

    let tst_out = create_wires () in
    Src.Of_signal.assign tst_out.s (Src.Of_signal.reg ~enable:tst_in.d.ready spec tst_in.s); 
    tst_in.d.ready <== (tst_out.d.ready |: ~:(tst_out.s.valid));
    tst_out

end

module Of_pair (DataFst : Interface.S) (DataSnd : Interface.S) = struct
  module TstFst = Make(DataFst)
  module TstSnd = Make(DataSnd)

  module Data = struct
    type 'a t =
      { fst : 'a DataFst.t
      ; snd : 'a DataSnd.t
      }
    [@@deriving sexp_of, hardcaml]

    let create fst snd = {fst; snd}
  end

  include Make(Data)
  
  let join_comb (fst : TstFst.t) (snd : TstSnd.t) =
    let open Signal in

    let res = create ~valid:((TstFst.valid fst) &: (TstSnd.valid snd)) ~data:{Data.fst = (TstFst.data fst); snd = (TstSnd.data snd)} in

    assign (TstFst.ready fst) ((TstSnd.valid snd) &: ready res);
    assign (TstSnd.ready snd) ((TstFst.valid fst) &: ready res);

    res

  let split_comb (tst : t) =
    let open Signal in

    let fst = TstFst.create_wires () in
    let snd = TstSnd.create_wires () in

    assign (TstFst.valid fst) (valid tst &: TstSnd.ready snd);    
    assign (TstSnd.valid snd) (valid tst &: TstFst.ready fst);
    assign (ready tst) ((TstFst.ready fst) &: (TstSnd.ready snd));

    let data = data tst in
    TstFst.D.Of_signal.assign (TstFst.data fst) data.fst;
    TstSnd.D.Of_signal.assign (TstSnd.data snd) data.snd;

    fst, snd

end
