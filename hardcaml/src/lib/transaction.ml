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

  val data_len : int

  val t_of_if : Signal.t Src.t -> Signal.t Dst.t -> t
  val if_of_t : t -> Signal.t Src.t * Signal.t Dst.t
  val inputs : t -> Signal.t Src.t
  val outputs : t -> Signal.t Dst.t
  val create_wires : unit -> t
  val create : valid:Signal.t -> data:Signal.t D.t -> t

  val is_fired : t -> Signal.t
  val is_stalled : t -> Signal.t
  val ready : t -> Signal.t
  val valid : t -> Signal.t
  val data : t -> Signal.t D.t

  val map_comb : t -> f:(Signal.t D.t -> Signal.t D.t) -> t
  val filter_comb : t -> f:(Signal.t D.t -> Signal.t) -> t

  val demux : int -> Signal.t -> t -> t list
  val demux2_on : t -> f:(Signal.t D.t -> Signal.t) -> t * t
  val apply : t -> f:(valid:Signal.t -> data:Signal.t D.t -> Signal.t) -> unit
end

module Make (Data : Interface.S) : (S with module D = Data) = struct
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

end

module Serializer (Data : Interface.S) = struct
  module Tst = Make(Data)

  let serialize spec (tst : Tst.t) =
    let open Signal in

    if Tst.data_len <= Flow.word_width then raise_s [%message "transaction data with length <= word width is not supported"];
    if Tst.data_len % 8 <> 0 then raise_s [%message "transaction data should have length divisible by 8"];

    let empty_cnt = ((Flow.word_width - Tst.data_len % Flow.word_width) % Flow.word_width) / 8 in
    let data_words = (Tst.data_len + Flow.word_width - 1) / Flow.word_width in
    let data_buf_width = data_words * Flow.word_width in
    let data_buf = Always.Variable.reg ~width:data_buf_width spec in
    let data_packed = Data.Of_signal.pack ~rev:true (Tst.data tst) in

    let data_word_counter = Always.Variable.reg ~width:(num_bits_to_represent data_words) spec in

    let ready_next = Always.Variable.reg ~width:1 spec in

    let flow_src = Flow.Source.Of_signal.wires () in
    let flow_dst = Flow.Dest.Of_signal.wires () in

    let module SM = struct
      type t =
        | Idle
        | Write
      [@@deriving sexp_of, compare, enumerate]
    end
    in

    let sm = Always.State_machine.create (module SM) ~enable:vdd spec in

    Always.(compile [
    sm.switch [
      Idle, [
        ready_next <--. 1;
        data_word_counter <--. 0;

        when_ (Tst.valid tst) [
          data_buf <-- (if empty_cnt = 0 then data_packed else (concat_msb [data_packed; zero (empty_cnt * 8)]));
          ready_next <--. 0;
          sm.set_next Write
        ]
      ];

      Write, [
        when_ flow_dst.ready [
          data_word_counter <-- data_word_counter.value +:. 1;
          data_buf <-- (sll data_buf.value Flow.word_width);
          when_ (data_word_counter.value ==:. data_words - 1) [
            ready_next <--. 1;
            sm.set_next Idle;
          ];
        ]
      ];
    ]
    ]);

    assign (Tst.ready tst) ready_next.value;

    flow_src.data <== (sel_top data_buf.value Flow.word_width);
    flow_src.last <== (data_word_counter.value ==:. data_words - 1);
    flow_src.empty <== (mux2 flow_src.last (of_int ~width:Flow.empty_width empty_cnt) (zero Flow.empty_width));
    flow_src.valid <== sm.is Write;

    Flow.t_of_if flow_src flow_dst

  let deserialize spec (flow : Flow.t) =
    let open Signal in

    if Tst.data_len <= Flow.word_width then raise_s [%message "transaction data with length <= word width is not supported"];
    if Tst.data_len % 8 <> 0 then raise_s [%message "transaction data should have length divisible by 8"];

    let tst = Tst.create_wires () in
    let data_words = (Tst.data_len + Flow.word_width - 1) / Flow.word_width in
    let data_buf_width = data_words * Flow.word_width in

    let data_word_counter = Always.Variable.reg ~width:(num_bits_to_represent data_words) spec in

    let tst_valid = Always.Variable.reg ~width:1 spec in
    let data_buf = Always.Variable.reg ~width:data_buf_width spec in
    let append_data_buf () = Always.(data_buf <-- ((sll data_buf.value Flow.word_width) |: (uresize flow.src.data data_buf_width))) in

    let ready_next = Always.Variable.wire ~default:vdd in

    let module SM = struct
      type t =
        | Idle
        | Read
        | ReadLast
        | Wait
      [@@deriving sexp_of, compare, enumerate]
    end
    in

    let sm = Always.State_machine.create (module SM) ~enable:vdd spec in

    Always.(compile [
    sm.switch [
      Idle, [
        data_word_counter <--. 1;

        when_ (Flow.is_fired flow) [
          append_data_buf ();
          sm.set_next Read;
        ]
      ];

      Read, [
        when_ (Flow.is_fired flow) [
          data_word_counter <-- data_word_counter.value +:. 1;
          append_data_buf ();

          when_ (data_word_counter.value ==:. data_words - 1) [
            tst_valid <--. 1;

            if_ (flow.src.last) [
              ready_next <--. 0;
              sm.set_next Wait;
            ] [
              sm.set_next ReadLast;
            ]
          ];
        ]
      ];

      ReadLast, [
        when_ (Tst.ready tst) [
          tst_valid <--. 0;
        ];

        when_ (Flow.is_fired_last flow) [
          if_ (Tst.ready tst |: ~:(tst_valid.value)) [
            sm.set_next Idle;
          ] [
            ready_next <--. 0;
            sm.set_next Wait;
          ];
        ]
      ];

      Wait, [
        ready_next <--. 0;

        when_ (Tst.ready tst) [
          tst_valid <--. 0;
          ready_next <--. 1;
          sm.set_next Idle;
        ]
      ]
    ];
    ]);

    flow.dst.ready <== (reg spec ready_next.value);

    assign (Tst.valid tst) tst_valid.value;
    Data.Of_signal.assign (Tst.data tst) (Data.Of_signal.unpack ~rev:true (sel_top data_buf.value Tst.data_len));

    tst

end

module With_flow (Data : Interface.S) = struct
  module Tst = Make(Data)

  module Src = struct
    type 'a t =
      { tst : 'a Tst.Src.t
      ; flow : 'a Flow.Source.t
      }
  [@@deriving sexp_of, hardcaml ~rtlmangle:true]
  end

  module Dst = struct
    type 'a t =
      { tst : 'a Tst.Dst.t
      ; flow : 'a Flow.Dest.t
      }
  [@@deriving sexp_of, hardcaml ~rtlmangle:true]
  end

  type t = 
    { tst : Tst.t
    ; flow : Flow.t
    }

  let t_of_if (src : Signal.t Src.t) (dst : Signal.t Dst.t)= 
    let tst = Tst.t_of_if src.tst dst.tst in
    let flow = Flow.t_of_if src.flow dst.flow in
    {tst; flow}
  let if_of_t (t : t) =
    let tst_s, tst_d = Tst.if_of_t t.tst in
    let flow_src, flow_dst = Flow.if_of_t t.flow in
    {Src.tst = tst_s; flow = flow_src}, {Dst.tst = tst_d; flow = flow_dst}
  let create_wires () = 
    t_of_if (Src.Of_signal.wires ()) (Dst.Of_signal.wires ())

  let connect t1 t2 =
    let i1, o1 = if_of_t t1 in
    let i2, o2 = if_of_t t2 in
    Src.Of_signal.assign i1 i2;
    Dst.Of_signal.assign o2 o1

  let combine spec (tst_in : Tst.t) (flow_in : Flow.t) =
    let open Signal in

    let tst_data = Data.Of_always.reg spec in
    let tst_valid = Always.Variable.reg ~width:1 spec in
    let tst_out = Tst.create ~valid:tst_valid.value ~data:(Data.Of_always.value tst_data) in

    let buffer_en = Always.Variable.reg ~width:1 spec in
    let flow_out = Flow.bufferize spec ~ready_ahead:false flow_in in
    let flow_out = Flow.gate buffer_en.value flow_out in

    let module SM = struct
      type t = Idle | Busy | Wait
      [@@deriving sexp_of, compare, enumerate]
    end in

    let sm = Always.State_machine.create (module SM) ~enable:vdd spec in

    Always.(compile [
    sm.switch [
      Idle, [
        when_ (Tst.is_fired tst_in) [
          Data.Of_always.assign tst_data (Tst.data tst_in);
          tst_valid <--. 1;
          buffer_en <--. 1;

          sm.set_next Busy;
        ]
      ];

      Busy, [
        when_ (Tst.is_fired tst_out) [
          tst_valid <--. 0;
        ];

        when_ (Flow.is_fired_last flow_out) [
          buffer_en <--. 0;

          if_ (Tst.is_fired tst_out |: ~:(tst_valid.value)) [
            sm.set_next Idle;
          ] [
            sm.set_next Wait;
          ];
        ]
      ];

      Wait, [
        when_ (Tst.is_fired tst_out) [
          tst_valid <--. 0;
          sm.set_next Idle;
        ];
      ];
    ]
    ]);

    assign (Tst.ready tst_in) (sm.is Idle); (* TODO: this can be changed to reduce latency *)

    {tst = tst_out; flow = flow_out}

  let from_flow spec (flow : Flow.t) =
    let module Serializer = Serializer(Data) in
    let f1, f2 = Flow.split spec ~hdr_length:Tst.data_len ~source:flow in
    let tst = Serializer.deserialize spec f1 in
    combine spec tst f2

  let to_flow spec (flow : t) =
    let module Serializer = Serializer(Data) in
    let f1 = Serializer.serialize spec flow.tst in
    Flow.join spec ~hdr_length:Tst.data_len ~source1:f1 ~source2:flow.flow

  let demux spec ~(flow : t) ~sel _n =
    let open Signal in

    let _sel = reg spec ~enable:(Tst.valid flow.tst) sel in
    ()
  
end

module Of_pair (DataFst : Interface.S) (DataSnd : Interface.S) = struct
  module TstFst = Make(DataFst)
  module TstSnd = Make(DataSnd)
  module FstFlow = With_flow(DataFst)

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
