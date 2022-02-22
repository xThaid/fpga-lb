open Base
open Hardcaml
open Utilities

module AvalonST = struct
  module I = struct 
    type 'a t =
      { valid : 'a
      ; endofpacket : 'a
      ; startofpacket : 'a
      ; data : 'a [@bits 32]
      ; empty : 'a [@bits 2]
      }
    [@@deriving sexp_of, hardcaml]
  end
  
  module O = struct 
    type 'a t =
      { ready : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end
end

(* A representation of data stream. In fact, it is just a transaction with extra signals 
   denoting the state of a stream. *)
module Base = struct
  module TstData = struct
    type 'a t =
      { last : 'a
      ; data : 'a [@bits 32]
      ; empty : 'a [@bits 2]
      }
    [@@deriving sexp_of, hardcaml]
  end

  include Transaction.Make(TstData)

  let word_width = TstData.port_widths.data
  let empty_width = TstData.port_widths.empty

  let is_fired_last t = Signal.((is_fired t) &: t.s.data.last)

  let from_avalonst (i : Signal.t AvalonST.I.t) (o : Signal.t AvalonST.O.t) =
    let open Signal in

    let flow = create_wires () in

    flow.s.valid <== i.valid;
    flow.s.data.data <== i.data;
    flow.s.data.empty <== i.empty;
    flow.s.data.last <== i.endofpacket;

    o.ready <== flow.d.ready;

    flow

  let to_avalonst spec (flow : t) =
    let open Signal in

    let i = AvalonST.I.Of_signal.wires () in
    let o = AvalonST.O.Of_signal.wires () in

    let busy = Always.Variable.reg spec ~width:1 in

    Always.(compile [
      if_ busy.value [
        when_ (is_fired_last flow) [
          busy <--. 0;
        ]
      ] [
        when_ (is_fired flow) [
          busy <--. 1;
        ]
      ]
    ]);

    i.valid <== flow.s.valid;
    i.data <== flow.s.data.data;
    i.empty <== flow.s.data.empty;
    i.endofpacket <== flow.s.data.last;
    i.startofpacket <== (~:(busy.value) &: (is_fired flow));

    flow.d.ready <== o.ready;

    i, o

  let shifter ~shift reg_spec (source : t) = 
    let open Signal in

    if shift = 0 then
      source.s, vdd
    else

    let shifted = Src.Of_signal.wires () in

    let shift_remainder = word_width / 8 - shift in

    let shifted_source = Src.Of_always.wire zero in
    let shifted_ready_next = Always.Variable.wire ~default:gnd in

    let valid_transfer = is_fired source in
    let saved_transfer = Src.Of_signal.reg reg_spec ~enable:valid_transfer source.s in

    let will_require_extra_transfer = source.s.data.empty <:. shift_remainder in
    let extra_transfer = Always.Variable.reg ~width:1 reg_spec in

    Always.(compile [
      if_ extra_transfer.value [
        when_ source.d.ready [
          extra_transfer <--. 0 
        ]
      ] [
        when_ valid_transfer [
          extra_transfer <-- (source.s.data.last &: will_require_extra_transfer)
        ]
      ]
    ]);

    Always.(compile [
      shifted_source.data.data <-- concat_msb [sel_bottom saved_transfer.data.data (shift_remainder * 8); sel_top source.s.data.data (shift * 8)];
      if_ extra_transfer.value [
        shifted_source.valid <-- vdd;
        shifted_source.data.empty <-- saved_transfer.data.empty -: (of_int ~width:empty_width shift_remainder);
        shifted_source.data.last <-- vdd;
        shifted_ready_next <-- source.d.ready
      ] [
        shifted_source.valid <-- source.s.valid;
        shifted_source.data.empty <-- mux2 (source.s.data.empty <=:. shift_remainder) (zero empty_width) (source.s.data.empty -:. shift_remainder);
        shifted_source.data.last <-- (source.s.data.last &: ((~:) will_require_extra_transfer));
        shifted_ready_next <-- (~:) (valid_transfer &: source.s.data.last &: will_require_extra_transfer)
      ]
    ]);

    Src.Of_signal.(shifted <== (Src.Of_always.value shifted_source));

    shifted, shifted_ready_next.value

  (* Joins two flows into one by taking `hdr_length` bits from the first one
     and everything from the second one. Sources must have read latency equal
     to 0. Sink has ready latency 1. *)
  let join spec ~hdr_length ~(source1 : t) ~(source2 : t) =
    let open Signal in

    let shift = (hdr_length % word_width) / 8 in
    let shift_compl = ((word_width - (shift * 8) % word_width) % word_width) / 8 in

    let sink = create_wires () in
    let sink_ready_d = reg spec sink.d.ready in

    let shifted_source2, shifter_ready_next = shifter ~shift:shift_compl spec source2 in

    let source1_ready_next = Always.Variable.reg ~width:1 spec in
    let source2_ready_next = Always.Variable.reg ~width:1 spec in

    let source1_last_data = Always.Variable.reg ~width:word_width spec in
    
    let sink_src = Src.Of_always.wire zero in

    let module SMStates = struct
      type t =
        | ReadSource1
        | Transition
        | ReadSource2
      [@@deriving sexp_of, compare, enumerate]
    end
    in

    let sm = Always.State_machine.create (module SMStates) ~enable:vdd spec in

    AlwaysV2.(compile [
    sm.switch [
      ReadSource1, [
        source1_ready_next <-- sink.d.ready;

        sink_src.data.data <-- source1.s.data.data;
        sink_src.valid <-- source1.s.valid;

        when_ (sink_ready_d &: source1.s.valid &: source1.s.data.last) [
          if_const (shift <> 0) [
            source1_last_data <-- source1.s.data.data;
            sink_src.valid <--. 0;
            sm.set_next Transition;
          ] [
            sm.set_next ReadSource2;
          ];

          source1_ready_next <-- gnd;
          source2_ready_next <-- sink.d.ready;
        ]
      ];

      Transition, if shift <> 0 then [
        source2_ready_next <-- sink.d.ready;

        sink_src.data.data <-- (concat_msb [sel_top source1_last_data.value (shift * 8); sel_top source2.s.data.data (word_width - shift * 8)]);
        sink_src.valid <-- source2.s.valid;

        when_ (sink_ready_d &: source2.s.valid) [
          sm.set_next ReadSource2
        ]
      ] else [];

      ReadSource2, [
        source2_ready_next <-- (sink.d.ready &: shifter_ready_next);

        Src.Of_always.assign sink_src shifted_source2;

        when_ (sink_ready_d &: shifted_source2.valid &: shifted_source2.data.last) [
          source1_ready_next <-- sink.d.ready;
          source2_ready_next <--. 0;

          sm.set_next ReadSource1
        ]
      ];
    ]
    ]);

    source1.d.ready <== source1_ready_next.value;
    source2.d.ready <== source2_ready_next.value;

    Src.Of_signal.assign sink.s (Src.Of_always.value sink_src);
    
    pipe ~ready_ahead:true spec sink

  (* Splits a flow by forwarding `hdr_length` bits into the first flow and the
     remainder into the second one. Source must have read latency 0. Sink 1 have
     ready latency 1. *)
  let split spec ~hdr_length ~(source : t) =
    let open Signal in

    let header_leftover = hdr_length % word_width in
    let header_words = (hdr_length + word_width - 1) / word_width in
    let empty_cnt = ((word_width - hdr_length % word_width) % word_width) / 8 in

    let sink1 = create_wires () in
    let sink2 = create_wires () in

    let sink1_ready_d = reg spec sink1.d.ready in
    let sink2_ready_d = reg spec sink2.d.ready in

    let shifted_source, shifter_ready_next = shifter ~shift:(header_leftover / 8) spec source in
    
    let ready_next = Always.Variable.reg ~width:1 spec in
    let hdr_word_counter = Always.Variable.reg ~width:(num_bits_to_represent header_words) spec in

    let sink1_valid = Always.Variable.wire ~default:gnd in
    let sink2_valid = Always.Variable.wire ~default:gnd in

    let module SMStates = struct
      type t =
        | WriteSink1
        | WriteSink2
      [@@deriving sexp_of, compare, enumerate]
    end
    in

    let sm = Always.State_machine.create (module SMStates) ~enable:vdd spec in

    AlwaysV2.(compile [
    sm.switch [
      WriteSink1, [
        ready_next <-- sink1.d.ready;
        sink1_valid <-- source.s.valid;

        when_ (sink1_ready_d &: source.s.valid) [
          hdr_word_counter <-- (hdr_word_counter.value +:. 1);

          when_ sink1.s.data.last [
            ready_next <-- sink2.d.ready;
            sm.set_next WriteSink2;
          ]
        ]
      ];

      WriteSink2, [
        ready_next <-- (sink2.d.ready &: shifter_ready_next);
        sink2_valid <-- shifted_source.valid;
        
        when_ (sink2_ready_d &: shifted_source.valid &: sink2.s.data.last) [
          ready_next <-- sink1.d.ready;
          hdr_word_counter <--. 0;
          sm.set_next WriteSink1;
        ]
      ];
    ]
    ]);

    source.d.ready <== ready_next.value;

    sink1.s.data.data <== source.s.data.data;
    sink1.s.data.last <== (hdr_word_counter.value ==:. header_words - 1);
    sink1.s.data.empty <== (mux2 sink1.s.data.last (of_int ~width:empty_width empty_cnt) (zero empty_width));
    sink1.s.valid <== sink1_valid.value;

    sink2.s.data.data <== shifted_source.data.data;
    sink2.s.data.last <== shifted_source.data.last;
    sink2.s.data.empty <== shifted_source.data.empty;
    sink2.s.valid <== sink2_valid.value;

    sink1, pipe ~ready_ahead:true spec sink2
end

module Serializer (Data : Interface.S) = struct
  module Tst = Transaction.Make(Data)

  (* Serializes a data (wrapped in transaction) to a flow. *)
  let serialize spec (tst : Tst.t) =
    let open Signal in

    if Tst.data_len <= Base.word_width then raise_s [%message "transaction data with length <= word width is not supported"];
    if Tst.data_len % 8 <> 0 then raise_s [%message "transaction data should have length divisible by 8"];

    let empty_cnt = ((Base.word_width - Tst.data_len % Base.word_width) % Base.word_width) / 8 in
    let data_words = (Tst.data_len + Base.word_width - 1) / Base.word_width in
    let data_buf_width = data_words * Base.word_width in
    let data_buf = Always.Variable.reg ~width:data_buf_width spec in
    let data_packed = Data.Of_signal.pack ~rev:true (Tst.data tst) in

    let data_word_counter = Always.Variable.reg ~width:(num_bits_to_represent data_words) spec in

    let ready_next = Always.Variable.reg ~width:1 spec in

    let flow_src = Base.Src.Of_signal.wires () in
    let flow_dst = Base.Dst.Of_signal.wires () in

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
          data_buf <-- (sll data_buf.value Base.word_width);
          when_ (data_word_counter.value ==:. data_words - 1) [
            ready_next <--. 1;
            sm.set_next Idle;
          ];
        ]
      ];
    ]
    ]);

    assign (Tst.ready tst) ready_next.value;

    flow_src.data.data <== (sel_top data_buf.value Base.word_width);
    flow_src.data.last <== (data_word_counter.value ==:. data_words - 1);
    flow_src.data.empty <== (mux2 flow_src.data.last (of_int ~width:Base.empty_width empty_cnt) (zero Base.empty_width));
    flow_src.valid <== sm.is Write;

    Base.t_of_if flow_src flow_dst

  (* Deserializes a data from a flow. *)
  let deserialize spec ?(ready_ahead = false) (flow : Base.t) =
    let open Signal in

    if Tst.data_len <= Base.word_width then raise_s [%message "transaction data with length <= word width is not supported"];
    if Tst.data_len % 8 <> 0 then raise_s [%message "transaction data should have length divisible by 8"];

    let tst = Tst.create_wires () in
    let data_words = (Tst.data_len + Base.word_width - 1) / Base.word_width in
    let data_buf_width = data_words * Base.word_width in

    let data_word_counter = Always.Variable.reg ~width:(num_bits_to_represent data_words) spec in

    let tst_valid = Always.Variable.reg ~width:1 spec in
    let data_buf = Always.Variable.reg ~width:data_buf_width spec in
    let append_data_buf () = Always.(data_buf <-- ((sll data_buf.value Base.word_width) |: (uresize flow.s.data.data data_buf_width))) in

    let ready_next = Always.Variable.wire ~default:vdd in

    flow.d.ready <== (if ready_ahead then ready_next.value else reg spec ready_next.value);

    let flow = Base.t_of_if flow.s (Base.Dst.Of_signal.wires ()) in
    flow.d.ready <== reg spec ready_next.value;

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

        when_ (Base.is_fired flow) [
          append_data_buf ();
          sm.set_next Read;
        ]
      ];

      Read, [
        when_ (Base.is_fired flow) [
          data_word_counter <-- data_word_counter.value +:. 1;
          append_data_buf ();

          when_ (data_word_counter.value ==:. data_words - 1) [
            tst_valid <--. 1;

            if_ (flow.s.data.last) [
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

        when_ (Base.is_fired_last flow) [
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

    assign (Tst.valid tst) tst_valid.value;
    Data.Of_signal.assign (Tst.data tst) (Data.Of_signal.unpack ~rev:true (sel_top data_buf.value Tst.data_len));

    tst

end

(* Representation of a flow combined with a header. *)
module With_header (Data : Interface.S) = struct
  module Header = Transaction.Make(Data)

  module Src = struct
    type 'a t =
      { hdr : 'a Header.Src.t
      ; flow : 'a Base.Src.t
      }
  [@@deriving sexp_of, hardcaml ~rtlmangle:true]
  end

  module Dst = struct
    type 'a t =
      { hdr : 'a Header.Dst.t
      ; flow : 'a Base.Dst.t
      }
  [@@deriving sexp_of, hardcaml ~rtlmangle:true]
  end

  type t = 
    { hdr : Header.t
    ; flow : Base.t
    }

  let t_of_if (src : Signal.t Src.t) (dst : Signal.t Dst.t)= 
    let hdr = Header.t_of_if src.hdr dst.hdr in
    let flow = Base.t_of_if src.flow dst.flow in
    {hdr; flow}
  let if_of_t (t : t) =
    let hdr_s, hdr_d = Header.if_of_t t.hdr in
    let flow_src, flow_dst = Base.if_of_t t.flow in
    {Src.hdr = hdr_s; flow = flow_src}, {Dst.hdr = hdr_d; flow = flow_dst}
  let create_wires () = 
    t_of_if (Src.Of_signal.wires ()) (Dst.Of_signal.wires ())

  let create hdr flow = {hdr = hdr; flow}
  let create_empty () = create (Header.create_empty ()) (Base.create_empty ())

  let apply_names ?prefix ?suffix t =
    let src, dst = if_of_t t in
    let src = Src.Of_signal.apply_names ?prefix ?suffix src in
    let dst = Dst.Of_signal.apply_names ?prefix ?suffix dst in
    t_of_if src dst

  let connect t1 t2 =
    let i1, o1 = if_of_t t1 in
    let i2, o2 = if_of_t t2 in
    Src.Of_signal.assign i1 i2;
    Dst.Of_signal.assign o2 o1

  let pipe spec t = 
    create (Header.pipe spec t.hdr) (Base.pipe spec t.flow)

  let gate t ~enable = 
    create (Header.gate t.hdr ~enable) (Base.gate t.flow ~enable)

  let drop t =
    Header.drop t.hdr;
    Base.drop t.flow

  let fork t =
    let hdr1, hdr2 = Header.fork t.hdr in
    let flow1, flow2 = Base.fork t.flow in
    create hdr1 flow1, create hdr2 flow2

  let pipe_source spec t =
    create (Header.pipe_source spec t.hdr) (Base.pipe_source spec t.flow)

  let map_hdr t ~f =
    create (Header.map t.hdr ~f) t.flow

  module WeakBarrierStatus = struct
    type t =
      { valid : Signal.t
      ; last_fired : Signal.t
      }
  end

  (* Synchronizes header and flow together by letting through exactly one header and one flow at a time. *)
  let weak_barrier spec (flow : t) =
    let open Signal in

    let hdr_enabled_next = Always.Variable.wire ~default:gnd in
    let flow_enabled_next = Always.Variable.wire ~default:gnd in

    let hdr_out = Header.bufferized_gate spec flow.hdr ~enable_in:hdr_enabled_next.value in
    let flow_out = Base.bufferized_gate spec flow.flow ~enable_in:flow_enabled_next.value in

    let last = Always.Variable.wire ~default:gnd in

    let module HdrSM = struct
      type t = HdrIdle | HdrWait | HdrDone
      [@@deriving sexp_of, compare, enumerate]
    end in

    let module FlowSM = struct
      type t = FlowForward | FlowWait | FlowDone
      [@@deriving sexp_of, compare, enumerate]
    end in

    let sm_hdr = Always.State_machine.create (module HdrSM) ~enable:vdd spec in
    let sm_flow = Always.State_machine.create (module FlowSM) ~enable:vdd spec in

    Always.(compile [
    sm_hdr.switch [
      HdrIdle, [
        hdr_enabled_next <--. 1;
        when_ (Header.is_fired flow.hdr) [
          hdr_enabled_next <--. 0;
          sm_hdr.set_next HdrWait;
        ]
      ];

      HdrWait, [
        when_ (Header.is_fired hdr_out) [
          sm_hdr.set_next HdrDone;
        ]
      ];

      HdrDone, []
    ];

    sm_flow.switch [
      FlowForward, [
        flow_enabled_next <--. 1;
        when_ (Base.is_fired_last flow.flow) [
          flow_enabled_next <--. 0;
          sm_flow.set_next FlowWait;
        ]
      ];

      FlowWait, [
        when_ (Base.is_fired_last flow_out) [
          sm_flow.set_next FlowDone;
        ]
      ];

      FlowDone, []
    ];

    when_ (((sm_hdr.is HdrDone) |: (Header.is_fired hdr_out)) &: ((sm_flow.is FlowDone) |: (Base.is_fired_last flow_out))) [
      last <--. 1;
      hdr_enabled_next <--. 1;
      flow_enabled_next <--. 1;
      sm_hdr.set_next HdrIdle;
      sm_flow.set_next FlowForward;
    ]

    ]);
    
    {hdr = hdr_out; flow = flow_out}, {WeakBarrierStatus.valid = Base.valid flow_out |: Header.valid hdr_out; last_fired = last.value}

  module HeaderBarrierStatus = struct
    type t =
      { hdr_valid : Signal.t
      ; flow_end : Signal.t
      }
  end

  (* More strict barrier, in which the header must be sent firstly and then the flow is passed through. *)
  let header_barrier spec flow =
    let open Signal in

    let hdr_enabled_next = Always.Variable.wire ~default:gnd in
    let hdr_out = Header.bufferized_gate spec flow.hdr ~enable_in:hdr_enabled_next.value in

    let flow_enabled_next = Always.Variable.wire ~default:gnd in
    let flow_out = Base.bufferized_gate spec ~enable_out:flow_enabled_next.value flow.flow in

    let last = Always.Variable.wire ~default:gnd in

    let module SM = struct
      type t = Idle | Busy | Wait
      [@@deriving sexp_of, compare, enumerate]
    end in

    let sm = Always.State_machine.create (module SM) spec in

    Always.(compile [
    sm.switch [
      Idle, [
        hdr_enabled_next <--. 1;
        when_ (Header.is_fired flow.hdr) [
          hdr_enabled_next <--. 0;
          flow_enabled_next <--. 1;

          sm.set_next Busy;
        ]
      ];

      Busy, [
        flow_enabled_next <--. 1;
        when_ (Base.is_fired_last flow_out) [
          flow_enabled_next <--. 0;

          if_ (Header.is_fired hdr_out |: ~:(Header.valid hdr_out)) [
            last <--. 1;
            sm.set_next Idle;
          ] [
            sm.set_next Wait;
          ];
        ]
      ];

      Wait, [
        when_ (Header.is_fired hdr_out) [
          last <--. 1;
          sm.set_next Idle;
        ];
      ];
    ]
    ]);

    let status = 
      { HeaderBarrierStatus.hdr_valid = Header.valid hdr_out
      ; flow_end = last.value
      }
    in

    create hdr_out flow_out, status

  (* Takes a plain flow, deserializes its prefix to a header and the remainder
     flow is returned along with the header. *)
  let from_flow spec (flow : Base.t) =
    let module Serializer = Serializer(Data) in
    let f1, f2 = Base.split spec ~hdr_length:Header.data_len ~source:flow in
    let hdr = Serializer.deserialize spec ~ready_ahead:true f1 in
    create hdr f2

  (* Takes a flow with a header, deserializes the header into a flow and then both flows are 
     concated and returned. *)
  let to_flow spec (flow : t) =
    let module Serializer = Serializer(Data) in
    let f1 = Serializer.serialize spec flow.hdr in
    Base.join spec ~hdr_length:Header.data_len ~source1:f1 ~source2:flow.flow |> Base.pipe spec

  let arbitrate spec sources =
    let open Signal in

    let synchronized = List.map sources ~f:(weak_barrier spec) in
    let request, acknowledge = List.map synchronized ~f:(fun (_, s) -> s.valid, s.last_fired) |> List.unzip in

    let granted_onehot = Arbiter.round_robin spec ~request ~acknowledge in
    let granted = onehot_to_binary granted_onehot in
    let granted_valid = reduce (bits_lsb granted_onehot) ~f:( |: ) in
    
    let src = Src.Of_signal.mux granted (List.map synchronized ~f:(fun (s, _) -> fst (if_of_t s))) in
    let src = Src.Of_signal.mux2 granted_valid src (Src.map Src.port_widths ~f:zero) in
    
    let dst = Dst.Of_signal.wires () in

    List.iteri synchronized ~f:(fun i (s, _) -> 
      let _, source_dst = if_of_t s in
      let sel = bit granted_onehot i in
      Dst.Of_signal.assign source_dst (Dst.Of_signal.mux2 sel dst (Dst.map Dst.port_widths ~f:zero))
    );

    pipe spec (t_of_if src dst)

  (* Dispatches a single (header, flow) into multiple flows based on the result
     of the selector, which is a function that takes header's data and returns
     onehot signal denoting to which sink the data must be forwarded. *)
  let dispatch spec source ~selector = 
    let open Signal in

    let synchronized, status = header_barrier spec source in
    let selector_onehot = selector (Header.data synchronized.hdr) in

    let source_src, source_dst = if_of_t synchronized in

    let n = width selector_onehot in

    let busy = Always.Variable.reg ~width:1 spec in
    let sel_reg_onehot = Always.Variable.reg ~width:n spec in
    let sel_onehot = Always.Variable.wire ~default:(zero n) in
    let drop = ~:(reduce (bits_lsb sel_onehot.value) ~f:( |: )) in

    Always.(compile [
      if_ busy.value [
        sel_onehot <-- sel_reg_onehot.value;
        when_ status.flow_end [
          busy <--. 0;
        ]
      ] [
        when_ status.hdr_valid [
          sel_onehot <-- selector_onehot;
          sel_reg_onehot <-- selector_onehot;
          busy <--. 1;
        ]
      ]
    ]);

    let sink_srcs, sink_dsts = List.init n ~f:(fun _ -> if_of_t (create_wires ())) |> List.unzip in
    
    List.iter sink_srcs ~f:(fun src -> Src.Of_signal.assign src source_src);

    let dsts =
      List.map sink_dsts ~f:Dst.Of_signal.pack |>
      List.reduce_exn ~f:( |: ) |>
      Dst.Of_signal.unpack
    in

    Dst.Of_signal.assign source_dst (Dst.Of_signal.mux2 drop (Dst.map Dst.port_widths ~f:ones) dsts);

    let sinks = 
      List.map2_exn sink_srcs sink_dsts ~f:t_of_if |>
      List.mapi ~f:(fun i sink -> gate ~enable:(bit sel_onehot.value i) sink)
    in

    sinks

  (* Filters (header, flow) based on the header data. *)
  let filter spec source ~f = 
    let sinks = dispatch spec source ~selector:f in
    if List.length sinks <> 1 then raise_s [%message "filter should return a signal of length 1"];
    List.nth_exn sinks 0

end