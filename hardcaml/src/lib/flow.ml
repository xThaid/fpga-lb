open Base
open Hardcaml
open Utilities

module Source = struct 
  type 'a t =
    { valid : 'a
    ; last : 'a
    ; data : 'a [@bits 32]
    ; empty : 'a [@bits 2]
    }
  [@@deriving sexp_of, hardcaml]

  let data_width = port_widths.data
  let empty_width = port_widths.empty
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

  let connect f1 f2 =
    Source.Of_signal.assign f1.src f2.src;
    Dest.Of_signal.assign f2.dst f1.dst

  (* bufferize cuts Flow.Source paths (valid/last/data/empty). As the result, it reduces read latency from 1 to 0. *)
  let bufferize reg_spec (source : 'a t) =
    let open Signal in

    let sink = create_empty () in 

    let store_in_to_out = Always.Variable.wire ~default:gnd in
    let store_in_to_buff = Always.Variable.wire ~default:gnd in
    let store_buff_to_out = Always.Variable.wire ~default:gnd in

    let buffer = Source.Of_always.reg reg_spec in
    let out = Source.Of_always.reg reg_spec in

    let assign_step (dst : Always.Variable.t Source.t) (src : Signal.t Source.t) = 
      Always.(proc [
        dst.data <-- src.data;
        dst.empty <-- src.empty;
        dst.last <-- src.last;
      ])
    in

    (* Enable ready input next cycle if output is ready or the buffer will not be filled on the next cycle *)
    let input_ready_early = sink.dst.ready |: (((~:) buffer.valid.value) &: (((~:) out.valid.value) |: ((~:) source.src.valid))) in
    let input_ready = reg reg_spec input_ready_early in

    Always.(compile [
      if_ input_ready [
        (* Input is ready *)
        if_ (sink.dst.ready |: ((~:) out.valid.value)) [
          (* Output is ready or currently not valid, transfer data to output *)
          out.valid <-- source.src.valid;
          store_in_to_out <--. 1
        ] [
          (* Output is not ready, store input in buffer *)
          buffer.valid <-- source.src.valid;
          store_in_to_buff <--. 1
        ]
      ] @@ elif sink.dst.ready [
        (* Input is not ready, but output is ready *)
        out.valid <-- buffer.valid.value;
        buffer.valid <--. 0;
        store_buff_to_out <--. 1
      ] [];

      if_ store_in_to_out.value [
        assign_step out source.src
      ] @@ elif store_in_to_buff.value [
        assign_step buffer source.src
      ] [];

      when_ store_buff_to_out.value [
        assign_step out (Source.Of_always.value buffer)
      ]
    ]);

    source.dst.ready <== input_ready_early;
    Source.Of_signal.(sink.src <== (Source.Of_always.value out));

    sink

  let shifter ~shift reg_spec (source : 'a t) = 
    let open Signal in

    if shift = 0 then
      source.src, vdd
    else

    let shifted = Source.Of_signal.wires () in

    let shift_remainder = Source.data_width / 8 - shift in

    let shifted_source = Source.Of_always.wire zero in
    let shifted_ready_next = Always.Variable.wire ~default:gnd in

    let valid_transfer = source.dst.ready &: source.src.valid in
    let saved_transfer = Source.Of_signal.reg reg_spec ~enable:valid_transfer source.src in

    let will_require_extra_transfer = source.src.empty <:. shift_remainder in
    let extra_transfer = Always.Variable.reg ~width:1 reg_spec in

    Always.(compile [
      if_ extra_transfer.value [
        when_ source.dst.ready [
          extra_transfer <--. 0 
        ]
      ] [
        when_ valid_transfer [
          extra_transfer <-- (source.src.last &: will_require_extra_transfer)
        ]
      ]
    ]);

    Always.(compile [
      shifted_source.data <-- concat_msb [sel_bottom saved_transfer.data (shift_remainder * 8); sel_top source.src.data (shift * 8)];
      if_ extra_transfer.value [
        shifted_source.valid <-- vdd;
        shifted_source.empty <-- saved_transfer.empty -: (of_int ~width:Source.empty_width shift_remainder);
        shifted_source.last <-- vdd;
        shifted_ready_next <-- source.dst.ready
      ] [
        shifted_source.valid <-- source.src.valid;
        shifted_source.empty <-- mux2 (source.src.empty <=:. shift_remainder) (zero Source.empty_width) (source.src.empty -:. shift_remainder);
        shifted_source.last <-- (source.src.last &: ((~:) will_require_extra_transfer));
        shifted_ready_next <-- (~:) (valid_transfer &: source.src.last &: will_require_extra_transfer)
      ]
    ]);

    Source.Of_signal.(shifted <== (Source.Of_always.value shifted_source));

    shifted, shifted_ready_next.value

  (* Sources must have read latency equal to 0. Sink has ready latency 1 *)
  let join spec ~shift ~(source1 : 'a t) ~(source2 : 'a t) =
    let open Signal in

    let shift_compl = ((Source.data_width - (shift * 8) % Source.data_width) % Source.data_width) / 8 in

    let sink = create_empty () in
    let sink_ready_d = reg spec sink.dst.ready in

    let shifted_source2, shifter_ready_next = shifter ~shift:shift_compl spec source2 in

    let source1_ready_next = Always.Variable.reg ~width:1 spec in
    let source2_ready_next = Always.Variable.reg ~width:1 spec in

    let source1_last_data = Always.Variable.reg ~width:Source.data_width spec in
    
    let sink_src = Source.Of_always.wire zero in

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
        source1_ready_next <-- sink.dst.ready;

        sink_src.data <-- source1.src.data;
        sink_src.valid <-- source1.src.valid;

        when_ (sink_ready_d &: source1.src.valid &: source1.src.last) [
          if_const (shift <> 0) [
            source1_last_data <-- source1.src.data;
            sink_src.valid <--. 0;
            sm.set_next Transition;
          ] [
            sm.set_next ReadSource2;
          ];

          source1_ready_next <-- gnd;
          source2_ready_next <-- sink.dst.ready;
        ]
      ];

      Transition, [
        source2_ready_next <-- sink.dst.ready;

        sink_src.data <-- (concat_msb [sel_top source1_last_data.value (shift * 8); sel_top source2.src.data (Source.data_width - shift * 8)]);
        sink_src.valid <-- source2.src.valid;

        when_ (sink_ready_d &: source2.src.valid) [
          sm.set_next ReadSource2
        ]
      ];

      ReadSource2, [
        source2_ready_next <-- (sink.dst.ready &: shifter_ready_next);

        Source.Of_always.assign sink_src shifted_source2;

        when_ (sink_ready_d &: shifted_source2.valid &: shifted_source2.last) [
          source1_ready_next <-- sink.dst.ready;
          source2_ready_next <--. 0;

          sm.set_next ReadSource1
        ]
      ];
    ]
    ]);

    source1.dst.ready <== source1_ready_next.value;
    source2.dst.ready <== source2_ready_next.value;

    Source.Of_signal.assign sink.src (Source.Of_always.value sink_src);
    
    sink

  (* Source must have read latency 0. Sinks have ready latency 1. *)
  let split spec ~hdr_length ~(source : 'a t) =
    let open Signal in

    let header_leftover = hdr_length % Source.data_width in
    let header_words = (hdr_length + Source.data_width - 1) / Source.data_width in
    let empty_cnt = ((Source.data_width - hdr_length % Source.data_width) % Source.data_width) / 8 in

    let sink1 = create_empty () in
    let sink2 = create_empty () in

    let sink1_ready_d = reg spec sink1.dst.ready in
    let sink2_ready_d = reg spec sink2.dst.ready in

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
        ready_next <-- sink1.dst.ready;
        sink1_valid <-- source.src.valid;

        when_ (sink1_ready_d &: source.src.valid) [
          hdr_word_counter <-- (hdr_word_counter.value +:. 1);

          when_ sink1.src.last [
            ready_next <-- sink2.dst.ready;
            sm.set_next WriteSink2;
          ]
        ]
      ];

      WriteSink2, [
        ready_next <-- (sink2.dst.ready &: shifter_ready_next);
        sink2_valid <-- shifted_source.valid;
        
        when_ (sink2_ready_d &: shifted_source.valid &: sink2.src.last) [
          ready_next <-- sink1.dst.ready;
          hdr_word_counter <--. 0;
          sm.set_next WriteSink1;
        ]
      ];
    ]
    ]);

    source.dst.ready <== ready_next.value;

    sink1.src.data <== source.src.data;
    sink1.src.last <== (hdr_word_counter.value ==:. header_words - 1);
    sink1.src.empty <== (mux2 sink1.src.last (of_int ~width:Source.empty_width empty_cnt) (zero Source.empty_width));
    sink1.src.valid <== sink1_valid.value;

    sink2.src.data <== shifted_source.data;
    sink2.src.last <== shifted_source.last;
    sink2.src.empty <== shifted_source.empty;
    sink2.src.valid <== sink2_valid.value;

    sink1, sink2

end


