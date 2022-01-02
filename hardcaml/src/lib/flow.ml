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

  (* Adds a buffer before the sink. It exposes the signal that is asserted when
   the sink will be ready in the next cycle. It can be used to improve timings. *)
  let bufferize reg_spec (sink : 'a t) =
    let open Signal in

    let source = create_empty () in 

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

    source.dst.ready <== input_ready;
    Source.Of_signal.(sink.src <== (Source.Of_always.value out));

    source, input_ready_early

  let shifter ~shift reg_spec (source : 'a t) = 
    let open Signal in

    if shift = 0 then
      source.src, vdd
    else

    let shifted = Source.Of_signal.wires () in

    let word_width = Source.port_widths.data in
    let empty_width = Source.port_widths.empty in
    let shift_remainder = word_width / 8 - shift in

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
        shifted_source.empty <-- source.src.empty -: (of_int ~width:empty_width shift_remainder);
        shifted_source.last <-- vdd;
        shifted_ready_next <-- source.dst.ready
      ] [
        shifted_source.valid <-- source.src.valid;
        shifted_source.empty <-- mux2 (source.src.empty <=:. shift_remainder) (zero empty_width) (source.src.empty -:. shift_remainder);
        shifted_source.last <-- (source.src.last &: ((~:) will_require_extra_transfer));
        shifted_ready_next <-- (~:) (valid_transfer &: source.src.last &: will_require_extra_transfer)
      ]
    ]);

    Source.Of_signal.(shifted <== (Source.Of_always.value shifted_source));

    shifted, shifted_ready_next.value

  let join spec ~shift ~(flow1 : 'a t) ~(flow2 : 'a t) =
    let open Signal in

    let word_width = Source.port_widths.data in
    let shift_compl = ((word_width - (shift * 8) % word_width) % word_width) / 8 in

    let sink_out = create_empty () in
    let sink, sink_ready_early = bufferize spec sink_out in

    let shifted_flow2, shifter_ready_next = shifter ~shift:shift_compl spec flow2 in

    let flow1_ready_next = Always.Variable.reg ~width:1 spec in
    let flow2_ready_next = Always.Variable.reg ~width:1 spec in

    let flow1_last_data = Always.Variable.reg ~width:word_width spec in
    
    let sink_src = Source.Of_always.wire zero in

    let module SMState = struct
      type t =
        | ReadFlow1
        | Transition
        | ReadFlow2
      [@@deriving sexp_of, compare, enumerate]
    end
    in

    let sm = Always.State_machine.create (module SMState) ~enable:vdd spec in

    AlwaysV2.(compile [
    sm.switch [
      ReadFlow1, [
        flow1_ready_next <-- sink_ready_early;

        sink_src.data <-- flow1.src.data;
        sink_src.valid <-- flow1.src.valid;

        when_ (sink.dst.ready &: flow1.src.valid &: flow1.src.last) [
          if_const (shift <> 0) [
            flow1_last_data <-- flow1.src.data;
            sink_src.valid <--. 0;
            sm.set_next Transition;
          ] [
            sm.set_next ReadFlow2;
          ];

          flow1_ready_next <-- gnd;
          flow2_ready_next <-- sink_ready_early;
        ]
      ];

      Transition, [
        flow2_ready_next <-- sink_ready_early;

        sink_src.data <-- (concat_msb [sel_top flow1_last_data.value (shift * 8); sel_top flow2.src.data (word_width - shift * 8)]);
        sink_src.valid <-- flow2.src.valid;

        when_ (sink.dst.ready &: flow2.src.valid) [
          sm.set_next ReadFlow2
        ]
      ];

      ReadFlow2, [
        flow2_ready_next <-- (sink_ready_early &: shifter_ready_next);

        Source.Of_always.assign sink_src shifted_flow2;

        when_ (sink.dst.ready &: shifted_flow2.valid &: shifted_flow2.last) [
          flow1_ready_next <-- sink_ready_early;
          flow2_ready_next <--. 0;

          sm.set_next ReadFlow1
        ]
      ];
    ]
    ]);

    flow1.dst.ready <== flow1_ready_next.value;
    flow2.dst.ready <== flow2_ready_next.value;

    Source.Of_signal.assign sink.src (Source.Of_always.value sink_src);
    
    sink_out

end


