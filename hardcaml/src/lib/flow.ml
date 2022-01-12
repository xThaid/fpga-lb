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

type t = 
{ src : Signal.t Source.t
; dst : Signal.t Dest.t
}

let t_of_if (src : Signal.t Source.t) (dst : Signal.t Dest.t) = {src; dst}
let if_of_t (t : t) =  t.src, t.dst
let create_wires () = t_of_if (Source.Of_signal.wires ()) (Dest.Of_signal.wires ())

let word_width = Source.port_widths.data
let empty_width = Source.port_widths.empty

let is_fired t = Signal.(t.src.valid &: t.dst.ready)
let is_fired_last t = Signal.((is_fired t) &: t.src.last)
let is_stalled t = Signal.(t.src.valid &: ~:(t.dst.ready))

let from_avalonst (i : Signal.t AvalonST.I.t) (o : Signal.t AvalonST.O.t) =
  let open Signal in

  let flow = create_wires () in

  flow.src.valid <== i.valid;
  flow.src.data <== i.data;
  flow.src.empty <== i.empty;
  flow.src.last <== i.endofpacket;

  o.ready <== flow.dst.ready;

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

  i.valid <== flow.src.valid;
  i.data <== flow.src.data;
  i.empty <== flow.src.empty;
  i.endofpacket <== flow.src.last;
  i.startofpacket <== (~:(busy.value) &: (is_fired flow));

  flow.dst.ready <== o.ready;

  i, o

let connect f1 f2 =
  Source.Of_signal.assign f1.src f2.src;
  Dest.Of_signal.assign f2.dst f1.dst

(* bufferize cuts Flow.Source paths (valid/last/data/empty). Optionally, it can cut ready path as well. *)
let bufferize reg_spec ?(ready_ahead = true) ?(enable = Signal.vdd) (source : t) =
  let open Signal in

  let module BufferData = struct
      type 'a t =
        { data : 'a [@bits 32]
        ; empty : 'a [@bits 2]
        ; last : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end
  in

  let module Buffer = Fifos.Buffer(BufferData) in

  let sink = create_wires () in 

  let buffer_in = Buffer.I.Of_signal.wires () in
  buffer_in.wr_enable <== source.src.valid;
  buffer_in.rd_enable <== (sink.dst.ready &: enable);
  buffer_in.wr_data.data <== source.src.data;
  buffer_in.wr_data.empty <== source.src.empty;
  buffer_in.wr_data.last <== source.src.last;

  let buffer_out = Buffer.create reg_spec buffer_in in
  source.dst.ready <== (if ready_ahead then buffer_out.ready_next else buffer_out.ready);
  sink.src.valid <== (buffer_out.rd_valid &: enable);
  sink.src.data <== buffer_out.rd_data.data;
  sink.src.last <== buffer_out.rd_data.last;
  sink.src.empty <== buffer_out.rd_data.empty;

  sink

let shifter ~shift reg_spec (source : t) = 
  let open Signal in

  if shift = 0 then
    source.src, vdd
  else

  let shifted = Source.Of_signal.wires () in

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
      shifted_source.empty <-- saved_transfer.empty -: (of_int ~width:empty_width shift_remainder);
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

(* Sources must have read latency equal to 0. Sink has ready latency 1 *)
let join spec ~hdr_length ~(source1 : t) ~(source2 : t) =
  let open Signal in

  let shift = (hdr_length % word_width) / 8 in
  let shift_compl = ((word_width - (shift * 8) % word_width) % word_width) / 8 in

  let sink = create_wires () in
  let sink_ready_d = reg spec sink.dst.ready in

  let shifted_source2, shifter_ready_next = shifter ~shift:shift_compl spec source2 in

  let source1_ready_next = Always.Variable.reg ~width:1 spec in
  let source2_ready_next = Always.Variable.reg ~width:1 spec in

  let source1_last_data = Always.Variable.reg ~width:word_width spec in
  
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

      sink_src.data <-- (concat_msb [sel_top source1_last_data.value (shift * 8); sel_top source2.src.data (word_width - shift * 8)]);
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
  
  bufferize spec sink

(* Source must have read latency 0. Sinks have ready latency 1. *)
let split spec ~hdr_length ~(source : t) =
  let open Signal in

  let header_leftover = hdr_length % word_width in
  let header_words = (hdr_length + word_width - 1) / word_width in
  let empty_cnt = ((word_width - hdr_length % word_width) % word_width) / 8 in

  let sink1 = create_wires () in
  let sink2 = create_wires () in

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
  sink1.src.empty <== (mux2 sink1.src.last (of_int ~width:empty_width empty_cnt) (zero empty_width));
  sink1.src.valid <== sink1_valid.value;

  sink2.src.data <== shifted_source.data;
  sink2.src.last <== shifted_source.last;
  sink2.src.empty <== shifted_source.empty;
  sink2.src.valid <== sink2_valid.value;

  sink1, bufferize spec sink2


