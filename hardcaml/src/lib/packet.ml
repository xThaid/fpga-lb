open Base
open !Hardcaml

(* Adds a buffer before the sink. It exposes the signal that is asserted when
   the sink will be ready in the next cycle. It can be used to improve timings. *)
let bufferize_stream reg_spec (sink : 'a Flow.Endpoint.t) =
  let open Signal in

  let source = Flow.Endpoint.create_empty () in 

  let store_in_to_out = Always.Variable.wire ~default:gnd in
  let store_in_to_buff = Always.Variable.wire ~default:gnd in
  let store_buff_to_out = Always.Variable.wire ~default:gnd in

  let buffer = Flow.Source.Of_always.reg reg_spec in
  let out = Flow.Source.Of_always.reg reg_spec in

  let assign_step (dst : Always.Variable.t Flow.Source.t) (src : Signal.t Flow.Source.t) = 
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
      assign_step out (Flow.Source.Of_always.value buffer)
    ]
  ]);

  source.dst.ready <== input_ready;
  Flow.Source.Of_signal.(sink.src <== (Flow.Source.Of_always.value out));

  source, input_ready_early

let shift_stream ~shift reg_spec (source : 'a Flow.Source.t) = 
  let open Signal in

  let shifted = Flow.Endpoint.create_empty () in

  let word_width = Flow.Source.port_widths.data in
  let empty_width = Flow.Source.port_widths.empty in
  let shift_remainder = word_width / 8 - shift in

  let shifted_source = Flow.Source.Of_always.wire zero in
  let shifted_ready_next = Always.Variable.wire ~default:gnd in

  let valid_transfer = shifted.dst.ready &: source.valid in
  let saved_transfer = Flow.Source.Of_signal.reg reg_spec ~enable:valid_transfer source in

  let will_require_extra_transfer = source.empty <:. shift_remainder in
  let extra_transfer = Always.Variable.reg ~width:1 reg_spec in

  Always.(compile [
    if_ extra_transfer.value [
      when_ shifted.dst.ready [
        extra_transfer <--. 0 
      ]
    ] [
      when_ valid_transfer [
        extra_transfer <-- (source.last &: will_require_extra_transfer)
      ]
    ]
  ]);

  Always.(compile [
    shifted_source.data <-- concat_msb [sel_bottom saved_transfer.data (shift_remainder * 8); sel_top source.data (shift * 8)];
    if_ extra_transfer.value [
      shifted_source.valid <-- vdd;
      shifted_source.empty <-- source.empty -: (of_int ~width:empty_width shift_remainder);
      shifted_source.last <-- vdd;
      shifted_ready_next <-- shifted.dst.ready
    ] [
      shifted_source.valid <-- source.valid;
      shifted_source.empty <-- mux2 (source.empty <=:. shift_remainder) (zero empty_width) (source.empty -:. shift_remainder);
      shifted_source.last <-- (source.last &: ((~:) will_require_extra_transfer));
      shifted_ready_next <-- (~:) (valid_transfer &: source.last &: will_require_extra_transfer)
    ]
  ]);

  Flow.Source.Of_signal.(shifted.src <== (Flow.Source.Of_always.value shifted_source));

  shifted, shifted_ready_next.value

module Header (Data : Interface.S) = struct
  type 'a t =
    { valid : 'a
    ; data : 'a Data.t
    }
  [@@deriving sexp_of, hardcaml]

  let data_len = List.fold Data.Names_and_widths.port_widths ~init:0 ~f:(+)
end

module Depacketizer (HeaderData : Interface.S) = struct
  module Header = Header(HeaderData)

  module States = struct
    type t =
      | Idle
      | ReadHeader
      | ReadPayload
    [@@deriving sexp_of, compare, enumerate]
  end

  let create reg_spec ~(sink : 'a Flow.Endpoint.t) ~(source : 'a Flow.Endpoint.t) =
    let open Signal in

    let word_width = Flow.Source.port_widths.data in
    let header_len = Header.data_len in

    (* sink_ready_early denotes if the sink will be ready in the next cycle *)
    let sink, sink_ready_early = bufferize_stream reg_spec sink in

    let header_shift = header_len % word_width in
    let header_aligned = header_shift = 0 in

    if header_len = 0 then raise_s [%message "packet header should have length bigger than 0"];
    if header_len % 8 <> 0 then raise_s [%message "packet header should have length divisible by 8"];
    
    let header = Header.Of_signal.wires () in
    let header_words = (header_len + word_width - 1) / word_width in
    let header_buf_width = header_words * word_width in

    let header_valid_next = Always.Variable.wire ~default:gnd in
    let header_buf = Always.Variable.reg ~width:header_buf_width reg_spec in
    let append_hdr_buf () = Always.(header_buf <-- ((sll header_buf.value word_width) |: (uresize source.src.data header_buf_width))) in

    let sm = Always.State_machine.create (module States) ~enable:vdd reg_spec in

    let hdr_word_counter = Always.Variable.reg ~width:(num_bits_to_represent header_words) reg_spec in

    let ready_next = Always.Variable.reg reg_spec ~width:1 in

    let shifted_source, shifted_source_ready_next = 
      if header_aligned then
        let ep = Flow.Endpoint.create_empty () in
        Flow.Source.Of_signal.assign ep.src source.src;
        ep, vdd
      else
        shift_stream ~shift:(header_shift / 8) reg_spec source.src
    in

    let sm_set_read_payload () = Always.(proc [
      header_valid_next <-- vdd;
      ready_next <-- sink_ready_early;
      sm.set_next ReadPayload
    ])
    in

    Always.(compile [
    sm.switch [
      Idle, [
        hdr_word_counter <--. 1;
        ready_next <--. 1;
        when_ source.src.valid [
          append_hdr_buf ();
          if (header_words = 1) then
            sm_set_read_payload ()
          else
            sm.set_next ReadHeader
        ]
      ];

      ReadHeader, [
        ready_next <--. 1;
        when_ source.src.valid [
          append_hdr_buf ();
          hdr_word_counter <-- hdr_word_counter.value +:. 1;
          when_ (hdr_word_counter.value ==:. header_words - 1) [
            sm_set_read_payload ()
          ];
        ]
      ];

      ReadPayload, [
        ready_next <-- (sink_ready_early &: shifted_source_ready_next);
        
        when_ (sink.dst.ready &: shifted_source.src.valid) [
          when_ shifted_source.src.last [
            ready_next <--. 1;
            sm.set_next Idle;
          ]
        ]
      ]
    ]
  ]);

  source.dst.ready <== ready_next.value;

  header.valid <== reg reg_spec header_valid_next.value;
  HeaderData.Of_signal.(header.data <== unpack ~rev:true (sel_top header_buf.value header_len));

  shifted_source.dst.ready <== source.dst.ready;
  sink.src.valid <== (shifted_source.src.valid &: (sm.is ReadPayload));
  sink.src.empty <== shifted_source.src.empty;
  sink.src.data <== shifted_source.src.data;
  sink.src.last <== shifted_source.src.last;

  header

end
