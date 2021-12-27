open !Hardcaml

module Header (Data : Interface.S) = struct
  open Base

  type 'a t =
    { valid : 'a
    ; data : 'a Data.t
    }
  [@@deriving sexp_of, hardcaml]

  let data_len = List.fold Data.Names_and_widths.port_widths ~init:0 ~f:(+)
end

module Depacketizer (Params : Interface.S) (HeaderData : Interface.S) = struct 
  module Endpoint = Stream.Endpoint(Params)
  module Src2Si = Endpoint.SourceToSink
  module Header = Header(HeaderData)
  open Base

  (* Adds a buffer before the sink. It exposes the signal that is asserted when
   the sink will be ready in the next cycle. It can be used to improve timings. *)
  let bufferize_stream reg_spec (sink : 'a Endpoint.t) =
    let open Signal in

    let source = Endpoint.create () in 

    let store_in_to_out = Always.Variable.wire ~default:gnd in
    let store_in_to_buff = Always.Variable.wire ~default:gnd in
    let store_buff_to_out = Always.Variable.wire ~default:gnd in

    let buffer = Src2Si.Of_always.reg reg_spec in
    let out = Src2Si.Of_always.reg reg_spec in

    let assign_step (dst : Always.Variable.t Src2Si.t) (src : Signal.t Src2Si.t) = 
      Always.(proc [
        dst.data <-- src.data;
        dst.empty <-- src.empty;
        dst.last <-- src.last;
      ])
    in

    (* Enable ready input next cycle if output is ready or the buffer will not be filled on the next cycle *)
    let input_ready_early = sink.si2src.ready |: (((~:) buffer.valid.value) &: (((~:) out.valid.value) |: ((~:) source.src2si.valid))) in
    let input_ready = reg reg_spec input_ready_early in

    Always.(compile [
      if_ input_ready [
        (* Input is ready *)
        if_ (sink.si2src.ready |: ((~:) out.valid.value)) [
          (* Output is ready or currently not valid, transfer data to output *)
          out.valid <-- source.src2si.valid;
          store_in_to_out <--. 1
        ] [
          (* Output is not ready, store input in buffer *)
          buffer.valid <-- source.src2si.valid;
          store_in_to_buff <--. 1
        ]
      ] (elif sink.si2src.ready [
        (* Input is not ready, but output is ready *)
        out.valid <-- buffer.valid.value;
        buffer.valid <--. 0;
        store_buff_to_out <--. 1
      ] []);

      if_ store_in_to_out.value [
        assign_step out source.src2si
      ] (elif store_in_to_buff.value [
        assign_step buffer source.src2si
      ] []);

      when_ store_buff_to_out.value [
        assign_step out (Src2Si.Of_always.value buffer)
      ]
    ]);

    source.si2src.ready <== input_ready;
    Params.Of_signal.(sink.params <== source.params);
    Src2Si.Of_signal.(sink.src2si <== (Src2Si.Of_always.value out));

    source, input_ready_early

    let shift_stream shift_offset reg_spec (source : 'a Src2Si.t) = 
      let open Signal in

      let shifted = Endpoint.create () in

      let word_width = Src2Si.port_widths.data in
      let empty_width = Src2Si.port_widths.empty in
      let shift_remainder = word_width / 8 - shift_offset in

      let shifted_source = Src2Si.Of_always.wire zero in
      let shifted_ready_next = Always.Variable.wire ~default:gnd in

      let valid_transfer = shifted.si2src.ready &: source.valid in
      let saved_transfer = Src2Si.Of_signal.reg reg_spec ~enable:valid_transfer source in

      let will_require_extra_transfer = source.empty <:. shift_offset in
      let extra_transfer = Always.Variable.reg ~width:1 reg_spec in

      Always.(compile [
        if_ extra_transfer.value [
          when_ shifted.si2src.ready [
            extra_transfer <--. 0 
          ]
        ] [
          when_ valid_transfer [
            extra_transfer <-- (source.last &: will_require_extra_transfer)
          ]
        ]
      ]);

      Always.(compile [
        if_ extra_transfer.value [
          shifted_source.data <-- uresize (sel_bottom saved_transfer.data (shift_remainder * 8)) word_width;
          shifted_source.valid <-- vdd;
          shifted_source.empty <-- (of_int ~width:empty_width shift_offset) -: source.empty;
          shifted_source.last <-- vdd;
          shifted_ready_next <-- shifted.si2src.ready
        ] [
          shifted_source.data <-- concat_msb [sel_bottom saved_transfer.data (shift_remainder * 8); sel_top source.data (shift_offset * 8)];
          shifted_source.valid <-- source.valid;
          shifted_source.empty <-- mux2 (source.empty <=:. shift_remainder) (zero empty_width) (source.empty -:. shift_remainder);
          shifted_source.last <-- (source.last &: ((~:) will_require_extra_transfer));
          shifted_ready_next <-- (~:) (valid_transfer &: source.last &: will_require_extra_transfer)
        ]
      ]);

      Src2Si.Of_signal.(shifted.src2si <== (Src2Si.Of_always.value shifted_source));

      shifted, shifted_ready_next.value

  module States = struct
    type t =
      | Idle
      | ReadHeader
      | ReadPayload
    [@@deriving sexp_of, compare, enumerate]
  end
  let create reg_spec (sink : 'a Endpoint.t) (source : 'a Endpoint.t) =
    let open Signal in

    let word_width = Src2Si.port_widths.data in
    let header_len = Header.data_len in

    (* sink_ready_early denotes if the internal will be ready in the next cycle *)
    let sink, sink_ready_early = bufferize_stream reg_spec sink in

    let header_leftover = header_len % word_width in
    let header_offset = word_width - header_leftover in
    let header_aligned = header_offset = 0 in

    if header_len = 0 then raise_s [%message "packet header should have length bigger than 0"];
    if header_len % 8 <> 0 then raise_s [%message "packet header should have length divisible by 8"];
    
    let header = Header.Of_signal.wires () in
    let header_words = (header_len + word_width - 1) / word_width in
    let header_buf_width = header_words * word_width in

    let header_valid = Always.Variable.wire ~default:gnd in
    let header_buf = Always.Variable.reg ~width:header_buf_width reg_spec in
    let append_hdr_buf () = Always.(header_buf <-- ((sll header_buf.value word_width) |: (uresize source.src2si.data header_buf_width))) in

    let sm = Always.State_machine.create (module States) ~enable:vdd reg_spec in

    let hdr_word_counter = Always.Variable.reg ~width:(num_bits_to_represent header_words) reg_spec in

    let ready_next = Always.Variable.reg reg_spec ~width:1 in

    let shifted_source, shifted_source_ready_next = 
      if header_aligned then
        let ep = Endpoint.create () in
        Src2Si.Of_signal.assign ep.src2si source.src2si;
        ep, vdd
      else
        shift_stream (header_offset / 8) reg_spec source.src2si
    in

    let switch_to_read_payload () = Always.(proc [
      header_valid <-- vdd;
      ready_next <-- sink_ready_early;
      sm.set_next ReadPayload
    ])
    in

    Always.(compile [
    sm.switch [
      Idle, [
        hdr_word_counter <--. 1;
        ready_next <--. 1;
        when_ source.src2si.valid [
          append_hdr_buf ();
          if (header_words = 1) then
            sm.set_next ReadHeader
          else
            switch_to_read_payload ()
        ]
      ];

      ReadHeader, [
        ready_next <--. 1;
        when_ source.src2si.valid [
          append_hdr_buf ();
          hdr_word_counter <-- hdr_word_counter.value +:. 1;
          when_ (hdr_word_counter.value ==:. header_words - 1) [
            switch_to_read_payload ()
          ];
        ]
      ];

      ReadPayload, [
        ready_next <-- (sink_ready_early &: shifted_source_ready_next);
        
        when_ (sink.si2src.ready &: shifted_source.src2si.valid) [
          when_ shifted_source.src2si.last [
            sm.set_next Idle;
          ]
        ]
      ]
    ]
  ]);

  Params.Of_signal.(sink.params <== source.params);
  source.si2src.ready <== ready_next.value;

  header.valid <== header_valid.value;
  HeaderData.Of_signal.(header.data <== unpack (sel_top header_buf.value header_len));

  shifted_source.si2src.ready <== source.si2src.ready;
  sink.src2si.valid <== (shifted_source.src2si.valid &: (sm.is ReadPayload));
  sink.src2si.empty <== shifted_source.src2si.empty;
  sink.src2si.data <== shifted_source.src2si.data;
  sink.src2si.last <== shifted_source.src2si.last;

  header

end
