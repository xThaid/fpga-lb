open Base
open !Hardcaml

module Header (Data : Interface.S) = struct
  type 'a t =
    { valid : 'a
    ; data : 'a Data.t
    }
  [@@deriving sexp_of, hardcaml]

  let data_len = List.fold Data.Names_and_widths.port_widths ~init:0 ~f:(+)
end

module Packetizer (HeaderData : Interface.S) = struct
  module Header = Header(HeaderData)

  let word_width = Flow.Source.port_widths.data
  let empty_width = Flow.Source.port_widths.empty
  let header_len = Header.data_len

  let header_disassemble reg_spec ~(hdr : 'a Header.t) =
    let open Signal in

    if header_len <= word_width then raise_s [%message "packet header with length <= word width is not supported"];
    if header_len % 8 <> 0 then raise_s [%message "packet header should have length divisible by 8"];

    let empty_cnt = ((word_width - header_len % word_width) % word_width) / 8 in
    let header_words = (header_len + word_width - 1) / word_width in
    let header_buf_width = header_words * word_width in
    let header_buf = Always.Variable.reg ~width:header_buf_width reg_spec in
    let header_packed = HeaderData.Of_signal.pack ~rev:true hdr.data in

    let hdr_word_counter = Always.Variable.reg ~width:(num_bits_to_represent header_words) reg_spec in

    let busy = Always.Variable.reg ~width:1 reg_spec in

    let flow_src = Flow.Source.Of_signal.wires () in
    let flow_dst = Flow.Dest.Of_signal.wires () in

    let latch_header () = Always.(proc [
      hdr_word_counter <--. 0;
      header_buf <-- (if empty_cnt = 0 then header_packed else (concat_msb [header_packed; zero (empty_cnt * 8)]));
    ]) in

    Always.(compile [
      if_ busy.value [
        when_ flow_dst.ready [
          hdr_word_counter <-- hdr_word_counter.value +:. 1;
          header_buf <-- (sll header_buf.value word_width);
          when_ (hdr_word_counter.value ==:. header_words - 1) [
            if_ hdr.valid [
              latch_header ();
            ] [
              busy <--. 0;
            ]
          ];
        ]
      ] [
        when_ hdr.valid [
          latch_header ();
          busy <--. 1;
        ]
      ]
    ]);

    flow_src.data <== (sel_top header_buf.value word_width);
    flow_src.last <== (hdr_word_counter.value ==:. header_words - 1);
    flow_src.empty <== (mux2 flow_src.last (of_int ~width:empty_width empty_cnt) (zero empty_width));
    flow_src.valid <== busy.value;

    Flow.Endpoint.create flow_src flow_dst

  let header_assemble reg_spec ~(source : 'a Flow.Endpoint.t) =
    let open Signal in

    if header_len <= word_width then raise_s [%message "packet header with length <= word width is not supported"];
    if header_len % 8 <> 0 then raise_s [%message "packet header should have length divisible by 8"];

    let header = Header.Of_signal.wires () in
    let header_words = (header_len + word_width - 1) / word_width in
    let header_buf_width = header_words * word_width in

    let header_valid_next = Always.Variable.wire ~default:gnd in
    let header_buf = Always.Variable.reg ~width:header_buf_width reg_spec in
    let append_hdr_buf () = Always.(header_buf <-- ((sll header_buf.value word_width) |: (uresize source.src.data header_buf_width))) in

    let busy = Always.Variable.reg ~width:1 reg_spec in

    Always.(compile [
      if_ busy.value [
        when_ source.src.valid [
          append_hdr_buf ();
          when_ source.src.last [
            header_valid_next <-- vdd;
            busy <--. 0;
          ];
        ]
      ] [
        when_ source.src.valid [
          append_hdr_buf ();
          busy <--. 1;
        ]
      ]
    ]);

    source.dst.ready <== vdd;

    header.valid <== (reg reg_spec header_valid_next.value);
    HeaderData.Of_signal.(header.data <== unpack ~rev:true (sel_top header_buf.value header_len));

    header

  let create_packetizer reg_spec ~(hdr : 'a Header.t) ~(source : 'a Flow.Endpoint.t) =
    let header_shift = header_len % word_width in

    let hdr_flow = header_disassemble reg_spec ~hdr in
    let joined_flow = Flow.Endpoint.join reg_spec ~shift:(header_shift / 8) ~source1:hdr_flow ~source2:source in

    Flow.Endpoint.bufferize reg_spec joined_flow

  let create_depacketizer reg_spec ~(source : 'a Flow.Endpoint.t) =
    let flow1, flow2 = Flow.Endpoint.split reg_spec ~hdr_length:header_len ~source in

    let hdr = header_assemble reg_spec ~source:flow1 in

    hdr, Flow.Endpoint.bufferize reg_spec flow2

end
