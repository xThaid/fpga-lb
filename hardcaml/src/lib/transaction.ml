open Base
open Hardcaml

module Make (Data : Interface.S) = struct
  module Src = struct
    type 'a t =
      { valid : 'a
      ; data : 'a Data.t
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

  let create s d = {s; d}
  let create_empty () = create (Src.Of_signal.wires ()) (Dst.Of_signal.wires ())

  let data_len = List.reduce_exn Data.Names_and_widths.port_widths ~f:(+)
end

module Serializer (Data : Interface.S) = struct
  module Transaction = Make(Data)

  let serialize spec (tst : Transaction.t) =
    let open Signal in

    if Transaction.data_len <= Flow.word_width then raise_s [%message "transaction data with length <= word width is not supported"];
    if Transaction.data_len % 8 <> 0 then raise_s [%message "transaction data should have length divisible by 8"];

    let empty_cnt = ((Flow.word_width - Transaction.data_len % Flow.word_width) % Flow.word_width) / 8 in
    let data_words = (Transaction.data_len + Flow.word_width - 1) / Flow.word_width in
    let data_buf_width = data_words * Flow.word_width in
    let data_buf = Always.Variable.reg ~width:data_buf_width spec in
    let data_packed = Data.Of_signal.pack ~rev:true tst.s.data in

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

        when_ tst.s.valid [
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

    tst.d.ready <== ready_next.value;

    flow_src.data <== (sel_top data_buf.value Flow.word_width);
    flow_src.last <== (data_word_counter.value ==:. data_words - 1);
    flow_src.empty <== (mux2 flow_src.last (of_int ~width:Flow.empty_width empty_cnt) (zero Flow.empty_width));
    flow_src.valid <== sm.is Write;

    Flow.create flow_src flow_dst

  let deserialize spec (flow : Flow.t) =
    let open Signal in

    if Transaction.data_len <= Flow.word_width then raise_s [%message "transaction data with length <= word width is not supported"];
    if Transaction.data_len % 8 <> 0 then raise_s [%message "transaction data should have length divisible by 8"];

    let tst = Transaction.create_empty () in
    let data_words = (Transaction.data_len + Flow.word_width - 1) / Flow.word_width in
    let data_buf_width = data_words * Flow.word_width in

    let tst_valid_next = Always.Variable.wire ~default:gnd in
    let data_buf = Always.Variable.reg ~width:data_buf_width spec in
    let append_data_buf () = Always.(data_buf <-- ((sll data_buf.value Flow.word_width) |: (uresize flow.src.data data_buf_width))) in

    let ready_next = Always.Variable.wire ~default:vdd in

    let module SM = struct
      type t =
        | Idle
        | Read
        | Wait
      [@@deriving sexp_of, compare, enumerate]
    end
    in

    let sm = Always.State_machine.create (module SM) ~enable:vdd spec in

    Always.(compile [
    sm.switch [
      Idle, [
        when_ flow.src.valid [
          append_data_buf ();
          sm.set_next Read;
        ]
      ];

      Read, [
        when_ (Flow.is_fired flow) [
          append_data_buf ();
          when_ flow.src.last [
            if_ tst.d.ready [
              tst_valid_next <-- vdd;
              sm.set_next Idle;
            ] [
              ready_next <--. 0;
              sm.set_next Wait;
            ]
          ];
        ]
      ];

      Wait, [
        ready_next <--. 0;

        when_ tst.d.ready [
          tst_valid_next <-- vdd;
          ready_next <--. 1;
          sm.set_next Idle;
        ]
      ]
    ];
    ]);

    flow.dst.ready <== (reg spec ready_next.value);

    tst.s.valid <== (reg spec tst_valid_next.value);
    Data.Of_signal.(tst.s.data <== unpack ~rev:true (sel_top data_buf.value Transaction.data_len));

    tst

end