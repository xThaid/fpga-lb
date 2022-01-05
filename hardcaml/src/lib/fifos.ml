open !Base
open Hardcaml
open Signal

module Buffer (Data : Interface.S) = struct
  module I = struct
    type 'a t =
      { wr_data : 'a Data.t [@rtlprefix "wr_"]
      ; wr_enable : 'a
      ; rd_enable : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { rd_data : 'a Data.t [@rtlprefix "rd_"]
      ; rd_valid : 'a
      ; ready : 'a
      ; ready_next : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create spec (i : Signal.t I.t) =

    let buffer = Data.Of_always.reg spec in
    let out = Data.Of_always.reg spec in

    let store_in_to_out = Always.Variable.wire ~default:gnd in
    let store_in_to_buff = Always.Variable.wire ~default:gnd in
    let store_buff_to_out = Always.Variable.wire ~default:gnd in

    let buffer_valid = Always.Variable.reg ~width:1 spec in
    let out_valid = Always.Variable.reg ~width:1 spec in

    let ready_next = i.rd_enable |: (~:(buffer_valid.value) &: (~:(out_valid.value) |: ~:(i.wr_enable))) in
    let ready = reg spec ready_next in

    Always.(compile [
      if_ ready [
        (* Input is ready *)
        if_ (i.rd_enable |: ~:(out_valid.value)) [
          (* Output is ready or currently not valid, transfer data to output *)
          out_valid <-- i.wr_enable;
          store_in_to_out <--. 1
        ] [
          (* Output is not ready, store input in buffer *)
          buffer_valid <-- i.wr_enable;
          store_in_to_buff <--. 1
        ]
      ] @@ elif i.rd_enable [
        (* Input is not ready, but output is ready *)
        out_valid <-- buffer_valid.value;
        buffer_valid <--. 0;
        store_buff_to_out <--. 1
      ] [];

      if_ store_in_to_out.value [
        Data.Of_always.assign out i.wr_data
      ] @@ elif store_in_to_buff.value [
        Data.Of_always.assign buffer i.wr_data
      ] [];

      when_ store_buff_to_out.value [
        Data.Of_always.assign out (Data.Of_always.value buffer)
      ]
    ]);

    { O.rd_data = Data.Of_always.value out
    ; rd_valid = out_valid.value
    ; ready
    ; ready_next
    }

end