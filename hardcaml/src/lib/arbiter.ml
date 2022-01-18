open Base
open Hardcaml
open Hardcaml_circuits

let round_robin spec ~request ~acknowledge =
  let open Signal in

  let n = List.length request in
  let width = num_bits_to_represent n in
  let request_with_idx = List.mapi request ~f:(fun i valid ->
      { With_valid.valid; value = of_int ~width i })
  in
  let acknowledge_mask = concat_lsb acknowledge in

  let mask = Always.Variable.reg ~width:n spec in

  let granted_onehot = Always.Variable.wire ~default:(zero n) in
  let granted_reg_onehot = Always.Variable.reg ~width:n spec in
  let granted_reg_valid = reduce (bits_lsb granted_reg_onehot.value) ~f:( |: ) in

  let selected = Arbiters.Round_robin_with_priority.combinational 
    (module Signal) ~index:(Arbiters.Index.Offset mask.value) ~data:request_with_idx
  in
  let selected_onehot = sel_bottom (binary_to_onehot selected.value) n in

  Always.(compile [
    if_ granted_reg_valid [
      granted_onehot <-- granted_reg_onehot.value;
      when_ (granted_onehot.value &: acknowledge_mask) [
        granted_reg_onehot <--. 0;
      ]
    ] [
      when_ selected.valid [
        granted_onehot <-- selected_onehot;
        granted_reg_onehot <-- selected_onehot;
        mask <-- Arbiters.Index.next_mask mask.value;
      ]
    ]
  ]);

  granted_onehot.value