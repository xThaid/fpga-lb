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
  let granted_onehot = Always.Variable.reg ~width:n spec in
  let granted_valid = reduce (bits_lsb granted_onehot.value) ~f:( |: ) in

  let selected = Arbiters.Round_robin_with_priority.combinational 
    (module Signal) ~index:(Arbiters.Index.Offset mask.value) ~data:request_with_idx
  in
  let selected_onehot = binary_to_onehot selected.value in

  let select_next () = Always.(
    when_ selected.valid [
      granted_onehot <-- selected_onehot;
      mask <-- Arbiters.Index.next_mask mask.value;
    ]
  ) in

  Always.(compile [
    if_ granted_valid [
      when_ (granted_onehot.value &: acknowledge_mask) [
        granted_onehot <--. 0;
        select_next ();
      ]
    ] [
      select_next ();
    ]
  ]);

  granted_onehot.value