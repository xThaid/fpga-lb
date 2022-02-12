open Hardcaml

module Tick = Transaction.Make(Interface.Empty)

let create cnt_width spec ~enable ~top =
  let open Signal in

  let counter = Always.Variable.reg ~width:cnt_width spec in

  let overflow = counter.value >=: top in
  let tick = Tick.create ~valid:(overflow &: enable) ~data:(Interface.Empty.map Interface.Empty.port_widths ~f:zero) in

  Always.(compile [
    if_ (Tick.is_fired tick) [
      counter <--. 0;
    ] @@ elif (enable &: ~:(overflow)) [
      counter <-- counter.value +:. 1;
    ] []
  ]);

  tick