(** A timer that generates a tick (transaction) every time the counter reaches
    the top value. *)

open Hardcaml

module Tick = Transaction.Make(Interface.Empty)

let create counter_width spec ~enable ~top =
  let open Signal in

  let counter = Always.Variable.reg ~width:counter_width spec in

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