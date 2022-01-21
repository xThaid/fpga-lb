open Hardcaml
open Lb_dataplane
open Lb_dataplane_test_lib
open Sim_elements
open Lwt

module DataplaneSim = struct
  module I = Dataplane.I
  module O = Dataplane.O

  let create_fn (scope : Scope.t) (i : Signal.t I.t) : (Signal.t O.t) =
    Dataplane.create_from_if scope i

end

let devname = "tap0"
let clock_freq = 100.0

let every period f =
  Lwt.async (fun () ->
    let rec loop () =
      f ()
      >>= fun () ->
      Lwt_unix.sleep period
      >>= fun () ->
      loop ()
    in
    loop ())

let () =
  let module Sim = Sim.Sim(DataplaneSim) in
  let module Emitter = AvalonFlowEmitter in
  let module Consumer = AvalonFlowConsumer in

  let sim = Sim.create ~name:"dataplane" ~gtkwave:false ~trace:false () in

  let inputs = Sim.inputs sim in
  let outputs = Sim.outputs sim in

  let emitter = Emitter.create inputs.rx outputs.rx in
  let consumer = Consumer.create outputs.tx inputs.tx in

  Sim.add_element sim (module Emitter) emitter;
  Sim.add_element sim (module Consumer) consumer;

  emitter.enabled <- true;
  consumer.enabled <- true;

  Stdio.printf "open %s\n%!" devname;
  let fd, _ = Tuntap.opentap ~pi:false ~devname ~persist:false () in
  Stdio.printf "ok hwaddr: %s\n%!" (Macaddr.to_string (Tuntap.get_macaddr devname));

  Tuntap.set_up_and_running devname;

  let lfd = Lwt_unix.of_unix_file_descr fd ~blocking:false in

  every (1.0 /. clock_freq) (fun () ->
    Sim.cycle sim;

    let packet = Consumer.take_transfer consumer in
    match packet with
    | Some data ->
      let buf = Lwt_bytes.of_bytes data in
      Lwt_unix.handle_unix_error (Lwt_bytes.write lfd buf 0) (Lwt_bytes.length buf)
      >>= fun len ->
      Stdio.printf "write: %d bytes\n%!" len;
      Lwt.return ()
    | None -> Lwt.return ()
  );

  let rec loop () =
    let buf = Lwt_bytes.create 4096 in
    Lwt_unix.handle_unix_error (Lwt_bytes.read lfd buf 0) (Lwt_bytes.length buf)
    >>= fun len ->
    Stdio.printf "read: %d bytes\n%!" len;
    let packet = Lwt_bytes.extract buf 0 len in
    Emitter.add_transfer emitter (Lwt_bytes.to_bytes packet);
    Lwt.return ()
    >>= loop
  in

  Lwt_main.run (loop ())
