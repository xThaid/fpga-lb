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
  let module BusHost = BusHost(Dataplane.BusAgent) in

  let sim = Sim.create ~name:"dataplane" ~gtkwave:false ~trace:false () in

  let inputs = Sim.inputs sim in
  let outputs = Sim.outputs sim in

  let emitter = Emitter.create inputs.rx outputs.rx in
  let consumer = Consumer.create outputs.tx inputs.tx in
  let bus = BusHost.create outputs.bus inputs.bus in

  Sim.add_element sim (module BusHost) bus;
  Sim.add_element sim (module Emitter) emitter;
  Sim.add_element sim (module Consumer) consumer;

  let write_vip_map ip idx =
    BusHost.schedule_write bus 64 (Constant.to_int (Constant.of_hex_string ~signedness:Unsigned ~width:32 ip));
    BusHost.schedule_write bus 65 idx
  in

  let write_hash_ring vip_idx slot real_idx = 
    BusHost.schedule_write bus 67 real_idx;
    BusHost.schedule_write bus 66 ((vip_idx * Balancer.Consts.ring_size) + slot);
  in

  let write_real_info real_idx real_ip =
    BusHost.schedule_write bus 69 (Constant.to_int (Constant.of_hex_string ~signedness:Unsigned ~width:32 real_ip));
    BusHost.schedule_write bus 68 real_idx;
  in
  
  Sim.cycle_n sim 2;

  emitter.enabled <- true;
  consumer.enabled <- true;

  write_vip_map "0a001001" 2;
  write_vip_map "0a001002" 3;

  for i = 0 to 1 do
    write_hash_ring 2 (i + 0) (i + 10);
    write_hash_ring 2 (i + 2) (i + 10);
    write_hash_ring 2 (i + 4) (i + 10);
    write_hash_ring 2 (i + 6) (i + 10);
  done;

  for i = 0 to 3 do
    write_hash_ring 3 (i + 0) (i + 20);
    write_hash_ring 3 (i + 4) (i + 20)
  done;

  write_real_info 0 "efefefef";

  write_real_info 10 "face0001";
  write_real_info 11 "face0002";

  write_real_info 20 "beef0001";
  write_real_info 21 "beef0002";
  write_real_info 22 "beef0003";
  write_real_info 23 "beef0004";

  Sim.cycle_n sim 40;

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
