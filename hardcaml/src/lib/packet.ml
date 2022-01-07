open Base
open Hardcaml

module Packetizer (Data : Interface.S) = struct
  module Header = Transaction.Make(Data)
  module Serializer = Transaction.Serializer(Data)

  let create_packetizer reg_spec ~(hdr : Header.t) ~(source : Flow.t) =
    let header_shift = Header.data_len % Flow.word_width in

    let hdr_flow = Serializer.serialize reg_spec hdr in
    let joined_flow = Flow.join reg_spec ~shift:(header_shift / 8) ~source1:hdr_flow ~source2:source in

    Flow.bufferize reg_spec joined_flow

  let create_depacketizer reg_spec ~(source : Flow.t) =
    let flow1, flow2 = Flow.split reg_spec ~hdr_length:Header.data_len ~source in

    let hdr = Serializer.deserialize reg_spec flow1 in

    hdr, Flow.bufferize reg_spec flow2

end
