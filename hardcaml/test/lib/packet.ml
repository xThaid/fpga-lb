open Base
open Hardcaml
open Lb_dataplane

let serialize b = 
  Bits.split_msb ~part_width:8 b |> List.map ~f:Bits.to_char

let serialize_eth_hdr (hdr : Bits.t Common.EthernetHeader.t) =
  Common.EthernetHeader.Of_bits.pack ~rev:true hdr |> serialize

let serialize_arp_pkt (pkt : Bits.t Common.ArpPacket.t) =
  Common.ArpPacket.Of_bits.pack ~rev:true pkt |> serialize

let serialize_ipv4_hdr (pkt : Bits.t Common.IPv4Header.t) =
  Common.IPv4Header.Of_bits.pack ~rev:true pkt |> serialize

let serialize_icmp_echo_req (req : Bits.t Common.ICMPEchoRequest.t) =
  Common.ICMPEchoRequest.Of_bits.pack ~rev:true req |> serialize

let serialize_udp_hdr hdr = Common.UDPHeader.Of_bits.pack ~rev:true hdr |> serialize

let hex_to_bits width str = Bits.of_hex ~width str

let create_eth_hdr src_mac dest_mac ether_type = 
  { Common.EthernetHeader.src_mac
  ; dest_mac
  ; ether_type
  } |> 
  Common.EthernetHeader.map2 Common.EthernetHeader.port_widths ~f:hex_to_bits

let create_arp_pkt oper sha spa tha tpa =
  { Common.ArpPacket.htype = "0001"
  ; ptype = "0800"
  ; hlen = "06"
  ; plen = "04"
  ; oper; sha; spa; tha; tpa
  } |>
  Common.ArpPacket.map2 Common.ArpPacket.port_widths ~f:hex_to_bits

let create_ipv4_hdr total_length protocol src_ip dst_ip =
  let pkt = 
    { Common.IPv4Header.version = "4"
    ; ihl = "5"
    ; dscp = "0"
    ; ecn = "0"
    ; total_length = Constant.to_hex_string ~signedness:Unsigned (Constant.of_int ~width:16 total_length)
    ; identification = "0000"
    ; flags = "0"
    ; fragment_offset = "0"
    ; ttl = "ff"
    ; protocol
    ; hdr_checksum = "0000"
    ; src_ip
    ; dst_ip
    } |> 
    Common.IPv4Header.map2 Common.IPv4Header.port_widths ~f:hex_to_bits
  in
  let checksum = Ip.calc_checksum (module Bits) pkt in
  {pkt with hdr_checksum = checksum}

let create_udp_hdr src_port dst_port len =
  { Common.UDPHeader.src_port
  ; dst_port
  ; length = Constant.to_hex_string ~signedness:Unsigned (Constant.of_int ~width:16 len)
  ; checksum = "0000"
  } |>
  Common.UDPHeader.map2 Common.UDPHeader.port_widths ~f:hex_to_bits

let create_icmp_echo_req identifier seq_number = 
  let req = 
    { Common.ICMPEchoRequest.type_of_msg = "8"
    ; code = "0"
    ; checksum = "0"
    ; identifier
    ; seq_number
    } |> Common.ICMPEchoRequest.map2 Common.ICMPEchoRequest.port_widths ~f:hex_to_bits
  in
  {req with checksum = Hashes.one_complement_sum (module Bits) (Common.ICMPEchoRequest.Of_bits.pack ~rev:true req)}

let create_arp_req src_mac spa tpa =
  let eth = create_eth_hdr src_mac "ffffffffffff" "0806" in
  let arp = create_arp_pkt "01" src_mac spa "000000000000" tpa in
  let padding = (List.init 18 ~f:(fun _ -> Char.of_int_exn 0)) in
  
  let data = serialize_eth_hdr eth @ serialize_arp_pkt arp @ padding in
  Bytes.of_char_list data

let create_arp_resp src_mac spa =
  let eth = create_eth_hdr src_mac "ffffffffffff" "0806" in
  let arp = create_arp_pkt "02" src_mac spa "000000000000" spa in
  let padding = (List.init 18 ~f:(fun _ -> Char.of_int_exn 0)) in
  
  let data = serialize_eth_hdr eth @ serialize_arp_pkt arp @ padding in
  Bytes.of_char_list data

let create_icmp_echo_req src_mac dst_mac src_ip dst_ip id seq =
  let eth = create_eth_hdr src_mac dst_mac "0800" in
  let icmp = create_icmp_echo_req id seq in
  let icmp_serialized = serialize_icmp_echo_req icmp in

  let ipv4 = create_ipv4_hdr ((List.length icmp_serialized) + 20) "01" src_ip dst_ip in

  let data = serialize_eth_hdr eth @ serialize_ipv4_hdr ipv4 @ icmp_serialized in
  Bytes.of_char_list data
