open !Base
open !Hardcaml

module EthernetHeader = struct 
  type 'a t =
    { dest_mac : 'a[@bits 48]
    ; src_mac : 'a[@bits 48]
    ; ether_type : 'a[@bits 16]
    }
  [@@deriving sexp_of, hardcaml]
end

module ArpPacket = struct
  type 'a t =
    { htype : 'a [@bits 16]
    ; ptype : 'a [@bits 16]
    ; hlen : 'a [@bits 8]
    ; plen : 'a [@bits 8]
    ; oper : 'a [@bits 16]
    ; sha : 'a [@bits 48]
    ; spa : 'a [@bits 32]
    ; tha : 'a [@bits 48]
    ; tpa : 'a [@bits 32]
    }
  [@@deriving sexp_of, hardcaml]
end

module IPv4Header = struct
  type 'a t =
    { version : 'a [@bits 4]
    ; ihl : 'a [@bits 4]
    ; dscp : 'a [@bits 6]
    ; ecn : 'a [@bits 2]
    ; total_length : 'a [@bits 16]
    ; identification : 'a [@bits 16]
    ; flags : 'a [@bits 3]
    ; fragment_offset : 'a [@bits 13]
    ; ttl : 'a [@bits 8]
    ; protocol : 'a [@bits 8]
    ; hdr_checksum : 'a [@bits 16]
    ; src_ip : 'a [@bits 32]
    ; dst_ip : 'a [@bits 32]
    }
  [@@deriving sexp_of, hardcaml]
end

module UDPHeader = struct
  type 'a t =
    { src_port : 'a [@bits 16]
    ; dst_port : 'a [@bits 16]
    ; length : 'a [@bits 16]
    ; checksum : 'a [@bits 16]
    }
  [@@deriving sexp_of, hardcaml]
end

module ICMPEchoRequest = struct
  type 'a t =
    { type_of_msg : 'a [@bits 8]
    ; code : 'a [@bits 8]
    ; checksum : 'a [@bits 16]
    ; identifier : 'a [@bits 16]
    ; seq_number : 'a [@bits 16]
    }
  [@@deriving sexp_of, hardcaml]
end