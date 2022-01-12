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