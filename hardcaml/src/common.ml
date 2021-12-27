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
