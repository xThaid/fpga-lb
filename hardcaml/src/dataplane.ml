open Hardcaml

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; foo : 'a [@bits 8]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { foo_d : 'a [@bits 8] }
  [@@deriving sexp_of, hardcaml]
end

let create (_scope : Scope.t) (input : Signal.t I.t) =
  let spec_with_clear =
    Reg_spec.create ~clock:input.clock ~clear:input.clear ()
  in
  let foo_d = Signal.reg spec_with_clear ~enable:Signal.vdd input.foo in
  { O. foo_d }

let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
  let module H = Hierarchy.In_scope(I)(O) in
  H.hierarchical ~scope ~name:"dataplane" create input
