open Hardcaml

module AlwaysV2 = struct
  include Always

  let if_const b t f = proc (if b then t else f)
  let when_const b t = if_const b t []

end
