

module Loc = struct

  module Map = Map.Make(struct
    type t = Ident.uident * Ident.uident
    let compare = Pervasives.compare
  end)

  let create node_name clause = node_name, Constr.name clause
end
