type t =
  | AtLeastOneNode

exception Error of t

let print_error pp = function
  | AtLeastOneNode ->
    Format.fprintf pp "error: AST requires at least one node"

let try_error f =
  try
    f ()
  with Error e -> print_error Format.std_formatter e
