module T = Tree

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                  Define operation to transform nodes                  | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

type t = location * operation
and operation =
    Remove of arg
  | Replace of arg * arg
  | Subst of arg * node
