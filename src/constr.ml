open Sexplib.Conv

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                          Clause constructor                           | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

type t = constr
and constr = Ident.uident * tycon list
and tycon =
    Tycon of Ident.uident
  | List of tycon
  | Option of tycon
with sexp

open ExtFormat

let f = fprintf

class print = object (self : 'a)
  method constr pp (nm, l) =
    f pp "%a@ %a" Ident.pp_uident nm (pp_list self#tycon pp_space_sep) l
  method tycon pp = function
  | Tycon nm -> f pp "%a" Ident.pp_uident nm
  | List ty -> f pp "[%a]" self # tycon ty
  | Option ty -> f pp "?%a" self # tycon ty
end

let name = fst
let tycon = snd

let rec iter_tycon f = function
| Tycon ident -> f ident
| List tycon
| Option tycon -> iter_tycon f tycon
