open Sexplib.Conv

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                            Tree definition                            | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

type 'a t = 'a tree
and 'a tree =
    Tree of 'a * (Ident.uident * 'a t list)
  | Var of 'a * Ident.lident
  | Const of const
and const =
    String of string
  | Int of int
with sexp

open ExtFormat
let f = fprintf

let type_of = function
| Tree (ty, _) -> ty
| Var (ty,_) -> ty
| Const (String _) -> Constr.Tycon (Ident.uident "string")
| Const (Int _) -> Constr.Tycon (Ident.uident "string")

class print = object (self : 'a)
  method tree pp = function
  | Tree ((), (nm, lst)) -> f pp "@[<hov>(%a@ @[<hov 2>%a@])@]" Ident.pp_uident nm (pp_list self # tree pp_space_sep) lst
  | Var ((),nm) -> Ident.pp_lident pp nm
  | Const c -> f pp "%a" self#const c
  method const pp = function
  | Int i -> f pp "%d" i
  | String s -> f pp "\"%s\"" s
end

class typed_print = object (self : 'a)
  method tree pp = function
  | Tree (ty, (nm, lst)) -> f pp "@[<hov>((%a@ @[<hov 2>%a@])@ : @ %a)@]" Ident.pp_uident nm (pp_list self # tree pp_space_sep) lst (new Constr.print) # tycon ty
  | Var (ty,nm) -> f pp "@[<hov>(%a@ : @ %a)@]" Ident.pp_lident nm (new Constr.print) # tycon ty
  | Const c -> f pp "%a" self#const c
  method const pp = function
  | Int i -> f pp "%d" i
  | String s -> f pp "\"%s\"" s

end
