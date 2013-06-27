open Sexplib.Conv

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                           Definition of AST                           | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

type 'a t = 'a program
and 'a program = 'a definition list
and 'a definition =
    Ast of Ident.uident * ast_node list
  | Map of Ident.lident * type_decl * 'a rewrite_node list
and ast_node = Ident.uident * node
and 'a rewrite_node = Ident.uident * 'a rewrite_clause list
and 'a rewrite_clause = 'a Tree.t * 'a Tree.t
and node =
    CustomNode of clause list
  | NativeNode of Ident.lident
  | AliasNode of Constr.tycon
and type_decl = Ident.uident * Ident.uident
and clause = Constr.t
and arg = int
with sexp

type untyped_program = unit program
with sexp
type typed_program = Constr.tycon program
with sexp

open ExtFormat
let f = fprintf

let pp_biarrow_sep pp () = f pp "@ =>@ "

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                       This is our base printer                        | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

class virtual ['a] base_print = object (self)
  method program pp p = f pp "@[<v>%a@]@;@." (pp_list self # definition pp_space_sep) p
  method definition pp (x : 'a definition) =
    match x with
    Ast (nm, nodes) ->
      f pp "@[<v>@[<h>ast@ %a@]@;{@[<v 2>@;@[<v>%a@]@]@;}@]@;"
        Ident.pp_uident nm (pp_list self # ast_node pp_space_sep) nodes
  | Map (nm, types, nodes) ->
      f pp "@[<v>@[<h>map@ %a@ %a@]@;{@[<v 2>@;@[<v>%a@]@]@;}@]@;"
        Ident.pp_lident nm self # type_decl types
        (pp_list self # rewrite_node pp_space_sep) nodes
  method ast_node pp (nm, node) = f pp "@[<hov>%a:@ %a@]" Ident.pp_uident nm self # node node
  method rewrite_node pp (nm, clauses) =
    f pp "@[<hov>%a:@ %a@]" Ident.pp_uident nm (pp_list self # rewrite_clause pp_newline_bar_sep) clauses
  method node pp = function
    CustomNode clauses -> f pp "%a" (pp_list self # clause pp_newline_bar_sep) clauses
  | NativeNode name -> Ident.pp_lident pp name
  | AliasNode t -> (new Constr.print) # tycon pp t
  method type_decl pp (s,d) =
    f pp "@[<hov 2>%a@]" (pp_list Ident.pp_uident pp_biarrow_sep) [s;d]
  method clause = (new Constr.print) # constr
end

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                     Printer for untyped programs                      | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

class print = object (self : 'a)
  inherit [unit] base_print
  (* This could be done by passing the object and setting up a type constraint *)
  method rewrite_clause pp (ltree, rtree) =
    f pp "@[<hov 2>%a@ =>@ %a@]" (new Tree.print) # tree ltree (new Tree.print) # tree rtree
end

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                      Printer for typed programs                       | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

class typed_print = object (self : 'a)
  inherit [Constr.tycon] base_print
  method rewrite_clause pp (ltree, rtree) =
    f pp "@[<hov 2>%a@ =>@ %a@]" (new Tree.typed_print) # tree ltree (new Tree.typed_print) # tree rtree
end

let output_untyped_program channel s =
  Sexplib.Sexp.output_hum channel (sexp_of_untyped_program s);
  output_char channel '\n'

let output_typed_program channel s =
  Sexplib.Sexp.output_hum channel (sexp_of_typed_program s);
  output_char channel '\n'
