open Camlp4.PreCast

let (|>) = BatPervasives.(|>)
let _loc = Loc.ghost

module OCamlPrinter = Printers.OCaml

module Emit = struct
  let rec program definitions =
    <:str_item<

      open Sexplib.Conv
      $List.map definition definitions |> Ast.stSem_of_list$

    >>

  and definition =
    function
    | Program.Ast (name, nodes) ->

      let first_type :: rest_types = List.map ast_node nodes in
      let (first_name,_) :: _ = nodes in
      <:str_item<

        module $uid:Ident.string_of_uident name$ = struct
          module SourceLocation = struct
            module Lexing = struct
              include Lexing
              let sexp_of_position _ = sexp_of_int 0
              let position_of_sexp _ = Lexing.dummy_pos
            end
            type t = Lexing.position * Lexing.position

          end
            module rec $first_type$ and $Ast.mbAnd_of_list rest_types$

            type t = $uid:Ident.string_of_uident first_name$.t;;
            let t_of_sexp = $uid:Ident.string_of_uident first_name$.t_of_sexp;;
            let sexp_of_t = $uid:Ident.string_of_uident first_name$.sexp_of_t;;

        end
      >>
    | Program.Map (nm, (st,dt), nodes) ->
      let methods = List.map (rewrite_node (st,dt)) nodes in
      <:str_item<

        class $lid:Ident.string_of_lident nm$ = object (self : 'a)
            $methods |> Ast.crSem_of_list$
          method sourceLocation x = x
        end
      >>

  and ast_node (nm, nd) =
    match nd with
    | Program.CustomNode clauses ->

        <:module_binding<
          $uid:Ident.string_of_uident nm$ :
          sig
            type t = $List.map ast_clause clauses |> Ast.tyOr_of_list$ | SEXP
            val sexp_of_t : t -> Sexplib.Sexp.t;;
            val t_of_sexp : Sexplib.Sexp.t -> t;;
          end =
          struct
            type t = $List.map ast_clause clauses |> Ast.tyOr_of_list$ | SEXP
          end

        >>

    | Program.NativeNode name ->

        <:module_binding<

          $uid:Ident.string_of_uident nm$ :
          sig
            type t = $lid:Ident.string_of_lident name$
          end = struct
            type t = $lid:Ident.string_of_lident name$
          end

        >>

    | Program.AliasNode t ->
        <:module_binding<

          $uid:Ident.string_of_uident nm$ :
          sig
            type t = ($tycon t$)
          end = struct
            type t = ($tycon t$)
          end
        >>
  and tycon args =
    let rec loop =
      function
      | Constr.Tycon nm ->
          <:ctyp< $uid:(Ident.string_of_uident nm)$.t >>
      | Constr.List ty ->
          <:ctyp< $loop ty$ list >>
      | Constr.Option ty ->
          <:ctyp< $loop ty$ option >>
    in
    loop args

  and ast_clause (nm, args) =
    <:ctyp< $uid:Ident.string_of_uident nm$ of $List.map tycon args |> Ast.tySta_of_list $>>

  and rewrite_node typing (nm, cl) =
    <:class_str_item<

        method $lid:Ident.string_of_lident (Ident.lident_of_uident nm)$ =
          function $List.map (rewrite_clause typing) cl |> Ast.mcOr_of_list$

    >>

 and rewrite_clause (st,dt) (l,r) = <:match_case< $tup:paTree st l$ -> $tup:exTree dt r$>>

 and paTy = function
 | Constr.Tycon nm -> <:patt<$uid:Ident.string_of_uident nm$>>

 and paTree t = function
 | Tree.Tree (Constr.Tycon ty,(nm, lst)) -> <:patt< $uid:Ident.string_of_uident t$. $uid:Ident.string_of_uident ty$ . $uid:Ident.string_of_uident nm$ $List.map (paTree t) lst |> Ast.paCom_of_list$>>
 | Tree.Var (_,nm) -> <:patt< $lid:Ident.string_of_lident nm$ >>
 | Tree.Const (Tree.String str) -> <:patt<$str:str$>>
 | Tree.Const (Tree.Int i) -> <:patt<$int:string_of_int i$>>

 and exTree t = function
 | Tree.Tree (Constr.Tycon ty,(nm, lst)) -> constr t ty (nm, lst)
 | Tree.Var (ty,nm) ->
     let id str = Ident.string_of_lident (Ident.lident_of_uident str) in
     let rec tycon = function
     | Constr.List ty -> <:expr<List.map (fun $lid:Ident.string_of_lident nm$ -> $tycon ty$) $lid:Ident.string_of_lident nm$>>
     | Constr.Option ty -> <:expr<(match $lid:Ident.string_of_lident nm$ with Some $lid:Ident.string_of_lident nm$ -> Some ($tycon ty$) | None -> None )>>
     | Constr.Tycon ty ->
         match id ty with
         | "string"|"int"|"int32"|"int64"|"bool"->
         <:expr< $lid:Ident.string_of_lident nm$ >>
         | _ ->
         <:expr< self # $lid:id ty$ $lid:Ident.string_of_lident nm$ >>
     in
     tycon ty
 | Tree.Const (Tree.String str) -> <:expr<$str:str$>>
 | Tree.Const (Tree.Int i) -> <:expr<$int:string_of_int i$>>

 and constr t ty (nm, lst) =
   let e = List.map (exTree t) lst |> Ast.exCom_of_list in
   List.fold_left (fun e1 e2 -> <:expr< $e1$ $e2$ >>) <:expr< $uid:Ident.string_of_uident t$ . $uid:Ident.string_of_uident ty$ . $uid:Ident.string_of_uident nm$>>
     (Ast.list_of_expr e [])

end

let output_program file program =
  Emit.program program
  |> OCamlPrinter.print_implem ~output_file:file
