open Camlp4.PreCast

let (|>) = BatPervasives.(|>)
let _loc = Loc.ghost

module OCamlPrinter = Printers.OCaml

module Emit = struct
  let rec program definitions =
    List.map definition definitions
    |> Ast.stSem_of_list

  and definition =
    function
    | Program.Ast (name, nodes) ->
      let first_type :: rest_types = List.map ast_node nodes in
      <:str_item<

        module $uid:Ident.string_of_uident name$ = struct
            type t = $first_type$
            and $rest_types |> Ast.tyAnd_of_list$
        end

      >>
    | Program.Map (nm, (st,dt), nodes) ->
      let methods = List.map (rewrite_node (st,dt)) nodes in
      <:str_item<

        class $lid:Ident.string_of_lident nm$ = object (self : 'a)
            $methods |> Ast.crSem_of_list$
        end
      >>

  and ast_node (nm, nd) =
    match nd with
    | Program.CustomNode clauses ->
        <:ctyp<

        $lid:Ident.string_of_lident (Ident.lident_of_uident nm)$ = $List.map ast_clause clauses |> Ast.tyOr_of_list$

        >>
    | Program.NativeNode name ->
        <:ctyp<

          $lid:Ident.string_of_lident (Ident.lident_of_uident nm)$ = $lid:Ident.string_of_lident name$

        >>

  and ast_clause (nm, args) =
    let args =
      let rec loop =
        function
        | Constr.Tycon nm ->
            <:ctyp< $lid:Ident.string_of_lident (Ident.lident_of_uident nm)$ >>
        | Constr.List ty ->
            <:ctyp< $loop ty$ list >>
        | Constr.Option ty ->
            <:ctyp< $loop ty$ option >>
      in
      List.map loop args
    in
    <:ctyp< $uid:Ident.string_of_uident nm$ of $Ast.tySta_of_list args$>>

  and rewrite_node typing (nm, cl) =
    <:class_str_item<

        method $lid:Ident.string_of_lident (Ident.lident_of_uident nm)$ =
          function $List.map (rewrite_clause typing) cl |> Ast.mcOr_of_list$

    >>

 and rewrite_clause (st,dt) (l,r) = <:match_case< $tup:paTree st l$ -> $tup:exTree dt r$>>

 and paTree t = function
 | Tree.Tree (_,(nm, lst)) -> <:patt< $uid:Ident.string_of_uident t$ . $uid:Ident.string_of_uident nm$ $List.map (paTree t) lst |> Ast.paCom_of_list$>>
 | Tree.Var (_,nm) -> <:patt< $lid:Ident.string_of_lident nm$ >>
 | Tree.Const (Tree.String str) -> <:patt<$str:str$>>
 | Tree.Const (Tree.Int i) -> <:patt<$int:string_of_int i$>>

 and exTree t = function
 | Tree.Tree (_,(nm, lst)) -> constr t (nm, lst)
 | Tree.Var (_,nm) -> <:expr< $lid:Ident.string_of_lident nm$ >>
 | Tree.Const (Tree.String str) -> <:expr<$str:str$>>
 | Tree.Const (Tree.Int i) -> <:expr<$int:string_of_int i$>>

 and constr t (nm, lst) =
   let e = List.map (exTree t) lst |> Ast.exCom_of_list in
   List.fold_left (fun e1 e2 -> <:expr< $e1$ $e2$ >>) <:expr< $uid:Ident.string_of_uident t$ . $uid:Ident.string_of_uident nm$>>
     (Ast.list_of_expr e [])

end

let output_program file program =
  Emit.program program
  |> OCamlPrinter.print_implem ~output_file:file
