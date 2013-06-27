module P = Program
module T = Tree
let (|>) = BatPervasives.(|>)

module Count = Node.Loc.Map
module Clause = Constr

let definition = function
| P.Ast (name, nodes) ->

    let visit_node (node_name, node) =

      let visit_clause clause =
        let loc = Node.Loc.create node_name clause in
        loc, clause
      in

      match node with
      | P.CustomNode clauses -> List.map visit_clause clauses
      | _ -> []
    in

    let clauses = List.map visit_node nodes |> List.concat in

    let counter = Hashtbl.create 123 in

    let count (loc, clause) =
      let tycon = Clause.tycon clause in
      let iter name =
        try

          let c = Hashtbl.find counter name in
          Hashtbl.remove counter name;
          Hashtbl.add counter name (c+1)

        with Not_found -> Hashtbl.add counter name 1

      in
      List.iter (Clause.iter_tycon iter) tycon
    in

    List.iter count clauses;

    let print_count name count =
      Printf.printf "%s: %d\n" (Ident.string_of_uident name) count
    in

    Hashtbl.iter print_count counter
| _ -> ()

let program p = List.iter definition p
