%{

%}

/* +=====~~~-------------------------------------------------------~~~=====+ */
/* |                                Tokens                                 | */
/* +=====~~~-------------------------------------------------------~~~=====+ */

%token TOK_LPAREN TOK_RPAREN
%token TOK_LBRACKET TOK_RBRACKET
%token TOK_LBRACE TOK_RBRACE
%token TOK_ARROW
%token TOK_BIARROW
%token TOK_COLONCOLON
%token TOK_DOT
%token TOK_BANG TOK_TILDE
%token TOK_PLUS TOK_MINUS TOK_STAR
%token TOK_EQUALEQUAL
%token TOK_QUESTION
%token TOK_COLON
%token TOK_EQUAL
%token TOK_COMMA
%token TOK_ELLIPSIS
%token TOK_SEMICOLON
%token TOK_BAR

%token TOK_AST TOK_MAP

%token <string> TOK_INT_LITERAL
%token <string> TOK_CHAR_LITERAL
%token <string> TOK_STRING_LITERAL
%token <Ident.uident> TOK_LABEL
%token <Ident.lident> TOK_LIDENT
%token <Ident.uident> TOK_UIDENT

%token TOK_EOF

%start<Program.untyped_program> program
%%

program
: definitions=list(definition) TOK_EOF                          { definitions }

definition
: TOK_AST nm=TOK_UIDENT TOK_LBRACE nds=list(ast_node) TOK_RBRACE
                                                      { Program.Ast (nm, nds) }
| TOK_MAP nm=TOK_LIDENT TOK_COLON ty=type_decl
  TOK_LBRACE nds=list(rewrite_node) TOK_RBRACE
                                                  { Program.Map (nm, ty, nds) }

ast_node
: nm=TOK_LABEL option(TOK_BAR) node=node { nm, node }

rewrite_node
: nm=TOK_LABEL option(TOK_BAR) l=separated_list(TOK_BAR,rewrite_clause) {nm, l}

node
: clauses=separated_list(TOK_BAR,ast_clause)     { Program.CustomNode clauses }
| nm=TOK_LIDENT                                       { Program.NativeNode nm }
| TOK_EQUAL nm=tycon                                   { Program.AliasNode nm }

ast_clause
: t=constr                                                               {  t }

rewrite_clause
: l=topl_tree TOK_BIARROW r=topl_tree                                   { l,r }

constr
: tag=TOK_UIDENT tycons=list(tycon)                             { tag, tycons }

tycon
: nm=TOK_UIDENT                                             { Constr.Tycon nm }
| TOK_LBRACKET t=tycon TOK_RBRACKET                           { Constr.List t }
| TOK_QUESTION t=tycon                                      { Constr.Option t }

type_decl
: s=TOK_UIDENT TOK_BIARROW d=TOK_UIDENT                                 { s,d }

topl_tree
: nm=TOK_UIDENT tree=list(tree)                   { Tree.Tree ((), (nm,tree)) }
| TOK_LPAREN tree=topl_tree TOK_RPAREN                                 { tree }

tree
: TOK_LPAREN nm=TOK_UIDENT tree=list(tree) TOK_RPAREN
                                                  { Tree.Tree ((),(nm, tree)) }
| nm=TOK_UIDENT                                    { Tree.Tree ((), (nm, [])) }
| nm=TOK_LIDENT                                            { Tree.Var ((),nm) }
