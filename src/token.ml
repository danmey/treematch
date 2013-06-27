open Parser

(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                      Printable version of token                       | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

let to_string = function
| TOK_LPAREN -> "TOK_LPAREN"
| TOK_RPAREN -> "TOK_RPAREN"
| TOK_LBRACKET -> "TOK_LBRACKET"
| TOK_RBRACKET -> "TOK_RBRACKET"
| TOK_LBRACE -> "TOK_LBRACE"
| TOK_RBRACE -> "TOK_RBRACE"
| TOK_ARROW -> "TOK_ARROW"
| TOK_BIARROW -> "TOK_BIARROW"
| TOK_COLONCOLON -> "TOK_COLONCOLON"
| TOK_DOT -> "TOK_DOT"
| TOK_BANG -> "TOK_BANG"
| TOK_TILDE -> "TOK_TILDE"
| TOK_PLUS -> "TOK_PLUS"
| TOK_MINUS -> "TOK_MINUS"
| TOK_STAR -> "TOK_STAR"
| TOK_EQUALEQUAL -> "TOK_EQUALEQUAL"
| TOK_QUESTION -> "TOK_QUESTION"
| TOK_COLON -> "TOK_COLON"
| TOK_EQUAL -> "TOK_EQUAL"
| TOK_COMMA -> "TOK_COMMA"
| TOK_ELLIPSIS -> "TOK_ELLIPSIS"
| TOK_SEMICOLON -> "TOK_SEMICOLON"
| TOK_BAR -> "TOK_BAR"
| TOK_AST -> "TOK_AST"
| TOK_MAP -> "TOK_MAP"
| TOK_INT_LITERAL s -> Printf.sprintf "TOK_INT_LITERAL(%s)" s
| TOK_CHAR_LITERAL s -> Printf.sprintf "TOK_CHAR_LITERAL(%s)" s
| TOK_STRING_LITERAL s -> Printf.sprintf "TOK_STRING_LITERAL(%s)" s
| TOK_LIDENT s -> Printf.sprintf "TOK_LIDENT(%s)" (Ident.string_of_lident s)
| TOK_UIDENT s -> Printf.sprintf "TOK_UIDENT(%s)" (Ident.string_of_uident s)
| TOK_LABEL s -> Printf.sprintf "TOK_LABEL(%s)" (Ident.string_of_uident s)
| TOK_EOF -> "TOK_EOF"
