(*+ -auto-loc
 *)
{
  open Parser
  let line = ref 1
}

let lower    = ['a'-'z']
let upper    = ['A'-'Z']

let digit    = ['0'-'9']

let alpha = (lower | upper)
let alnum = (alpha | digit)

let lident = (lower | '_')(alnum | '_')*
let uident = upper (alnum | '_')*

let bstring = '`'  ('\\' _ | [^ '\\' '`' ])* '`'
let dstring = '"'  ('\\' _ | [^ '\\' '"' ])* '"'
let sstring = '\'' ('\\' _ | [^ '\\' '\''])* '\''

let d = digit
let ws = [' ' '\t' '\r']

rule token = parse
(* whitespace *)
| '\n'                                                          { line := !line + 1; token lexbuf }
| [' ' '\t' '\r']+                                              { token lexbuf }

(* keywords, operators *)
| "("                                                           { TOK_LPAREN }
| ")"                                                           { TOK_RPAREN }
| "["                                                           { TOK_LBRACKET }
| "]"                                                           { TOK_RBRACKET }
| "{" | "<%"                                                    { TOK_LBRACE }
| "}" | "%>"                                                    { TOK_RBRACE }
| "->"                                                          { TOK_ARROW }
| "=>"                                                          { TOK_BIARROW }
| "::"                                                          { TOK_COLONCOLON }
| "."                                                           { TOK_DOT }
| "!"                                                           { TOK_BANG }
| "~"                                                           { TOK_TILDE }
| "+"                                                           { TOK_PLUS }
| "-"                                                           { TOK_MINUS }
| "*"                                                           { TOK_STAR }
| "=="                                                          { TOK_EQUALEQUAL }
| "?"                                                           { TOK_QUESTION }
| ":"                                                           { TOK_COLON }
| "="                                                           { TOK_EQUAL }
| ","                                                           { TOK_COMMA }
| "..."                                                         { TOK_ELLIPSIS }
| "_"                                                           { TOK_ELLIPSIS }
| ";"                                                           { TOK_SEMICOLON }
| "|"                                                           { TOK_BAR }

(* C++ comments *)
| "//" [^ '\n']*                                                { token lexbuf }

(* C comments *)
| "/*" ([^ '*']| "*"* [^ '*' '/'])* "*"+ "/"                    { token lexbuf }
| "/*" ([^ '*']| "*"* [^ '*' '/'])* "*"*                        { failwith "unterminated comment" }

(* identifier *)
| "ast"                                                         { TOK_AST }
| "map"                                                         { TOK_MAP }
| uident as nm ":"                                              { TOK_LABEL (Ident.uident nm) }
| uident                                                        { TOK_UIDENT (Ident.uident (Lexing.lexeme lexbuf)) }
| lident                                                        { TOK_LIDENT (Ident.lident (Lexing.lexeme lexbuf)) }

| d+                                                            { TOK_INT_LITERAL (Lexing.lexeme lexbuf) }

(* strings *)
| 'L'?sstring                                                   { TOK_CHAR_LITERAL (Lexing.lexeme lexbuf) }
| 'L'?dstring                                                   { TOK_STRING_LITERAL (Lexing.lexeme lexbuf) }

| eof                                                           { TOK_EOF }

| _ as c                                                        { failwith (Char.escaped c) }

(* EOF *)
