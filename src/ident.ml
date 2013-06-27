open Sexplib.Conv

type 'a t = string
and uident = [`U] t
and lident = [`L] t
and 'a ident = ([< `U|`L] as 'a) t

let uident  : string -> uident = fun name ->
  if BatChar.is_uppercase name.[0] then
    name
  else
    String.capitalize name

let lident : string -> lident = fun name ->
  if BatChar.is_lowercase name.[0] || name.[0] = '_' then
    name
  else
    String.uncapitalize name

let ident : string -> 'a ident = BatPervasives.identity

let uident_of_sexp (Sexplib.Type.Atom name) = name
let lident_of_sexp (Sexplib.Type.Atom name) = name
let sexp_of_uident (name : uident) = Sexplib.Type.Atom name
let sexp_of_lident (name : lident) = Sexplib.Type.Atom name

let string_of_lident : lident -> string = BatPervasives.identity
let string_of_uident : uident -> string = BatPervasives.identity

let string_of_ident : 'a ident -> string = BatPervasives.identity

let uident_of_lident (ident : lident) = uident ident
let lident_of_uident (ident : uident) = lident ident

let pp_uident pp ident = Format.pp_print_string pp (string_of_uident ident)
let pp_lident pp ident = Format.pp_print_string pp (string_of_lident ident)
let pp_ident pp ident = Format.pp_print_string pp (string_of_ident ident)
