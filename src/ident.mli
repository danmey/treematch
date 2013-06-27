type 'a t
and uident = [`U] t
and lident = [`L] t
and 'a ident = ([< `U|`L] as 'a) t

val uident : string -> uident
val lident : string -> lident
val ident : string -> 'a ident

val string_of_uident : uident -> string
val string_of_lident : lident -> string

val string_of_ident : lident -> string

val lident_of_uident : uident -> lident
val uident_of_lident : lident -> uident

val pp_uident : Format.formatter -> uident -> unit
val pp_lident : Format.formatter -> lident -> unit
val pp_ident : Format.formatter -> 'a ident -> unit

val uident_of_sexp : Sexplib.Sexp.t -> uident
val lident_of_sexp : Sexplib.Sexp.t -> lident
val sexp_of_uident : uident -> Sexplib.Sexp.t
val sexp_of_lident : lident -> Sexplib.Sexp.t
