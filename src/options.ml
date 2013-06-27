module Priv = struct
  let _i = ref false
  let _infer = ref false
  let _no_emit = ref false
  let _special = ref false
  let _dump_ast = ref false
  let _dump_tokens = ref false
  let _show_tycon_freq = ref false
end


let () =
  Cmdline.register "treematch" Arg.([
    "-i",                       Set Priv._i,		   " generate interface files";
    "-infer",                   Set Priv._infer,           " infer the ast from map strategies";
    "-no-emit",                 Set Priv._i,		   " Don't generate code";
    "-special",                 Set Priv._special,         " Use special backend";
    "-dump-ast",		Set Priv._dump_ast,	   " dump source ast";
    "-dump-tokens",		Set Priv._dump_tokens,	   " dump source tokens";
    "-show-tycon-freq",         Set Priv._show_tycon_freq, " Show type constructor usage frequency"
  ])


let _i () = !Priv._i
let _infer () = !Priv._infer
let _no_emit () = !Priv._no_emit
let _special () = !Priv._special
let _dump_ast () = !Priv._dump_ast
let _dump_tokens () = !Priv._dump_tokens
let _show_tycon_freq () = !Priv._show_tycon_freq
