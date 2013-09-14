open Ocamlbuild_plugin;;

flag ["ocaml"; "compile"; "lib_baselib"] (S [A"-I"; A"../../../_build/src/baselib/core"]);;
flag ["ocaml"; "link"; "lib_baselib"; "byte"] (A"../../../_build/src/baselib/core/corelib.cma");;
flag ["ocaml"; "link"; "lib_baselib"; "native"] (A"../../../_build/src/baselib/core/corelib.cmxa");;
