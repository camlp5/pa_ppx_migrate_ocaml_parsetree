#include "version.inc"

let top = try Sys.getenv "TOP" with Not_found -> "" ;;
let destdir = ref (top ^ "/local-install") ;;

Arg.(parse [
         "-destdir", (Arg.String (fun s -> destdir := s)),
         ("<path>    Set the destination directory (default: " ^
	    !destdir ^ ")")
       ]
       (fun _ -> failwith "mk_meta: no anonymous args supported")
     "mk_meta -destdir <dir>")
;;

Fmt.(pf stdout {|# Specifications for the "pa_migrate_ocaml_parsetree" library:
requires = "fmt,bos"
version = "%s"
description = "pa_migrate_ocaml_parsetree support"

# For linking
package "link" (
requires = "fmt,bos"
archive(byte) = "pa_ppx_ocaml_migrate_parsetree.cma"
archive(native) = "pa_ppx_migrate_ocaml_parsetree.cmxa"
)

# For the toploop:
archive(byte,toploop) = "pa_ppx_migrate_ocaml_parsetree.cma"

  # For the preprocessor itself:
  requires(syntax,preprocessor) = "fmt,bos"
  archive(syntax,preprocessor,-native) = "pa_ppx_migrate_ocaml_parsetree.cma"
  archive(syntax,preprocessor,native) = "pa_ppx_migrate_ocaml_parsetree.cmxa"
|} version)
