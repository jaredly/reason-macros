
open Migrate_parsetree;
open OCaml_407.Ast;

let () = Driver.register(~name="macros",~args=[], Versions.ocaml_407, (_config, _cookies) => Macros.macroMapper([]));
