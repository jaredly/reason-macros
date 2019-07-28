
open Migrate_parsetree;
open OCaml_407.Ast;

let loc = Location.none;
let macros = Macros.collect([%str
  let%macro shared = (name: string) => eval__name ++ " yeah";
]);

let () = Driver.register(~name="shared", ~args=[], Versions.ocaml_407, (_config, _cookies) => Macros.macroMapper(macros));

