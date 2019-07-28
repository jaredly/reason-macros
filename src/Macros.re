open Migrate_parsetree;
open OCaml_407.Ast;

open Parsetree;

open Ast_mapper;

open Location;

// Collect macros for later application
let process = (str, collected) => {
  let (items, macros, _apply) =
    str
    |> List.fold_left(
         ((items, macros, apply), item) => {
           switch (item.pstr_desc) {
           | Pstr_extension(({txt, loc: _}, payload), attributes) =>
             switch (SetupMacro.processMacro(txt, payload, attributes)) {
             | None => (
                 apply.structure(apply, [item]) @ items,
                 macros,
                 apply,
               )
             | Some(macro) => (
                 items,
                 [macro, ...macros],
                 Eval.applyMapper([macro, ...macros]),
               )
             }
           | _ => (apply.structure(apply, [item]) @ items, macros, apply)
           }
         },
         (
           [],
           collected,
           collected == []
             ? Ast_mapper.default_mapper : Eval.applyMapper(collected),
         ),
       );
  let items = items |> List.rev;
  (items, macros);
};

let collect = ast => process(ast, []) |> snd;

let macroMapper = collected => {
  ...Ast_mapper.default_mapper,
  structure: (_mapper, str) => {
    let (items, _macros) = process(str, collected);
    items;
  },
};