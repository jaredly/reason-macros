

open MacroTypes;
open Parsetree;
open Longident;

open Ast_mapper;

let rec macroMapper = collected =>
  Parsetree.{
    ...Ast_mapper.default_mapper,
    structure: (mapper, str) => {
      let (items, _macros, _apply) = str |> List.fold_left(
        ((items, macros, apply), item) => {
          switch (item.pstr_desc) {
            | Pstr_extension(({txt, loc}, payload), attributes) => {
              switch (SetupMacro.processMacro(txt, payload, attributes)) {
                | None => (apply.structure(apply, [item]) @ items, macros, apply)
                | Some(macro) => (items, [macro, ...macros], Eval.applyMapper([macro, ...macros]))
              }
            }
            | _ => (apply.structure(apply, [item]) @ items, macros, apply)
          }
        },
        ([], collected, Ast_mapper.default_mapper)
      );
      items |> List.rev
    }
  }
  ;

let () = Ast_mapper.run_main(_argv => macroMapper([]));