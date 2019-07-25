
let fail = (loc, txt) => raise(Location.Error(Location.error(~loc, txt)));

open Parsetree;
open Longident;

type argType = int;
// TODO maybe support more kinds of patterns
type patIdent = Asttypes.loc(string);

type macro =
  | Let(pattern, pattern, pattern, expression)

let rec getStringAttribute = (attributes, name) => switch attributes {
  | [] => None
  | [({Location.txt}, PStr([{pstr_desc: Pstr_eval({pexp_desc: Pexp_constant(Const_string(v, _))}, _)}])), ..._] when txt == name => Some(v)
  | [_, ...rest] => getStringAttribute(rest, name)
};

// let collected = ref([]);

let processMacro = (txt, payload, attributes) => {
  open Parsetree;
  switch (txt, payload) {
    | ("macro.let", PStr([{pstr_desc: Pstr_value(Nonrecursive, [
      {
        pvb_pat,
        pvb_expr: {
          pexp_desc: Pexp_fun("", None, patternName, {
            pexp_desc: Pexp_fun("", None, valueName, {
              pexp_desc: Pexp_fun("", None, continueName, body)
            })
          })
        },
        pvb_attributes,
        pvb_loc
      }
    ])}])) => {
      let name = switch pvb_pat.ppat_desc {
        | Ppat_any => switch (getStringAttribute(pvb_attributes, "macro.name")) {
          | None => failwith("macro.name must be provided when the let pattern is _")
          | Some(name) => name
        }
        | Ppat_var({txt}) => txt
        | _ => failwith("macro.let pattern must either be a var or _ with macro.name supplied")
      };
      Some(Let(patternName, valueName, continueName, body))
    }
    | _ => None
  }
}

let applyStrMacros = (macros, stri) => None;
let applyExpMacros = (macros, exp) => exp;

let applyMapper = macros => Parsetree.{
  ...Ast_mapper.default_mapper,
  structure: (mapper, str) => List.fold_left(
    (result, item) => {
      switch (applyStrMacros(macros, item)) {
        | None => [Ast_mapper.default_mapper.structure_item(mapper, item), ...result]
        | Some(items) => List.rev(mapper.structure(mapper, items)) @ result
      }
    },
    [],
    str
  ) |> List.rev,
  expr: (mapper, expr) => Ast_mapper.default_mapper.expr(mapper, applyExpMacros(macros, expr))
};

let rec macroMapper = collected =>
  Parsetree.{
    ...Ast_mapper.default_mapper,
    structure: (mapper, str) => {
      let (items, _macros) = str |> List.fold_left(
        ((items, macros), item) => {
          switch (item.pstr_desc) {
            | Pstr_extension(({txt, loc}, payload), attributes) => {
              switch (processMacro(txt, payload, attributes)) {
                | None => ([Ast_mapper.default_mapper.structure_item(mapper, item), ...items], macros)
                | Some(macro) => (items, [macro, ...macros])
              }
            }
            | _ => ([Ast_mapper.default_mapper.structure_item(mapper, item), ...items], macros)
          }
        },
        ([], collected)
      );
      items |> List.rev
    }
  }
  ;

let () = Ast_mapper.run_main(_argv => macroMapper([]));