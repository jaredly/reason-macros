
let fail = (loc, txt) => raise(Location.Error(Location.error(~loc, txt)));
let fail2 = (loc, txt, loc2, txt2) => raise(Location.Error(Location.error(~loc, ~sub=[
  Location.error(~loc=loc2, txt2)
], txt)));

open Parsetree;
open Longident;

type argType = int;
// TODO maybe support more kinds of patterns
type patIdent = Asttypes.loc(string);

// type argType =
//   | Expr
//   | LongCapIdent
//   | CapIdent
//   | LongIdent
//   // | RecordObject(argType)
//   | Ident;

// type arg =
//   | Single(string, argType)
//   | Tuple(list(arg));

type macro =
  | Let(string, pattern, pattern, pattern, expression)
  | Toplevel(string, list(pattern), structure)
  | Expression(string, list(pattern), expression)



let bindPat = (pattern, pat) => switch pattern {
  | {ppat_desc: Ppat_var({txt})} => [(txt, (pat.ppat_loc, `Pattern(pat)))]
  | {ppat_desc: Ppat_constraint(
    {ppat_desc: Ppat_var({txt: binding_name})},
    {ptyp_desc: Ptyp_constr({txt: Lident(typ), loc: typloc}, [])}
  )} => switch typ {
    | "pattern" => [(binding_name, (pat.ppat_loc, `Pattern(pat)))]
    | "longCapIdent" => switch pat.ppat_desc {
      | Ppat_construct({txt}, None) => [(binding_name, (pat.ppat_loc, `LongCapIdent(txt)))]
      | _ => fail(pat.ppat_loc, "Argument must be a (possibly namespaced) capitalized Identifier")
    }
    | "capIdent" => switch pat.ppat_desc {
      | Ppat_construct({txt: Lident(name)}, None) => [(binding_name, (pat.ppat_loc, `CapIdent(name)))]
      | _ => fail(pat.ppat_loc, "Argument must be a non-namespaced capitalized Identifier")
    }
    | "ident" => switch pat.ppat_desc {
      | Ppat_var({txt: name}) => [(binding_name, (pat.ppat_loc, `Ident(name)))]
      | _ => fail(pat.ppat_loc, "Argument must be a non-namespaced capitalized Identifier")
    }
    // TODO shouldn't wait until here to error -- this should be prevalidated
    | _ => fail(typloc, "Unsupported type constraint")
  }
  // TODO shouldn't wait until here to error -- this should be prevalidated
  | _ => fail(pattern.ppat_loc, "Unsupported pattern type")
};

let bindLocal = (pattern, expr) => switch pattern {
  | {ppat_desc: Ppat_var({txt})} => [(txt, (expr.pexp_loc, `Expr(expr)))]
  | {ppat_desc: Ppat_constraint(
    {ppat_desc: Ppat_var({txt: binding_name})},
    {ptyp_desc: Ptyp_constr({txt: Lident(typ), loc: typloc}, [])}
  )} => switch typ {
    | "expression" => [(binding_name, (expr.pexp_loc, `Expr(expr)))]
    | "pattern" => switch expr.pexp_desc {
      | Pexp_extension(({txt: "pat"}, PPat(pattern, _))) => [(binding_name, (expr.pexp_loc, `Pattern(pattern)))]
      | _ => fail2(expr.pexp_loc, "Argument must be a pattern (pass in with [%pat? SomePattern])",
      typloc, "Declared here")
    }
    | "longCapIdent" => switch expr.pexp_desc {
      | Pexp_construct({txt}, None) => [(binding_name, (expr.pexp_loc, `LongCapIdent(txt)))]
      | _ => fail2(expr.pexp_loc, "Argument must be a (possibly namespaced) capitalized Identifier",
      typloc, "Declared here")
    }
    | "capIdent" => switch expr.pexp_desc {
      | Pexp_construct({txt: Lident(name)}, None) => [(binding_name, (expr.pexp_loc, `CapIdent(name)))]
      | _ => fail2(expr.pexp_loc, "Argument must be a non-namespaced capitalized Identifier",
      typloc, "Declared here")
    }
    | "longIdent" => switch expr.pexp_desc {
      | Pexp_ident({txt}) => [(binding_name, (expr.pexp_loc, `LongIdent(txt)))]
      | _ => fail2(expr.pexp_loc, "Argument must be a (possibly namespaced) identifier",
      typloc, "Declared here"
      )
    }
    | "ident" => switch expr.pexp_desc {
      | Pexp_ident({txt: Lident(name)}) => [(binding_name, (expr.pexp_loc, `Ident(name)))]
      | _ => fail2(expr.pexp_loc, "Argument must be a non-namespaced capitalized Identifier",
      typloc, "Declared here")
    }
    // TODO shouldn't wait until here to error -- this should be prevalidated
    | _ => fail(typloc, "Unsupported type constraint")
  }
  // TODO shouldn't wait until here to error -- this should be prevalidated
  | _ => fail(pattern.ppat_loc, "Unsupported pattern type")
};

let setupLocals = (args, payload, loc) => {
  switch (args, payload) {
    | ([pattern], PStr([{pstr_desc: Pstr_eval(expr, _)}])) => bindLocal(pattern, expr)
    | (args, PStr([{pstr_desc: Pstr_eval({pexp_desc: Pexp_tuple(items), pexp_loc}, _)}])) => {
      let rec loop = (args, items) => switch (args, items) {
        | ([arg, ...args], [item, ...items]) => bindLocal(arg, item) @ loop(args, items)
        | ([], []) => []
        | _ => assert(false)
      };
      if (List.length(args) != List.length(items)) {
        fail(pexp_loc, Printf.sprintf("Expected %d arguments, found %d", List.length(args), List.length(items)))
      };
      loop(args, items)
    }
    | _ => fail(loc, "Invalid payload for macro definition")
  }
};

let prefix = "eval__";
let pl = String.length(prefix);

let checkEvalPrefix = txt => {
  let tl = String.length(txt);
  if (tl <= pl) {
    None
  } else if (String.sub(txt, 0, pl)|>String.lowercase == prefix) {
    Some(String.sub(txt, pl, tl - pl))
  } else {
    None
  }
};

let strrx = Str.regexp("\\$eval\\{\\([^}]+\\)\\}");

let regexpReplace = (rx, string, fn) => {
  let rec loop = string => {
    switch (Str.search_forward(rx, string, 0)) {
      | exception Not_found => string
      | idx =>
        let tpl = fn(string);
        loop(Str.replace_first(rx, tpl, string))
    }
  };
  loop(string)
};

let evalString = (locals, string, loc) => {
  regexpReplace(strrx, string, (current) => {
    let text = Str.matched_group(1, current);
    switch (List.assoc(text, locals) |> snd) {
      | exception Not_found => fail(loc, "Unable to interpolate " ++ text ++ " in string literal")
      | `Ident(name) => name
      | `CapIdent(name) => name
      | `LongIdent(lident) | `LongCapIdent(lident) => String.concat(".", Longident.flatten(lident))
      | _ => fail(loc, "String interpolation only currently supported for ident and capIdent types")
    }
  })
}

let isCapitalized = txt => txt != "" && String.lowercase(String.sub(txt, 0, 1)) != String.sub(txt, 0, 1);

let evalCapIdent = (locals, txt, loc) => switch (checkEvalPrefix(txt)) {
  | None => txt
  | Some(vbl) => switch (List.assoc(vbl, locals) |> snd) {
    | exception Not_found => fail(loc, "No matching macro variable for " ++ vbl)
    | `CapIdent(name) => name
    | _ => fail(loc, "Macro variable " ++ vbl ++ " expected to be of type capIdent")
  }
};

let evalIdent = (locals, txt, loc) => switch (checkEvalPrefix(txt)) {
  | None => txt
  | Some(vbl) => switch (List.assoc(vbl, locals) |> snd) {
    | exception Not_found => fail(loc, "No matching macro variable for " ++ vbl)
    | `Ident(name) => name
    | _ => fail(loc, "Macro variable " ++ vbl ++ " expected to be of type ident")
  }
}

let evalIdentTxt = (locals, txt, loc) => {
  switch (checkEvalPrefix(txt)) {
    | None => None
    | Some(vbl) => Some(switch (List.assoc(vbl, locals) |> snd) {
      | exception Not_found => fail(loc, "No matching local for eval variable " ++ vbl)
      | `Ident(name) => isCapitalized(txt) ? fail(loc, "Variable " ++ vbl ++ " must be a capIdent or longCapIdent") : Lident(name)
      | `LongIdent(lident) => isCapitalized(txt) ? fail(loc, "Variable " ++ vbl ++ " must be a capIdent or longCapIdent") : lident
      | `CapIdent(name) => isCapitalized(txt) ? Lident(name) : fail(loc, "Variable " ++ vbl ++ " must be an ident or longIdent")
      | `LongCapIdent(lident) => isCapitalized(txt) ? lident : fail(loc, "Variable " ++ vbl ++ " must be an ident or longIdent")
      | _ => fail(loc, "Variable " ++ vbl ++ " expected to be an ident or longIdent")
    })
  }
};

let rec joinLidents = (one, two, loc) => switch two {
  | Lident(two) => Ldot(one, two)
  | Ldot(two, three) => Ldot(joinLidents(one, two, loc), three)
  | _ => fail(loc, "Cannot join a lident thats an apply")
};

let rec evalLongIdent = (locals, lident, loc) => {
  switch lident {
    | Lident(name) => switch (evalIdentTxt(locals, name, loc)) {
      | None => Lident(name)
      | Some(lident) => lident
    }
    | Ldot(one, two) => 
      let one = evalLongIdent(locals, one, loc);
      switch (evalIdentTxt(locals, two, loc)) {
        | None => Ldot(one, two)
        | Some(two) => joinLidents(one, two, loc)
      }
    | Lapply(one, two) => Lapply(
      evalLongIdent(locals, one, loc),
      evalLongIdent(locals, two, loc),
    )
  }
};

let rec evalMapper = locals => {
  ...Ast_mapper.default_mapper,
  structure_item: (mapper, item) => {
    let item = switch (item.pstr_desc) {
      | Pstr_primitive({pval_name, pval_prim} as prim) => {
        {...item, pstr_desc: Pstr_primitive({
          ...prim,
          pval_name: {...pval_name, txt: evalIdent(locals, pval_name.txt, pval_name.loc)},
          pval_prim: List.map(prim => evalString(locals, prim, item.pstr_loc), pval_prim)
        })}
      }
      | _ => item
    };
    Ast_mapper.default_mapper.structure_item(mapper, item)
  },
  module_binding: (mapper, mb) => {
    let ident = evalCapIdent(locals, mb.pmb_name.txt, mb.pmb_name.loc);
    let mb = if (ident != mb.pmb_name.txt) {
      {...mb, pmb_name: Location.mkloc(ident, mb.pmb_name.loc)}
    } else {
      mb
    };
    Ast_mapper.default_mapper.module_binding(mapper, mb)
  },
  expr: (mapper, expr) => {
    switch expr.pexp_desc {
      | Pexp_ident({txt: Lident(vbl)}) => switch (checkEvalPrefix(vbl)) {
        | None => expr
        | Some(vbl) => switch (List.assoc(vbl, locals)) {
          | exception Not_found => fail(expr.pexp_loc, "Variable " ++ vbl ++ " not found")
          | (loc, `Ident(name)) => {...expr, pexp_desc: Pexp_ident({txt: Lident(name), loc})}
          | (loc, `Expr(expr)) => expr
          | _ => fail(expr.pexp_loc, "Variable " ++ vbl ++ " expected to be an ident or expression")
        }
      }
      | Pexp_ident(ident) =>
        let newIdent = evalLongIdent(locals, ident.txt, ident.loc);
        if (ident.txt != newIdent) {
          {...expr, pexp_desc: Pexp_ident(Location.mkloc(newIdent, ident.loc))}
        } else {
          expr
        }
      | Pexp_constant(Const_string(string, fence)) => {
        let newString = evalString(locals, string, expr.pexp_loc);
        if (newString != string) {
          {...expr, pexp_desc: Pexp_constant(Const_string(newString, fence))}
        } else {
          expr
        }
      }
      | _ => Ast_mapper.default_mapper.expr(mapper, expr)
    }
  },
  pat: (mapper, pat) => {
    switch pat.ppat_desc {
      | Ppat_var({txt, loc}) => {
        switch (checkEvalPrefix(txt)) {
          | None => pat
          | Some(vbl) => switch (List.assoc(vbl, locals)) {
            | exception Not_found => fail(pat.ppat_loc, "Variable " ++ vbl ++ " not found")
            | (_loc, `Ident(name)) => {...pat, ppat_desc: Ppat_var({txt: name, loc})}
            | (_loc, `Pattern(pat)) => pat
            | _ => fail(pat.ppat_loc, "Variable " ++ vbl ++ " expected to be a pattern")
          }
        }
      }
      | Ppat_construct(ident, args) => {
        let newIdent = evalLongIdent(locals, ident.txt, ident.loc);
        let pat = if (ident.txt != newIdent) {
          {...pat, ppat_desc: Ppat_construct(Location.mkloc(newIdent, ident.loc), args)}
        } else {
          pat
        };
        Ast_mapper.default_mapper.pat(mapper, pat)
      }
      | Ppat_constant(Const_string(string, fence)) => {
        let newString = evalString(locals, string, pat.ppat_loc);
        if (newString != string) {
          {...pat, ppat_desc: Ppat_constant(Const_string(newString, fence))}
        } else {
          pat
        }
      }
      | _ => Ast_mapper.default_mapper.pat(mapper, pat)
    }
  }
};

let rec evalExpr = (locals, body) => {
  let mapper = evalMapper(locals);
  mapper.expr(mapper, body)
};

let rec evalStr = (locals, body) => {
  let mapper = evalMapper(locals);
  mapper.structure(mapper, body)
};

let rec getStringAttribute = (attributes, name) => switch attributes {
  | [] => None
  | [({Location.txt}, PStr([{pstr_desc: Pstr_eval({pexp_desc: Pexp_constant(Const_string(v, _))}, _)}])), ..._] when txt == name => Some(v)
  | [_, ...rest] => getStringAttribute(rest, name)
};

let rec collectArgs = expr => {
  switch expr.pexp_desc {
    | Pexp_fun("", None, pattern, body) => {
      let (inner, body) = collectArgs(body);
      ([pattern, ...inner], body)
    }
    | Pexp_fun(_, _, _, _) => fail(expr.pexp_loc, "Macro definition functions can't have default values or labels")
    | _ => ([], expr)
  }
};

let getStr = expr => switch expr.pexp_desc {
  | Pexp_extension(({txt: "str"}, PStr(str))) => str
  | _ => fail(expr.pexp_loc, "toplevel macro contents must be a [%str ]")
};

let getName = (pattern, attributes) => {
  switch pattern.ppat_desc {
    | Ppat_any => switch (getStringAttribute(attributes, "macro.name")) {
      | None => fail(pattern.ppat_loc, "macro.name must be provided when the let pattern is _")
      | Some(name) => name
    }
    | Ppat_var({txt}) => txt
    | _ => fail(pattern.ppat_loc, "macro.let pattern must either be a var or _ with macro.name supplied")
  };
}

let processMacro = (txt, payload, attributes) => {
  open Parsetree;
  switch (txt, payload) {
    | ("macro", PStr([{pstr_desc: Pstr_value(Nonrecursive, [
      {
        pvb_pat,
        pvb_expr,
        pvb_attributes,
        pvb_loc
      }
    ])}])) => {
      let (args, body) = collectArgs(pvb_expr);
      let name = getName(pvb_pat, pvb_attributes);
      Some(Expression(name, args, body))
    }
    | ("macro.toplevel", PStr([{pstr_desc: Pstr_value(Nonrecursive, [
      {
        pvb_pat,
        pvb_expr,
        pvb_attributes,
        pvb_loc
      }
    ])}])) => {
      let (args, body) = collectArgs(pvb_expr);
      let name = getName(pvb_pat, pvb_attributes);
      let str = getStr(body);
      Some(Toplevel(name, args, str))
    }
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
      let name = getName(pvb_pat, pvb_attributes);
      Some(Let(name, patternName, valueName, continueName, body))
    }
    | _ => None
  }
}

let rec findLet = (txt, macros) => switch macros {
  | [] => None
  | [Let(a, b, c, d, e), ..._] when a == txt => Some((b, c, d, e))
  | [_, ...rest] => findLet(txt, rest)
};

let rec findExpression = (txt, macros) => switch macros {
  | [] => None
  | [Expression(n, args, str), ..._] when n == txt => Some((args, str))
  | [_, ...rest] => findExpression(txt, rest)
};

let rec findToplevel = (txt, macros) => switch macros {
  | [] => None
  | [Toplevel(n, args, str), ..._] when n == txt => Some((args, str))
  | [_, ...rest] => findToplevel(txt, rest)
};

let applyStrMacros = (macros, stri) => switch stri.pstr_desc {
  | Pstr_extension(({txt, loc}, payload), _attributes) => {
    let found = findToplevel(txt, macros);
    switch found {
      | None => None
      | Some((args, body)) => {
        let locals = setupLocals(args, payload, stri.pstr_loc);
        Some(evalStr(locals, body))
      }
    }
  }
  | _ => None
};

let applyExpMacros = (macros, exp) => switch exp.pexp_desc {
  | Pexp_extension(({txt, loc}, payload)) => {
    switch (findExpression(txt, macros)) {
      | Some((args, body)) => {
        let locals = setupLocals(args, payload, exp.pexp_loc);
        evalExpr(locals, body)
      }
      | None => switch payload {
        | PStr([{pstr_desc: Pstr_eval({pexp_desc: Pexp_let(Nonrecursive, [{pvb_pat, pvb_expr}], body)}, _)}]) =>
          switch (findLet(txt, macros)) {
            | None => exp
            | Some((ppat, pval, pbody, macro)) => {
              let locals = bindPat(ppat, pvb_pat) @ bindLocal(pval, pvb_expr) @ bindLocal(pbody, body);
              evalExpr(locals, macro)
            }
          }
        | _ => exp
      }
    }
  }
  | _ => exp
};

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

open Ast_mapper;

let rec macroMapper = collected =>
  Parsetree.{
    ...Ast_mapper.default_mapper,
    structure: (mapper, str) => {
      let (items, _macros, _apply) = str |> List.fold_left(
        ((items, macros, apply), item) => {
          switch (item.pstr_desc) {
            | Pstr_extension(({txt, loc}, payload), attributes) => {
              switch (processMacro(txt, payload, attributes)) {
                | None => (apply.structure(apply, [item]) @ items, macros, apply)
                | Some(macro) => (items, [macro, ...macros], applyMapper([macro, ...macros]))
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