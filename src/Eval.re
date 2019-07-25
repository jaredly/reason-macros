
open MacroTypes;
open Parsetree;
open Longident;

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
      | exception Not_found => fail(loc, "Interpolation variable " ++ text ++ " not found")
      | `Ident(name) => name
      | `CapIdent(name) => name
      | `LongIdent(lident) | `LongCapIdent(lident) => String.concat(".", Longident.flatten(lident))
      | _ => fail(loc, "String interpolation only currently supported for ident and capIdent MacroTypes")
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
        let locals = Args.setupLocals(args, payload, stri.pstr_loc);
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
        let locals = Args.setupLocals(args, payload, exp.pexp_loc);
        evalExpr(locals, body)
      }
      | None => switch payload {
        | PStr([{pstr_desc: Pstr_eval({pexp_desc: Pexp_let(Nonrecursive, [{pvb_pat, pvb_expr}], body)}, _)}]) =>
          switch (findLet(txt, macros)) {
            | None => exp
            | Some((ppat, pval, pbody, macro)) => {
              let locals = Args.bindPat(ppat, pvb_pat) @ Args.bindLocal(pval, pvb_expr) @ Args.bindLocal(pbody, body);
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
