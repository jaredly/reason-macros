open Migrate_parsetree;
open OCaml_407.Ast;

open MacroTypes;
open Parsetree;
open Longident;

let showStr = str => {
  module BackConverter =
    Migrate_parsetree.Convert(
      Migrate_parsetree.OCaml_407,
      Migrate_parsetree.OCaml_404,
    );
  let oldAst = BackConverter.copy_structure(str);
  Reason_toolchain.Reason_syntax.format_implementation_with_comments(
    (oldAst, []),
    Format.str_formatter,
  );
  Format.flush_str_formatter();
};

let showExp2 = exp => {
  Printast.expression(0, Format.str_formatter, exp);
  Format.flush_str_formatter();
};

let showExp3 = exp => {
  Pprintast.expression(Format.str_formatter, exp);
  Format.flush_str_formatter();
};

let showExp = exp => {
  let str = [Ast_helper.Str.eval(exp)];
  module BackConverter =
    Migrate_parsetree.Convert(
      Migrate_parsetree.OCaml_407,
      Migrate_parsetree.OCaml_404,
    );
  let oldAst = BackConverter.copy_structure(str);
  Reason_toolchain.Reason_syntax.format_implementation_with_comments(
    (oldAst, []),
    Format.str_formatter,
  );
  Format.flush_str_formatter();
};

let prefix = "eval__";
let pl = String.length(prefix);

let checkEvalPrefix = txt => {
  let tl = String.length(txt);
  if (tl <= pl) {
    None;
  } else if (String.sub(txt, 0, pl) |> String.lowercase_ascii == prefix) {
    Some(String.sub(txt, pl, tl - pl));
  } else {
    None;
  };
};

let evalString = (locals: locals, string, loc) => {
  Platform.interpolateString(string, text => {
    switch (List.assoc(text, locals) |> snd) {
    | exception Not_found =>
      fail(loc, "Interpolation variable " ++ text ++ " not found")
    | `Ident(name) => name
    | `CapIdent(name) => name
    | `LongIdent(lident)
    | `LongCapIdent(lident) => String.concat(".", Longident.flatten(lident))
    | `BoolConst(true) => "true"
    | `BoolConst(false) => "false"
    | `IntConst(int) => string_of_int(int)
    | `StringConst(string) => string
    | _ =>
      fail(
        loc,
        "String interpolation only currently supported for ident and capIdent MacroTypes",
      )
    }
  });
};

let isCapitalized = txt =>
  txt != ""
  && String.lowercase_ascii(String.sub(txt, 0, 1)) != String.sub(txt, 0, 1);

let evalCapIdent = (locals, txt, loc) =>
  switch (checkEvalPrefix(txt)) {
  | None => txt
  | Some(vbl) =>
    switch (List.assoc(vbl, locals) |> snd) {
    | exception Not_found =>
      fail(loc, "No matching macro variable for " ++ vbl)
    | `CapIdent(name) => name
    | _ =>
      fail(
        loc,
        "Macro variable " ++ vbl ++ " expected to be of type capIdent",
      )
    }
  };

let evalIdent = (locals, txt, loc) =>
  switch (checkEvalPrefix(txt)) {
  | None => txt
  | Some(vbl) =>
    switch (List.assoc(vbl, locals) |> snd) {
    | exception Not_found =>
      fail(loc, "No matching macro variable for " ++ vbl)
    | `Ident(name) => name
    | _ =>
      fail(loc, "Macro variable " ++ vbl ++ " expected to be of type ident")
    }
  };

let evalIdentTxt = (locals, txt, loc) => {
  switch (checkEvalPrefix(txt)) {
  | None => None
  | Some(vbl) =>
    Some(
      switch (List.assoc(vbl, locals) |> snd) {
      | exception Not_found =>
        fail(loc, "No matching local for eval variable " ++ vbl)
      | `Ident(name) =>
        isCapitalized(txt)
          ? fail(
              loc,
              "Variable " ++ vbl ++ " must be a capIdent or longCapIdent",
            )
          : Lident(name)
      | `LongIdent(lident) =>
        isCapitalized(txt)
          ? fail(
              loc,
              "Variable " ++ vbl ++ " must be a capIdent or longCapIdent",
            )
          : lident
      | `CapIdent(name) =>
        isCapitalized(txt)
          ? Lident(name)
          : fail(loc, "Variable " ++ vbl ++ " must be an ident or longIdent")
      | `LongCapIdent(lident) =>
        isCapitalized(txt)
          ? lident
          : fail(loc, "Variable " ++ vbl ++ " must be an ident or longIdent")
      | _ =>
        fail(
          loc,
          "Variable " ++ vbl ++ " expected to be an ident or longIdent",
        )
      },
    )
  };
};

let rec joinLidents = (one, two, loc) =>
  switch (two) {
  | Lident(two) => Ldot(one, two)
  | Ldot(two, three) => Ldot(joinLidents(one, two, loc), three)
  | _ => fail(loc, "Cannot join a lident thats an apply")
  };

let rec evalLongIdent = (locals, lident, loc) => {
  switch (lident) {
  | Lident(name) =>
    switch (evalIdentTxt(locals, name, loc)) {
    | None => Lident(name)
    | Some(lident) => lident
    }
  | Ldot(one, two) =>
    let one = evalLongIdent(locals, one, loc);
    switch (evalIdentTxt(locals, two, loc)) {
    | None => Ldot(one, two)
    | Some(two) => joinLidents(one, two, loc)
    };
  | Lapply(one, two) =>
    Lapply(evalLongIdent(locals, one, loc), evalLongIdent(locals, two, loc))
  };
};

let localToExpr = (expr, local: valueType, loc, vbl) => {
  switch (local) {
  | `Expr(expr) => expr
  | `Ident(name) => {
      ...expr,
      pexp_desc: Pexp_ident({txt: Lident(name), loc}),
    }
  | `LongIdent(lident) => {
      ...expr,
      pexp_desc: Pexp_ident({txt: lident, loc}),
    }
  | `FloatConst(float) => {
      ...expr,
      pexp_desc: Pexp_constant(Pconst_float(string_of_float(float), None)),
    }
  | `IntConst(int) => {
      ...expr,
      pexp_desc: Pexp_constant(Pconst_integer(string_of_int(int), None)),
    }
  | `StringConst(string) => {
      ...expr,
      pexp_desc: Pexp_constant(Pconst_string(string, None)),
    }
  | `BoolConst(bool) => {
      ...expr,
      pexp_desc:
        Pexp_construct({loc, txt: Lident(bool ? "true" : "false")}, None),
    }
  | `Map(_) => fail(expr.pexp_loc, "Maps cannot be turned into expressions")
  | `Pattern(_)
  | `LongCapIdent(_)
  | `CapIdent(_) =>
    fail(
      expr.pexp_loc,
      "Variable " ++ vbl ++ " expected to be an ident, expression, or constant",
    )
  };
};

let localToPattern = (pat, local: valueType, loc, vbl) => {
  switch (local) {
  | `Pattern(pat) => pat
  | `Ident(name) => {...pat, ppat_desc: Ppat_var({txt: name, loc})}
  | `LongCapIdent(lident) => {
      ...pat,
      ppat_desc: Ppat_construct({txt: lident, loc}, None),
    }
  | `CapIdent(lident) => {
      ...pat,
      ppat_desc: Ppat_construct({txt: Lident(lident), loc}, None),
    }
  | `FloatConst(float) => {
      ...pat,
      ppat_desc: Ppat_constant(Pconst_float(string_of_float(float), None)),
    }
  | `IntConst(int) => {
      ...pat,
      ppat_desc: Ppat_constant(Pconst_integer(string_of_int(int), None)),
    }
  | `StringConst(string) => {
      ...pat,
      ppat_desc: Ppat_constant(Pconst_string(string, None)),
    }
  | `BoolConst(bool) => {
      ...pat,
      ppat_desc:
        Ppat_construct({loc, txt: Lident(bool ? "true" : "false")}, None),
    }
  | `Map(_) => fail(pat.ppat_loc, "Maps cannot be turned into patterns")
  | `Expr(_)
  | `LongIdent(_) =>
    fail(pat.ppat_loc, "Variable " ++ vbl ++ " expected to be a pattern")
  };
};

let rec evalLocal = (locals, expr): valueType => {
  switch expr.pexp_desc {
    | Pexp_ident({txt: Lident(name)}) => {
      switch (List.assoc(name, locals)) {
        | exception Not_found => fail(expr.pexp_loc, "Undefined macro variable: " ++ name)
        | (loc, value) => value
      }
    }
    | Pexp_constant(Pconst_string(string, _)) => `StringConst(evalString(locals, string, expr.pexp_loc))
    | Pexp_constant(Pconst_integer(int, _)) => `IntConst(int_of_string(int))
    | Pexp_constant(Pconst_float(float, _)) => `FloatConst(float_of_string(float))
    | Pexp_apply({pexp_desc: Pexp_ident({txt: Lident("=" | "==")})}, [(_, one), (_, two)]) => {
      `BoolConst(evalLocal(locals, one) == evalLocal(locals, two))
    }
    | Pexp_apply({pexp_desc: Pexp_ident({txt: Lident("<>" | "!=")})}, [(_, one), (_, two)]) => {
      `BoolConst(evalLocal(locals, one) != evalLocal(locals, two))
    }
    | Pexp_apply({pexp_desc: Pexp_ident({txt: Lident((">" | "<") as op)})}, [(_, one), (_, two)]) => {
      `BoolConst(switch (evalLocal(locals, one), evalLocal(locals, two)) {
        | (`IntConst(one), `IntConst(two)) => op == ">" ? one > two : one < two
        | (`FloatConst(one), `FloatConst(two)) => op == ">" ? one > two : one < two
        | _ => fail(expr.pexp_loc, "< and > can only be used with both ints or both floats")
      })
    }
    | Pexp_apply({pexp_desc: Pexp_ident({txt: Lident(("+" | "-") as op)})}, [(_, one), (_, two)]) => {
      switch (evalLocal(locals, one), evalLocal(locals, two)) {
        | (`IntConst(one), `IntConst(two)) => `IntConst(op == "+" ? one + two : one - two)
        | (`FloatConst(one), `FloatConst(two)) => `FloatConst(op == "+" ? one +. two : one -. two)
        | _ => fail(expr.pexp_loc, "+ and - can only be used with both ints or both floats")
      }
    }
    | Pexp_apply({pexp_desc: Pexp_ident({txt: Lident("++")})}, [(_, one), (_, two)]) => {
      switch (evalLocal(locals, one), evalLocal(locals, two)) {
        | (`StringConst(one), `StringConst(two)) => `StringConst(one ++ two)
        | _ => fail(expr.pexp_loc, "++ can only be used with both strings")
      }
    }

    | Pexp_apply({pexp_desc: Pexp_ident({txt: Lident("env")})}, [(_, arg)]) => switch (evalLocal(locals, arg)) {
      | `StringConst(name) => switch (Sys.getenv_opt(name)) {
        | None => `StringConst("")
        | Some(v) => `StringConst(v)
      }
      | _ => fail(expr.pexp_loc, "env() must be called with a string")
    }

    | Pexp_apply({pexp_desc: Pexp_ident({txt: Lident("get")})}, [(_, arg), (_, attr)]) => switch (evalLocal(locals, arg), evalLocal(locals, attr)) {
      | (`Map(items), `StringConst(attr)) => switch (List.assoc(attr, items)) {
        | exception Not_found => fail(expr.pexp_loc, "Invalid key " ++ attr)
        | v => v
      }
      | (one, two) => fail(expr.pexp_loc, "get() must be called with a map and a string (got " ++ showType(one) ++ " and " ++ showType(two) ++ ")")
    }

    | Pexp_extension(({txt: "bs.obj"}, PStr([{pstr_desc: Pstr_eval({pexp_desc: Pexp_record(attributes, None)}, _)}])))
    | Pexp_record(attributes, None) => {
      `Map(attributes |> List.map((({Location.txt}, expr)) => (Longident.flatten(txt) |> String.concat("."), evalLocal(locals, expr))))
    }


    | _ => fail(expr.pexp_loc, "Unable to %eval this expression.")
  }
};

let evalCondition = (locals, condition) => {
  switch (evalLocal(locals, condition)) {
    | `BoolConst(v) => v
    | _ => fail(condition.pexp_loc, "Expected a boolean")
  }
};

let rec evalMapper = (locals: locals) => {
  ...Ast_mapper.default_mapper,
  structure_item: (mapper, item) => {
    let item =
      switch (item.pstr_desc) {
      | Pstr_primitive({pval_name, pval_prim} as prim) => {
          ...item,
          pstr_desc:
            Pstr_primitive({
              ...prim,
              pval_name: {
                ...pval_name,
                txt: evalIdent(locals, pval_name.txt, pval_name.loc),
              },
              pval_prim:
                List.map(
                  prim => evalString(locals, prim, item.pstr_loc),
                  pval_prim,
                ),
            }),
        }
      | _ => item
      };
    Ast_mapper.default_mapper.structure_item(mapper, item);
  },
  module_binding: (mapper, mb) => {
    let ident = evalCapIdent(locals, mb.pmb_name.txt, mb.pmb_name.loc);
    let mb =
      if (ident != mb.pmb_name.txt) {
        {...mb, pmb_name: Location.mkloc(ident, mb.pmb_name.loc)};
      } else {
        mb;
      };
    Ast_mapper.default_mapper.module_binding(mapper, mb);
  },
  expr: (mapper, expr) => {
    let expr =
      switch (expr.pexp_desc) {
      // if%eval
      | Pexp_extension(({txt: "eval"}, PStr([{pstr_desc: Pstr_eval({pexp_desc: Pexp_ifthenelse(iff, thenn, elsee)}, _)}]))) => {
        if (evalCondition(locals, iff)) {
          mapper.expr(mapper, thenn)
        } else {
          switch elsee {
            | None => Ast_helper.Exp.construct(Location.mknoloc(Lident("()")), None)
            | Some(elsee) => mapper.expr(mapper, elsee)
          }
        }
      }
      // let%eval
      | Pexp_extension(({txt: "eval"}, PStr([{pstr_desc: Pstr_eval({pexp_desc: Pexp_let(Nonrecursive, [{pvb_pat, pvb_expr}], body)}, _)}]))) => {
        let locals = Args.bindLocal(pvb_pat, evalExpr(locals, pvb_expr)) @ locals;
        evalExpr(locals, body)
      }
      // [%eval something]
      | Pexp_extension(({txt: "eval"}, PStr([{pstr_desc: Pstr_eval(expr, _)}]))) => {
        // let locals = Args.bindLocal(pvb_pat, evalExpr(locals, pvb_expr)) @ locals;
        // evalExpr(locals, body)
        localToExpr(expr, evalLocal(locals, expr), expr.pexp_loc, "expression")
      }
      | Pexp_ident({txt: Lident(vbl)}) =>
        switch (checkEvalPrefix(vbl)) {
        | None => expr
        | Some(vbl) =>
          switch (List.assoc(vbl, locals)) {
          | exception Not_found =>
            fail(expr.pexp_loc, "Variable " ++ vbl ++ " not found")
          | (loc, local) => localToExpr(expr, local, loc, vbl)
          }
        }
      | Pexp_ident(ident) =>
        let newIdent = evalLongIdent(locals, ident.txt, ident.loc);
        if (ident.txt != newIdent) {
          {
            ...expr,
            pexp_desc: Pexp_ident(Location.mkloc(newIdent, ident.loc)),
          };
        } else {
          expr;
        };
      | Pexp_constant(Pconst_string(string, fence)) =>
        let newString = evalString(locals, string, expr.pexp_loc);
        if (newString != string) {
          {
            pexp_loc: Location.none,
            pexp_desc: Pexp_constant(Pconst_string(newString, fence)),
            pexp_attributes:
              expr.pexp_attributes
              |> List.map(((attr, payload)) =>
                   if (attr.Location.txt == "reason.raw_literal") {
                     (
                       attr,
                       PStr([
                         Ast_helper.Str.eval(
                           Ast_helper.Exp.constant(
                             Pconst_string(newString, None),
                           ),
                         ),
                       ]),
                     );
                   } else {
                     (attr, payload);
                   }
                 ),
          };
        } else {
          expr;
        };
      | _ => expr
      };
    Ast_mapper.default_mapper.expr(mapper, expr);
  },
  pat: (mapper, pat) => {
    switch (pat.ppat_desc) {
    | Ppat_var({txt, loc: _}) =>
      switch (checkEvalPrefix(txt)) {
      | None => pat
      | Some(vbl) =>
        switch (List.assoc(vbl, locals)) {
        | exception Not_found =>
          fail(pat.ppat_loc, "Variable " ++ vbl ++ " not found")
        | (loc, local) => localToPattern(pat, local, loc, vbl)
        }
      }
    | Ppat_construct(ident, args) =>
      let newIdent = evalLongIdent(locals, ident.txt, ident.loc);
      let pat =
        if (ident.txt != newIdent) {
          {
            ...pat,
            ppat_desc:
              Ppat_construct(Location.mkloc(newIdent, ident.loc), args),
          };
        } else {
          pat;
        };
      Ast_mapper.default_mapper.pat(mapper, pat);
    | Ppat_constant(Pconst_string(string, fence)) =>
      let newString = evalString(locals, string, pat.ppat_loc);
      if (newString != string) {
        {...pat, ppat_desc: Ppat_constant(Pconst_string(newString, fence))};
      } else {
        pat;
      };
    | _ => Ast_mapper.default_mapper.pat(mapper, pat)
    };
  },
}
and evalExpr = (locals, body) => {
  let mapper = evalMapper(locals);
  mapper.expr(mapper, body);
};

let evalStr = (locals, body) => {
  let mapper = evalMapper(locals);
  mapper.structure(mapper, body);
};

let rec findLet = (txt, macros) =>
  switch (macros) {
  | [] => None
  | [Let(a, b, c, d, e), ..._] when a == txt => Some((b, c, d, e))
  | [_, ...rest] => findLet(txt, rest)
  };

let rec findExpression = (txt, macros) =>
  switch (macros) {
  | [] => None
  | [Expression(n, args, str), ..._] when n == txt => Some((args, str))
  | [_, ...rest] => findExpression(txt, rest)
  };

let rec findToplevel = (txt, macros) =>
  switch (macros) {
  | [] => None
  | [Toplevel(n, args, str), ..._] when n == txt => Some((args, str))
  | [_, ...rest] => findToplevel(txt, rest)
  };

let applyStrMacros = (macros, stri) =>
  switch (stri.pstr_desc) {
  | Pstr_extension(({txt, loc: _}, payload), _attributes) =>
    let found = findToplevel(txt, macros);
    switch (found) {
    | None => None
    | Some((args, body)) =>
      let locals = Args.setupLocals(args, payload, stri.pstr_loc);
      Some(evalStr(locals, body));
    };
  | _ => None
  };

let applyExpMacros = (macros, exp) =>
  switch (exp.pexp_desc) {
  | Pexp_extension(({txt, loc: _}, payload)) =>
    switch (findExpression(txt, macros)) {
    | Some((args, body)) =>
      let locals = Args.setupLocals(args, payload, exp.pexp_loc);
      evalExpr(locals, body);
    | None =>
      switch (payload) {
      | PStr([
          {
            pstr_desc:
              Pstr_eval(
                {
                  pexp_desc:
                    Pexp_let(Nonrecursive, [{pvb_pat, pvb_expr}], body),
                },
                _,
              ),
          },
        ]) =>
        switch (findLet(txt, macros)) {
        | None => exp
        | Some((ppat, pval, pbody, macro)) =>
          let locals =
            Args.bindPat(ppat, pvb_pat)
            @ Args.bindLocal(pval, pvb_expr)
            @ Args.bindLocal(pbody, body);
          evalExpr(locals, macro);
        }
      | _ => exp
      }
    }
  | _ => exp
  };

let applyMapper = macros => {
  ...Ast_mapper.default_mapper,
  structure: (mapper, str) => {
    let items = List.fold_left(
      (result, item) => {
        switch (applyStrMacros(macros, item)) {
        | None => [item, ...result]
        | Some(items) => List.rev(items) @ result
        }
      },
      [],
      str,
    )
    |> List.rev;
    Ast_mapper.default_mapper.structure(
      mapper,
      items,
    );
  },
  expr: (mapper, expr) => {
    let expr = applyExpMacros(macros, expr);
    Ast_mapper.default_mapper.expr(mapper, expr);
  },
}