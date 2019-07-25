
open MacroTypes;
open Parsetree;
open Longident;



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
