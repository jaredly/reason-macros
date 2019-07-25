
open Parsetree;
open Longident;
open MacroTypes;

let bindPat = (pattern, pat): locals => switch pattern {
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

let bindLocal = (pattern, expr): locals => switch pattern {
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

let setupLocals = (args, payload, loc): locals => {
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
