
let%macro sink = (cap: capIdent, longCap: longCapIdent, id: ident, longId: longIdent, pat: pattern, exp: expression, num: int, isTrue: bool) => {
  let eval__id = 2;
  let m = Js.log(eval__id);
  switch (eval__exp) {
    | Eval__cap(47) => None
    | eval__pat => eval__exp
    | _ => None
  }
};

[%sink (Some, Some.Long.Cap, someIdent, Some.Long.ident, [%pat? Some(_)], None, 35, false)]
