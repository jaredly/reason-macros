
let%macro sink = (cap: capIdent, longCap: longCapIdent, id: ident, longId: longIdent, pat: pattern, exp: expression, num: int, isTrue: bool) => {
  let eval__id = 2;
  let%eval newVar = 5;
  let m = Js.log(eval__id);
  let m = if%eval (isTrue) {
    eval__newVar;
  } else {
    eval__newVar * 20;
  };
  if%eval (num - 14 > 20) {
    Js.log("Large number: $eval{num}")
  }
  switch (eval__exp) {
    | Eval__cap(47) => None
    | eval__pat => eval__exp
    | _ => None
  };
  let%eval (myMap: map) = {one: 2, three: "four", someIdent: "yes"};
  let%eval number = [%eval get(myMap, "$eval{id}")];
  eval__number
};

let%macro dev = (expression: expression) => if%eval (env("node_env") != "production") { eval__expression } else { () };

let%macro env = (vbl: string) => [%eval env(vbl)];

[%sink (Some, Some.Long.Cap, someIdent, Some.Long.ident, [%pat? Some(_)], None, 35, false)]

let%macro.pat apply = (ident: string, args: pattern) => [%pat? Pexp_apply({pexp_desc: Pexp_ident({txt: Lident(eval__ident)})}, eval__args)];

switch expr {
  | [%apply ("++", [%pat? [(_, one), (_, two)]])] => one
  | _ => ()
};

[%dev print_endline("Hello world")];

let editor = [%env "EDITOR"];