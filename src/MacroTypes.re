
open Migrate_parsetree;
open OCaml_407.Ast;

open Parsetree;

type valueType = [
  | `Expr(expression)
  | `Pattern(pattern)
  | `LongCapIdent(Longident.t)
  | `LongIdent(Longident.t)
  | `CapIdent(string)
  | `Ident(string)
  | `StringConst(string)
  | `BoolConst(bool)
  | `IntConst(int)
  | `Option(option(valueType))
  | `FloatConst(float)
  | `Map(list((string, valueType)))
];

let rec showType = (t: valueType) => switch t {
  | `Option(Some(v)) => "option(" ++ showType(v) ++ ")"
  | `Option(None) => "option(<empty>)"
  | `Expr(_) => "expression"
  | `Pattern(_) => "pattern"
  | `LongCapIdent(lident) => "long cap identifier (" ++ String.concat(".", Longident.flatten(lident)) ++ ")"
  | `LongIdent(lident) => "long identifier (" ++ String.concat(".", Longident.flatten(lident)) ++ ")"
  | `CapIdent(name) => "cap identifier (" ++ name ++ ")"
  | `Ident(name) => "identifier (" ++ name ++ ")"
  | `StringConst(str) => "string constant (" ++ str ++ ")"
  | `BoolConst(bool) => "bool constant (" ++ (bool ? "true" : "false") ++ ")"
  | `IntConst(int) => "string constant (" ++ string_of_int(int) ++ ")"
  | `FloatConst(float) => "string constant (" ++ string_of_float(float) ++ ")"
  | `Map(_) => "map"
}

type locals = list((string, (Location.t, valueType)));

type macro =
  | Let(string, pattern, pattern, pattern, expression)
  | Toplevel(string, list(pattern), structure)
  | Expression(string, list(pattern), expression)
  | Pattern(string, list(pattern), pattern)

let fail = (loc, txt) => raise(Location.Error(Location.error(~loc, txt)));
let fail2 = (loc, txt, loc2, txt2) => raise(Location.Error(Location.error(~loc, ~sub=[
  Location.error(~loc=loc2, txt2)
], txt)));