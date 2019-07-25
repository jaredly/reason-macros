
open Parsetree;
open Longident;

type valueType = [
  | `Expr(expression)
  | `Pattern(pattern)
  | `LongCapIdent(Longident.t)
  | `LongIdent(Longident.t)
  | `CapIdent(string)
  | `Ident(string)
];

type locals = list((string, (Location.t, valueType)));

type macro =
  | Let(string, pattern, pattern, pattern, expression)
  | Toplevel(string, list(pattern), structure)
  | Expression(string, list(pattern), expression)

let fail = (loc, txt) => raise(Location.Error(Location.error(~loc, txt)));
let fail2 = (loc, txt, loc2, txt2) => raise(Location.Error(Location.error(~loc, ~sub=[
  Location.error(~loc=loc2, txt2)
], txt)));