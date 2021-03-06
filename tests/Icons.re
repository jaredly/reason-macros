///
///
// ok, yeah we're having to
// - somehow indicate that `Name` should be substituted with the eval'd `name`
//   - yeah let's do `Eval__${the name of the eval-time variable}`
// - the string literal in the external needs interpolation
//   - again, let's use the `eval` moniker, so `some$eval{name}`
//   - could also do `some$eval{uppercase(name)}` or something
// - for other identifiers, the `eval__` prefix will do the same thing.

let%macro.toplevel ionicon = (name: capIdent, iconName: capIdent, small: ident) => {
  [%str module Eval__name = {
    let name = "$eval{iconName}";
    let eval__small = "awesome$eval{small}";
    [@bs.module] [@react.component]
    external make:
      (
        ~className: string=?,
        ~fontSize: string=?,
        ~color: string=?,
        ~onClick: 'event => unit=?
      ) =>
      string =
      "react-ionicons/lib/$eval{iconName}";
  }];
};

[%%ionicon (Link, IosLink, hi)];
[%%ionicon (Camera, MdCamera, ho)];
