// Reason Macros!
// Use templates to write your code for you.

let%macro add5 = (input: ident, log: string) => {
  if%eval (env("node_environment") != "production") {
    Js.log(eval__input)
  };
  eval__input + 5
};


let num = 15;
let someNumber = [%add5 (num, "error")];

// int, string, float, bool -- constants
// ident, capIdent, longIdent, longCapIdent
// expression, pattern
let%macro.toplevel ionicon = (name: capIdent, iconName: capIdent) => {
  [%str // toplevel
    module Eval__name = {
      let name = "$eval{name}";
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
    }
  ];
};

[%%ionicon (Link, IosLink)];
[%%ionicon (Download, MdDownload)];

let%macro.let opt = (pattern, value, continuation) =>
  switch (eval__value) {
  | None => None
  | Some(eval__pattern) => eval__continuation
  };

let doSomethingWithOptionals = () => {
  let%opt who = Some("world");
  let%opt greeting = Some("Hello");
  Some(greeting ++ "" ++ who);
};

let json = request => Js.Promise.resolve(request);
let fetch = url => Js.Promise.resolve("Hello wold");

let%macro.let async = (pattern, value, continuation) => {
  Js.Promise.then_(
    eval__pattern => eval__continuation,
    eval__value
  );
};

let doSomethingAsync = () => {
  let%async text = fetch("Hello");
  let%async more = text->json;
  Js.Promise.resolve(more);
};