
open Js_of_ocaml;

let strrx = Regexp.regexp("\\$eval\\{([^}]+)\\}");

let regexpReplace = (regexp, originalString, lookup) => {
  let rec loop = currentString => {
    switch (Regexp.search_forward(regexp, currentString, 0)) {
    | exception Not_found => currentString
    | None => currentString
    | Some((_, result)) =>
      let tpl = lookup(result);
      loop(Regexp.replace_first(regexp, currentString, tpl));
    };
  };
  loop(originalString);
};

let interpolateString = (string, lookup) => {
  regexpReplace(
    strrx,
    string,
    (result) => {
      let text = Regexp.matched_group(result, 1);
      switch (text) {
        | None => assert(false)
        | Some(text) => lookup(text)
      }
    }
  )
};

let check = (one, two) => {
  if (one != two) {
    print_endline("One: " ++ one ++ "; Two: " ++ two)
    assert(false)
  }
}

// print_endline("One")
// check(interpolateString("hi$eval{awesome}ho", (_) => "sauce"), "hisauceho")
// print_endline("Two")
// check(interpolateString("$eval{awesome}", (_) => "sauce"), "sauce")
