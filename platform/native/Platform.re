
let strrx = Str.regexp("\\$eval\\{\\([^}]+\\)\\}");

let regexpReplace = (rx, string, fn) => {
  let rec loop = string => {
    switch (Str.search_forward(rx, string, 0)) {
    | exception Not_found => string
    | _ =>
      let tpl = fn(string);
      loop(Str.replace_first(rx, tpl, string));
    };
  };
  loop(string);
};

let interpolateString = (string, lookup) => {
  regexpReplace(
    strrx,
    string,
    current => {
      let text = Str.matched_group(1, current);
      lookup(text)
    }
  )
};