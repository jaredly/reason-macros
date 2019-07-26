
module Converter = Migrate_parsetree.Convert(Migrate_parsetree.OCaml_404)(Migrate_parsetree.OCaml_407);
module BackConverter = Migrate_parsetree.Convert(Migrate_parsetree.OCaml_407)(Migrate_parsetree.OCaml_404);

module Fs = {

let maybeStat = (path) =>
  try (Some(Unix.stat(path))) {
  | Unix.Unix_error(Unix.ENOENT, _, _) => None
  };

let writeFile = (path, contents) => {
  try {
    let out = open_out(path);
    output_string(out, contents);
    close_out(out);
    true
  } {
    | _ => false
  }
};
let readFile = path => {
  switch (maybeStat(path)) {
  | Some({Unix.st_kind: Unix.S_REG}) =>
    let ic = open_in(path);
    let try_read = () =>
      switch (input_line(ic)) {
      | exception End_of_file => None
      | x => Some(x)
      };
    let rec loop = (acc) =>
      switch (try_read()) {
      | Some(s) => loop([s, ...acc])
      | None =>
        close_in(ic);
        List.rev(acc)
      };
    let text = loop([]) |> String.concat(String.make(1, '\n'));
    Some(text)
  | _ => None
  }
};

let readStdin = () => {
  let ic = stdin;
    let try_read = () =>
      switch (input_line(ic)) {
      | exception End_of_file => None
      | x => Some(x)
      };
    let rec loop = (acc) =>
      switch (try_read()) {
      | Some(s) => loop([s, ...acc])
      | None =>
        close_in(ic);
        List.rev(acc)
      };
    let text = loop([]) |> String.concat(String.make(1, '\n'));
    text
};

let readDirectory = (dir) => {
  let maybeGet = (handle) =>
    try (Some(Unix.readdir(handle))) {
    | End_of_file => None
    };
  let rec loop = (handle) =>
    switch (maybeGet(handle)) {
    | None =>
      Unix.closedir(handle);
      []
    | Some(name) when name == Filename.current_dir_name || name == Filename.parent_dir_name => loop(handle)
    | Some(name) => [name, ...loop(handle)]
    };
  switch (Unix.opendir(dir)) {
    | exception Unix.Unix_error(Unix.ENOENT, "opendir", _dir) => []
    | handle => loop(handle)
  }
};
}

let parse = input => {
  let lexbuf = Lexing.from_string(input);
  let ast = Reason_toolchain.RE.implementation(lexbuf);
  Converter.copy_structure(ast);
};

let print = ast => {
  let oldAst = BackConverter.copy_structure(ast);
  Reason_toolchain.Reason_syntax.format_implementation_with_comments((oldAst, []), Format.str_formatter);
  Format.flush_str_formatter();
};

let endsWith = (s, suffix) => {
  if (suffix == "") {
    true
  } else {
    let p = String.length(suffix);
    let l = String.length(s);
    p <= String.length(s) && String.sub(s, l - p, p) == suffix
  }
};

switch (Sys.argv) {
  | [|_|] => {
    let input = Fs.readStdin();

    let ast = parse(input);

    // do the actual transform.
    let mapper = Macros.macroMapper([]);
    let transformed = mapper.structure(mapper, ast);

  print_endline(print(transformed));
  }
  | [|_, "test"|] => {
    let failed = ref(0);
    Fs.readDirectory("./tests") |> List.iter(fname => {
      let full = "./tests/" ++ fname;
      if (endsWith(fname, ".re")) {
        let input = Fs.readFile(full);
        switch input {
          | None => ()
          | Some(input) => {
            let ast = parse(input);

            // do the actual transform.
            let mapper = Macros.macroMapper([]);
            let transformed = mapper.structure(mapper, ast);

            let output = (print(transformed));


            switch (Fs.readFile(full ++ ".out")) {
              | None => Fs.writeFile(full ++ ".out", output) |> ignore
              | Some(current) => {
                if (current != output) {
                  print_endline("Failure! Output for " ++ full ++ " was different!");
                  Fs.writeFile(full ++ ".out.new", output) |> ignore;
                  failed := failed^ + 1;
                } else {
                  print_endline(full ++ " matched")
                }
              }
            }

          }
        }
      }
    });
    if (failed^ > 0) {
      print_endline("Failures: " ++ string_of_int(failed^))
      exit(1)
    } else {
      print_endline("All clear!");
      exit(0)
    }
  }
  |_ => print_endline("Usage: macros [test]")
}