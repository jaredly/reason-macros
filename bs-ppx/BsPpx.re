open Migrate_parsetree;
open OCaml_407.Ast;

module Converter =
  Migrate_parsetree.Convert(
    Migrate_parsetree.OCaml_404,
    Migrate_parsetree.OCaml_407,
  );
let parse = input => {
  let lexbuf = Lexing.from_string(input);
  let ast = Reason_toolchain.RE.implementation(lexbuf);
  Converter.copy_structure(ast);
};

let maybeStat = path =>
  try(Some(Unix.stat(path))) {
  | Unix.Unix_error(Unix.ENOENT, _, _) => None
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
    let rec loop = acc =>
      switch (try_read()) {
      | Some(s) => loop([s, ...acc])
      | None =>
        close_in(ic);
        List.rev(acc);
      };
    let text = loop([]) |> String.concat(String.make(1, '\n'));
    Some(text);
  | _ => None
  };
};

let process = (~shared=?, input, output) => {
  let macros =
    switch (shared) {
    | None => []
    | Some(fname) =>
      let ast =
        parse(
          switch (readFile(fname)) {
          | None => failwith("No such shared file " ++ fname)
          | Some(text) => text
          },
        );
      Macros.collect(ast);
    };
  let mapper = Macros.macroMapper(macros);

  let ic = open_in(input);
  switch (Ast_io.from_channel(ic)) {
  | Ok((fn, Ast_io.Intf((module V), sg))) =>
    Location.input_name := fn;

    let signature =
      Versions.migrate((module V), (module OCaml_current)).copy_signature(
        sg,
      );
    let mapped = mapper.signature(mapper, signature);
    let back =
      Versions.migrate((module OCaml_current), (module V)).copy_signature(
        mapped,
      );
    let oc = open_out(output);
    Ast_io.to_channel(oc, fn, Ast_io.Intf((module V), back));
    close_out(oc);

  | Ok((fn, Ast_io.Impl((module V), st))) =>
    Location.input_name := fn;

    let structure =
      Versions.migrate((module V), (module OCaml_current)).copy_structure(
        st,
      );
    let mapped = mapper.structure(mapper, structure);
    let back =
      Versions.migrate((module OCaml_current), (module V)).copy_structure(
        mapped,
      );
    let oc = open_out(output);
    Ast_io.to_channel(oc, fn, Ast_io.Impl((module V), back));
    close_out(oc);

  | Error(_) => failwith("Invalid ast file")
  };
};

switch (Sys.argv) {
| [|_, shared, input, output|] => process(~shared, input, output)
| [|_, input, output|] => process(input, output)
| _ =>
  print_endline("Invalid args");
  exit(1);
};