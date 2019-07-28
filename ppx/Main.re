
open Migrate_parsetree;
open OCaml_407.Ast;

let sharedMacros = ref("");


let maybeStat = (path) =>
  try (Some(Unix.stat(path))) {
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

module Converter = Migrate_parsetree.Convert(Migrate_parsetree.OCaml_404)(Migrate_parsetree.OCaml_407);

let () = Driver.register(~name="macros",
~args=[
    ("--shared-macros", Arg.Set_string(sharedMacros), "PATH Set [%sharedMacros] to PATH")
], Versions.ocaml_407, (_config, cookies) => {
  let macros = switch (sharedMacros^) {
    | "" => []
    | path => {
      switch (readFile(path)) {
        | None => failwith("Cannot read shared macros file " ++ path ++ " from " ++       Unix.getcwd())
        | Some(text) => {
          let lexbuf = Lexing.from_string(text);
          let ast = Reason_toolchain.RE.implementation(lexbuf);
          let ast = Converter.copy_structure(ast);
          let (_items, macros) = Macros.collect(ast, []);
          macros
        }
      }
    }
  };
  Macros.macroMapper(macros)
});
