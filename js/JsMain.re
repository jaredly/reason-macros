
open Js_of_ocaml;

module Converter = Migrate_parsetree.Convert(Migrate_parsetree.OCaml_404)(Migrate_parsetree.OCaml_407);
module BackConverter = Migrate_parsetree.Convert(Migrate_parsetree.OCaml_407)(Migrate_parsetree.OCaml_404);

let parse = input => {
  let lexbuf = Lexing.from_string(Js.to_string(input));
  let ast = Reason_toolchain.RE.implementation(lexbuf);
  Converter.copy_structure(ast);
};

let print = ast => {
  let oldAst = BackConverter.copy_structure(ast);
  Reason_toolchain.Reason_syntax.format_implementation_with_comments((oldAst, []), Format.str_formatter);
  Format.flush_str_formatter();
};

Js.export("transform", Js.wrap_callback((macro_code, input) => {
  let macroAst = parse(macro_code);
  let macros = Macros.collect(macroAst);
  let ast = parse(input);

  // do the actual transform.
  let mapper = Macros.macroMapper(macros);
  let transformed = mapper.structure(mapper, ast);

  Js.string(print(transformed))
}));