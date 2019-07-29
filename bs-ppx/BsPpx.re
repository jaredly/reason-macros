open Migrate_parsetree;
open OCaml_407.Ast;

let process = (input, output) => {
  let mapper = Macros.macroMapper([]);

  let ic = open_in(input);
  switch (Ast_io.from_channel(ic)) {
  |  Ok((fn,  Ast_io.Intf((module V), sg))) =>
    Location.input_name := fn;

    let signature = Versions.migrate((module V), (module OCaml_current)).copy_signature(sg);
    let mapped = mapper.signature(mapper, signature);
    let back = Versions.migrate((module OCaml_current), (module V)).copy_signature(mapped);
    let oc = open_out(output);
    Ast_io.to_channel(oc, fn, Ast_io.Intf((module V), back));
    close_out(oc);

  |  Ok((fn,  Ast_io.Impl((module V), st))) =>
    Location.input_name := fn;

    let structure = Versions.migrate((module V), (module OCaml_current)).copy_structure(st);
    let mapped = mapper.structure(mapper, structure);
    let back = Versions.migrate((module OCaml_current), (module V)).copy_structure(mapped);
    let oc = open_out(output);
    Ast_io.to_channel(oc, fn, Ast_io.Impl((module V), back));
    close_out(oc);

  | Error(_) => failwith("Invalid ast file")
  };

}

switch (Sys.argv) {
  | [|_, input, output|] => {
    process(input, output)
  }
  | _ => {
    print_endline("Invalid args");
    exit(1)
  }
}
