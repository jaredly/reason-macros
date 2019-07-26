
module Js = {
  module Unsafe = {
    type any;
    external inject : 'a => any = "%identity";
    external get : ('a, 'b) => 'c = "caml_js_get";
    external set : ('a, 'b, 'c) => unit = "caml_js_set";
    external pure_js_expr : string => 'a = "caml_pure_js_expr";
    let global = pure_js_expr("joo_global_object");
    type obj;
    external obj : array((string, any)) => obj = "caml_js_object";
  };
  type optdef('a) = 'a;
  let undefined: optdef('a) = Unsafe.pure_js_expr("undefined");
  type meth_callback(-'a, +'b);
  type callback('a) = meth_callback(unit, 'a);
  external wrap_callback : ('a => 'b) => meth_callback('c, 'a => 'b) = "caml_js_wrap_callback";
  external wrap_meth_callback : ('a => 'b) => meth_callback('a, 'b) = "caml_js_wrap_meth_callback";
  type t(+'a);
  type js_string;
  external string : string => t(js_string) = "caml_js_from_string";
  external to_string : t(js_string) => string = "caml_js_to_string";
  external create_file : (t(js_string), t(js_string)) => unit = "caml_create_file";
  external to_bytestring : t(js_string) => string = "caml_js_to_byte_string";
};


module Converter = Migrate_parsetree.Convert(Migrate_parsetree.OCaml_404)(Migrate_parsetree.OCaml_407);
module BackConverter = Migrate_parsetree.Convert(Migrate_parsetree.OCaml_407)(Migrate_parsetree.OCaml_404);

Js.Unsafe.set(Js.Unsafe.global, "transform_with_macros", Js.wrap_callback(input => {
  let lexbuf = Lexing.from_string(Js.to_string(input));
  let ast = Reason_toolchain.RE.implementation(lexbuf);
  let currentAst = Converter.copy_structure(ast);
  let mapper = Macros.macroMapper([]);
  let transformed = mapper.structure(mapper, currentAst);
  let oldAst = BackConverter.copy_structure(transformed);
  Reason_toolchain.Reason_syntax.format_implementation_with_comments((oldAst, []), Format.str_formatter);
  let printed = Format.flush_str_formatter();
  Js.string(printed)
}));