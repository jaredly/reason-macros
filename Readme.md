# [%macro]

User-defined macros.
Without ppxs.

```reason

let%macro.fn something = (one, two, three) => {
  one + two + three
};

[%macro something(5, 6, 7)]

// I would like to support spreads (rest arguments).

// Also, macro-ify a record literal
// Or a js object literal

// Allow special things like [%FILENAME] and [%LINENO] and stuff
// Also would like a way to include a representation of the expression as text or something.

let%macro.switch thing = (value, cases) => {
  // hrmmm
  // is there a way to allow pseudo-procedures?
  // not suer.
  // Like going through each case and doing something to it.
  [%map cases(({pattern, condition, body}) => {
    [%case (pattern, condition, body)]
  })]
}


let%macro.let opt = (pattern, value, continuation) => {
  switch value {
    | Some(pattern) => continuation
    | None => None
  }
};

let%macro.let async = (pattern, value, continuation) => {
  Js.Promise.then_(value, pattern => continuation)
};

let%macro.async awesome = 10;


```



- Simple macros
  - List of magics
    - for all macros
      - 

    - for decorator macros
      - `payload` is the thing we're attached to. Will have to get fancy for local bindings, how to include just the binding and not the continuation

      - for a binding
        - `payload.binding.name` will give you the name as an identifier token
          - will throw exception if it's attached to not-a-binding

        - `payload.binding.value` will give you the stuff to the right of the equals

        - `payload.binding.continuation` will give you the stuff after the binding, if we're in an expression.

        - `payload.binding.value.args` will give you the args? if the value is a function. not sure how you would manipulate the args 

      - for a type
        - `payload.type.name` the name

        - `payload.type.manifest` the manifest

        - not sure what you can do with it.

        - ```
[%macro.decorator.withList
[%payload];
type arrayThing = array([%payload.type.name])
]
```

  - Things to figure out
    - how to determine whether to replace the target (when it's a decorator) or not? maybe always replace. So will have to specify that it's a decorator.
      - macro.decorator

    - Do I want to be able to do any logic? maybe with like an `[%conditional if ([%arg "name"] == "awesome") { ... } else { ... } ]`. Yeah! call it `@eval` or `@preval`
      - would have a fairly limited set of comparisons you could do.

      - Exercise: can I reproduce conditional compilation? I'll want a way to rect to ENV vars
        - ```
[%macro.decorator.native
  if%eval ([%env "bsb-backend"] == "native") { [%payload] }
];
[%macro.decorator.js
  if%eval ([%env "bsb-backend"] == "js") { [%payload] }
];
 
[@native]
let platform = "native";
 
[@js]
let platform = "js";
```

      - What about a switch on multiple backends? like Platform.select
        - ```
[%macro.platform
  switch%eval ([%env "bsb-backend"]) {
  | "native" => [%arg "native"]
  | "js" => [%arg "js"]
  }
];


## Examples of things I want to be able to do:

### A platform macro
```reason
let%macro platform = (record: record) => switch%eval ([%env "bsb-backend"]) {
  | "native" => record.native
  | "js" => record.js
}

[%platform {
  | "native" => 10
  | "js" => 5
}]

----

let%macro.decorator js = (payload: expression) => if%eval ([%env "bsb-backend"] == "js") { payload } else {()};
let%macro.decorator.str js = (payload: structure_item) => if%eval ([%env "bsb-backend"] == "js") { payload } else {()};

```

### let_ppx let%Anything style

```reason
let%macro.let async = (pattern, value, continuation) => {
  Js.Promise.then_(value, pattern => continuation)
}

let%async {something} = aPromise;
Js.Promise.resolve(ok)

->

Js.Promise.then_(aPromise, ({something}) => Js.Promise.resolve(ok))
```

```reason
let%macro.try async = (value, cases: rest(case)) => {
  Js.Promise.catch(value, err => [%construct.switch (err, cases)])
}

let caught = try%async (bPromise) {
  | "someText" => Js.Promise.resolve(ok)
  | exn => Js.Promise.reject(exn)
};

->

Js.Promise.catch(bPromise, err => switch err {
  | "someText" => Js.Promise.resolve(ok)
  | exn => Js.Promise.reject(exn)
})

```



### (nope) A "super assert"
```reason
[%super_assert a == 5]

-> if (!(a == b)) {
  print_endline("a == 5")
  assert(false)
}
```

or maybe
```reason
[%assert_equal (a, b)]

->
if (a != b) {
  print_endline(ermmm ok I need type info here, darn)
}
```

## Basic form

Types:
- pattern (generic pattern, can do a switch on it)
- expression (generic epxression, can switch probably)

- ident - if this is an argument that is already a pattern, then we parse a pattern ident. Otherwise an expression ident.
- (tuple literal) translates into the corresponding tuple form, for pattern or expr
- record - some kind of record literal. attributes can be gotten out, and will be checked at macro evaluation time. Can also get the fields as array((ident, expr)), and the rest arg maybe?
- js_object - a js object literal
- string (string literal)
- int (int literal)
- float (float literal)
- poly_variant - `&backtick;Awesome`
  - dunno about args. Might be nice to switch on the arg
  - but I guess I can cover that with - the type is an expression, and I do a switch on it.
  - yeah so maybe I don't need to be able to specify poly_variant? Unless I want to use the string of the variant name somehow. could be a future task
- 

### YOOO if I make this, I totally need an online playground for working with it. Must make jsoo compatible if at all possible.

Forms:
- `%macro` - a basic macro, multiple arguments given as a tuple literal. non-expression arguments given via `[%t: ]` and `[%p? ]`
  - rest argument as the last one, `others: rest(expression)`
- `%macro.let` has two forms:
  - `let%macro.let a = (pattern: pattern, typ: type, expr: expression, expr: expression) =>`
    - maybe cann this `macro.let.typed`?
  - `let%macro.let a = (pattern: pattern, expr: expression, expr: expression) =>`
- `%macro.let.toplevel`
- `%macro.decorator`
- `%macro.decorator.str`
- `%macro.decorator.pat`
- `%macro.decorator.typ`
- `%macro.switch`
  - `let%macro.switch a = (expr: expression, (pattern, cexpr): case, cases: rest(case)) =>`
  - not sure what can be done with this... other than constructing a new switch, I guess maybe with the pattern altered or something
  - `[%construct.switch (expr, cases.map(((pattern, cexpr)) => ([%p? Some(pattern)], cexpr)))]`
- `%macro.try`
  - `let%macro.try a = (expr: expression, cases: rest(case)) => Js.Promise.catch(expr, err => [%construct.switch (err, cases)])`

Constructors:
- `[%construct.switch (expr, cases)]` cases must be a `list(case)`, expr must be some kind of `expression`
- `[%construct.let (pattern, expr, continuation)]`
- `[%construct.let.typed (pattern, typ, expr, continuation)]`
- `[%error "Some macro error message"]`
  - would be nice to be able to provide an ast node to attach it to
- `[%construct.attribute (expr, string or ident)]`
- `[%construct.js_attribute (expr, string or ident)]`
- `[%env "some string"]` get an environmental variable at compile-time, you can do logic with it, becomes a string literal

Transformations:
- `[%string! some_ast_node]` - pretty prints it out using refmt
- `somearray.map(fn => thing)` - transform an array of something into something else....
- `somearray.reduce(base, (collector, item) => collector)` - reduce. not sure how far I can go with this
  - UPDATE: maybe get_in isn't even interesting
  - would be nice to make it so I can support `get_in(some, ["a", "b", "c"])`
  - even fancier `get_in(some, ["a", &backquot;opt("b"), "c"])`
```reason
let%macro get_in = (target: expression, path: list(string)) => {
  path.reduce(target, (target, item) => [%construct.attribute (target, item)])
}

// dunno if I can get this done tbh
let%macro get_in = (target: expression, path: list(expression)) => {
  path.reduce(target, (target, item) => switch%eval item {
    | `opt(expr) => switch ([%construct.]) {},
  });

  // yeah way too complicated
  loop((target, items), (target, items) => {
    switch%eval items {
      | [] => target
      | [one, ...rest] => [%recur (target, rest)]
    }
  })
}
```

- 


```reason
let%macro name = (arg1: type1, arg2: type2) => {
  body
}

[%name (arg1, arg2)]

let%macro.let name = (pattern: pattern, expr: expression, continuation: expression) => {

}

let%macro name = (arg: array(string)) => {
  arg.map(item => [%eval.concat (item, "hello")])
}





->> instead of `arg`
I think I'd rather have it be
let%macro.record platform = record => {
  switch%eval ([%env "bsb-backend"]) {
    | "native" => record.native
    | "js" => record.js
  }
}
 
let x = [%platform {
  native: "someNative",
  js: "someJs"
}];
```

      - How to do arguments?
        - magically interpolate a record definition into arguments
          - so like
```reason
let x = [%platform {
  native: "someNative",
  js: "someJs",
}]
```

        - use `[@arg.somearg "contents"]` decorators
          - 
```reason
let x = [@arg.native "someNative"]
[@arg.js "someJs"]
[%platform]
```

```reason
[@t {
  name: "contents",
  otherThing: 10,
}]
//
[%macro.t
  Testing.run([%arg "name"], [%arg "otherThing"])
]
```

  - 


# Some rust macros

https://doc.rust-lang.org/1.7.0/book/macros.html

```rs

#[macro_export]
macro_rules! vec {
    ( $( $x:expr ),* ) => {
        {
            let mut temp_vec = Vec::new();
            $(
                temp_vec.push($x);
            )*
            temp_vec
        }
    };
}


macro_rules! write_html {
    ($w:expr, ) => (());

    ($w:expr, $e:tt) => (write!($w, "{}", $e));

    ($w:expr, $tag:ident [ $($inner:tt)* ] $($rest:tt)*) => {{
        write!($w, "<{}>", stringify!($tag));
        write_html!($w, $($inner)*);
        write!($w, "</{}>", stringify!($tag));
        write_html!($w, $($rest)*);
    }};
}

fn main() {
    use std::fmt::Write;
    let mut out = String::new();

    write_html!(&mut out,
        html[
            head[title["Macros guide"]]
            body[h1["Macros are the best!"]]
        ]);

    assert_eq!(out,
        "<html><head><title>Macros guide</title></head>\
         <body><h1>Macros are the best!</h1></body></html>");
}


```