
let%macro.let opt = (pattern, value, continuation) => {
  switch eval__value {
    | Some(eval__pattern) => eval__continuation
    | None => None
  }
};

// can also do
[@macro.name "opt.force"]
let%macro.let _ = (pattern, value, continuation) => {
  switch eval__value {
    | Some(eval__pattern) => eval__continuation
    | None => failwith("Unwrapped optional")
  }
};

let x = {
  let%opt a = Some(3);
  let d = 4;
  let%opt.force () = None;
  Some(a + d)
};