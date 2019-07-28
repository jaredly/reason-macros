
let%macro go = (m: int) => "Ueah $eval{m}"

print_endline("Hi " ++ [%go 5] ++ [%shared "Boo"])
print_endline("Hi " ++ [%go 5])
