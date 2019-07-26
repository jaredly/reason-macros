let%macro simple = (num: int) => "hi $eval{num} times";
let%macro more = (str: string, num: int) => "hi $eval{num} and $eval{str}";

[%simple 5];
[%more ("People", 3)];