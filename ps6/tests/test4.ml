let f = fun x -> fun y -> if x < 0 then x - y else x + y in
let inc = f 1 in
(inc 3, inc 2)

