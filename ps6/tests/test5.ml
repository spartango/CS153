let f = fun x -> fun y -> if x < 0 then x + y else y + x in
let inc = f 1 in
let dec = f (0 - 1) in
let p = (inc 3, dec 2) in
(fst p) + (snd p)
