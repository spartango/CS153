let a = [3;4;5;6;7] in
let b = [6;7] in
let c = (6, hd(a) + hd(tl(a))) in
let d = (7,()) in
(c,d)
