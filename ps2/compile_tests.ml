open Test_framework
open Pretty_print
open Compile

let test_collect_vars p =
    let vars = collect_vars(p); !variables in
        ()
