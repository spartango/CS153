
exception FailedStabilization

let run_until_stable t_func limit =
  let rec until_stable prev count =
    if count >= limit then raise FailedStabilization
    else
      let out = t_func () in
      match prev with 
      | None           -> until_stable (Some(out)) (count + 1)
      | Some(prev_val) -> if out = prev_val then out 
                        else until_stable (Some(out)) (count + 1)
  in
  until_stable None 0 