
exception FailedStabilization

let run_until_stable t_func init_arg limit =
  let rec until_stable arg count =
    if count >= limit then raise FailedStabilization
    else
      let out = t_func arg in
      if (out = arg) then let _ = print_string (string_of_int count) in out
      else until_stable out (count + 1) 
  in
  until_stable init_arg 0 