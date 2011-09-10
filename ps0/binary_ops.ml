(* Higher order funciton for shift functions *)
let shift_or (shifter: int32 -> int -> int32) (targets : (int32 * int) list) : int32 =
    let op = 
        fun (accum : int32) (item : (int32 * int)) -> 
            match item with (value, shift) -> (Int32.logor accum (shifter value shift))
    in         
    List.fold_left op 0l targets

(* Shifts numbers left by a certain amount before ORing them together *)
let left_shift_or (targets : (int32 * int) list) : int32 = 
    shift_or Int32.shift_left targets

(* Shifts numbers right by a certain amount before ORing them together *)
let right_shift_or (targets : (int32 * int) list) : int32 = 
    shift_or Int32.shift_right_logical targets

(* Utility function to get lower bits of a 32 bit int, shedding the sign *)
let int32_lower (n : int32) : int32 = (Int32.logand n 0x0000FFFFl)

(* Utility function to get upper bits of a 32 bit int*)
let int32_upper (n : int32) : int32 = (Int32.shift_right_logical n 16)

(* Utility function to get lower bits of a 32 bit int, maintaining the sign *)
let int32_signed_lower (n : int32) : int32 = 
    (Int32.logor (Int32.shift_right_logical (Int32.logand n 0x80000000l) 16) 
                 (Int32.logand n 0x00007FFFl))
