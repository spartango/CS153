(* Utility function to create a bitmask of length offset from the leftmost bit by
 left_offset *)
let masker (length: int) (left_offset: int) : int32 =
    Int32.shift_right_logical (Int32.shift_right Int32.min_int (length - 1)) left_offset

(* Higher order funciton for shift functions *)
let shift_or (shifter: int32 -> int -> int32) (targets : (int32 * int) list) : int32 =
    let op = 
        fun (accum : int32) (item : (int32 * int)) -> 
            let (value, shift) = item -> (Int32.logor accum (shifter value shift))
    in         
    List.fold_left op 0l targets

(* Shifts numbers left by a certain amount before ORing them together *)
let left_shift_or (targets : (int32 * int) list) : int32 = 
    shift_or Int32.shift_left targets

(* Shifts numbers right by a certain amount before ORing them together *)
let right_shift_or (targets : (int32 * int) list) : int32 = 
    shift_or Int32.shift_right_logical targets

(* Utility function to get lower bits of a 32 bit int, shedding the sign *)
let int32_lower (n : int32) : int32 = (Int32.logand n (masker 16 16))

(* Utility function to get upper bits of a 32 bit int*)
let int32_upper (n : int32) : int32 = (Int32.shift_right_logical n 16)

(* Utility function to get lower bits of a 32 bit int, maintaining the sign *)
let int32_signed_lower (n : int32) : int32 = 
    (Int32.logor (Int32.shift_right_logical (Int32.logand n (masker 1 0)) 16) 
                 (Int32.logand n (masker 15 17)))

(* Translates a sudo int16 into an int32 *)
let int16_to_int32 (i16: int32) : int32 =
    let sign_bit = Int32.logand i16 (masker 1 16) in
    if sign_bit = 0l
    then (Int32.logand i16 (masker 15 17))
    (* Fill upper have with 1 bits for negatives *)
    else Int32.logor (masker 17 0) i16



