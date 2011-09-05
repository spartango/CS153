
type format = Reset     |
              Bright    |
              Dim       |
              Underline |
              Blink     |
              Reverse   |
              Hidden    

type color = Black   |
			 Red     |
			 Green   |
			 Yellow  |
			 Blue    |
			 Magenta |
			 Cyan    |
			 White   

let format_RESET     =  0
let format_BRIGHT    =  1
let format_DIM       =  2
let format_UNDERLINE =  3
let format_BLINK     =  4
let format_REVERSE   =  7
let format_HIDDEN    =  8

let color_BLACK     =  30
let color_RED       =  31
let color_GREEN     =  32
let color_YELLOW    =  33
let color_BLUE      =  34
let color_MAGENTA   =  35
let color_CYAN      =  36
let color_WHITE     =  37    

let default_format =  "\x1b\x5b0m"

let format_string target t_format t_color : string = 
    let s_format = 
        match t_format with 
            | Reset     ->  format_RESET
            | Bright    ->  format_BRIGHT
            | Dim       ->  format_DIM
            | Underline ->  format_UNDERLINE
            | Blink     ->  format_BLINK
            | Reverse   ->  format_REVERSE
            | Hidden    ->  format_HIDDEN
    in
    let s_color = 
        match t_color with
            | Black   -> color_BLACK
            | Red     -> color_RED
            | Green   -> color_GREEN
            | Yellow  -> color_YELLOW
            | Blue    -> color_BLUE
            | Magenta -> color_MAGENTA
            | Cyan    -> color_CYAN
            | White   -> color_WHITE
    in
    (*'\x1b\x5bATTR;COLOR;40m\tTARGET\n' *)
    "\x1b\x5b"^(string_of_int s_format)^";"^(string_of_int s_color)^"m"^target^default_format
    
let print_formatted_string target t_format t_color : unit= 
    print_string (format_string target t_format t_color)