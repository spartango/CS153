module RevList (L: LISTARGS) : (RLIST with
                                          type element = L.element)=  
    struct
        type element = L.element
        type rlist = (element list) 
        let empty = []
        let rec app_list (accum: rlist) (x: element list) : rlist=
            match x with
                | []         -> accum
                | head::tail -> app_list (head::accum) tail
        let rev_list (l: element list) : rlist = app_list [] l                      
        let to_list (x: rlist) : element list = app_list [] x      
    end

(* Reverse lists for MIPS instructions *)
module RInstList = RevList(struct type element = inst end)

(* Sugar for appending a list to an rlist *)
let (<@) (a: RInstList.rlist) (b: inst list) = RInstList.app_list a b
