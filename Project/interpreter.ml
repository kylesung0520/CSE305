    type stackValue = BOOL of bool | INT of int | ERROR | UNIT | STRING of string | NAME of string | CLOSURES | NONE
    type command = ADD | SUB | MUL | DIV | REM | NEG | SWAP | TOSTRING | PRINTLN | QUIT | POP | PUSH of stackValue
                  | CAT | AND | OR | NOT | LESSTHAN | EQUAL | IF | BIND

    let interpreter ((input : string) , (output : string)) : unit =
        let ic = open_in input in
        let oc = open_out output in 

        let rec loop_read acc =
            try
                let l = String.trim(input_line ic) in loop_read (l::acc)
            with
            End_of_file -> List.rev acc in

        let file_write str_val = Printf.fprintf oc "%s\n" str_val in

        let strList = loop_read [] in

    let popOP (svlst : stackValue list) =
        match svlst with
        |[] -> ERROR::svlst
        |sv::res_sv -> res_sv
    in

    let rec findVal (str : string) (hashlst : (string * stackValue) list) : stackValue =
        match hashlst with
        |[] -> NONE
        |(k,v)::res_hash -> if(k = str) then v
                            else findVal str res_hash
    in

    let addOP (svlst : stackValue list) (hashlst : (string * stackValue) list)=
        match svlst with
        |[] -> ERROR::svlst
        |sv::res_sv -> match res_sv with
                       |[] -> ERROR::svlst
                       |sv2::res_sv2 -> (match sv with
                                        |INT(a) -> (match sv2 with
                                                    |INT(b)-> INT(a+b)::res_sv2
                                                    |NAME(b) -> (match (findVal b hashlst) with
                                                                |INT(c) -> INT(a+c)::res_sv2
                                                                |_ -> ERROR::svlst)
                                                    |_ -> ERROR::svlst)
                                        |NAME(a) -> (match findVal a hashlst with
                                                    |INT(b) -> (match sv2 with
                                                                |INT(c) -> INT(b+c)::res_sv2
                                                                |NAME(c) -> (match findVal c hashlst with
                                                                            |INT(d) -> INT(b+d)::res_sv2
                                                                            |_ -> ERROR::svlst)
                                                                |_ -> ERROR::svlst)
                                                    |_ -> ERROR::svlst)
                                        |_ -> ERROR::svlst)
    in


    let subOP (svlst : stackValue list) (hashlst : (string * stackValue) list)=
        match svlst with
        |[] -> ERROR::svlst
        |sv::res_sv -> match res_sv with
                       |[] -> ERROR::svlst
                       |sv2::res_sv2 -> (match sv with
                                        |INT(a) -> (match sv2 with
                                                    |INT(b)-> INT(b-a)::res_sv2
                                                    |NAME(b) -> (match (findVal b hashlst) with
                                                                |INT(c) -> INT(c-a)::res_sv2
                                                                |_ -> ERROR::svlst)
                                                    |_ -> ERROR::svlst)
                                        |NAME(a) -> (match findVal a hashlst with
                                                    |INT(b) -> (match sv2 with
                                                                |INT(c) -> INT(c-b)::res_sv2
                                                                |NAME(c) -> (match findVal c hashlst with
                                                                            |INT(d) -> INT(d-b)::res_sv2
                                                                            |_ -> ERROR::svlst)
                                                                |_ -> ERROR::svlst)
                                                    |_ -> ERROR::svlst)
                                        |_ -> ERROR::svlst)
    in

    let mulOP (svlst : stackValue list) (hashlst : (string * stackValue) list)=
        match svlst with
        |[] -> ERROR::svlst
        |sv::res_sv -> match res_sv with
                       |[] -> ERROR::svlst
                       |sv2::res_sv2 -> (match sv with
                                        |INT(a) -> (match sv2 with
                                                    |INT(b)-> INT(a*b)::res_sv2
                                                    |NAME(b) -> (match (findVal b hashlst) with
                                                                |INT(c) -> INT(a*c)::res_sv2
                                                                |_ -> ERROR::svlst)
                                                    |_ -> ERROR::svlst)
                                        |NAME(a) -> (match findVal a hashlst with
                                                    |INT(b) -> (match sv2 with
                                                                |INT(c) -> INT(b*c)::res_sv2
                                                                |NAME(c) -> (match findVal c hashlst with
                                                                            |INT(d) -> INT(b*d)::res_sv2
                                                                            |_ -> ERROR::svlst)
                                                                |_ -> ERROR::svlst)
                                                    |_ -> ERROR::svlst)
                                        |_ -> ERROR::svlst)
    in

    let divOP (svlst : stackValue list) (hashlst : (string * stackValue) list)=
        match svlst with
        |[] -> ERROR::svlst
        |sv::res_sv -> match res_sv with
                       |[] -> ERROR::svlst
                       |sv2::res_sv2 -> (match sv with
                                        |INT(a) -> if(a == 0) then ERROR::svlst
                                                   else (match sv2 with
                                                        |INT(b)-> INT(b/a)::res_sv2
                                                        |NAME(b) -> (match (findVal b hashlst) with
                                                                    |INT(c) -> INT(c/a)::res_sv2
                                                                    |_ -> ERROR::svlst)
                                                        |_ -> ERROR::svlst)
                                        |NAME(a) -> (match findVal a hashlst with
                                                    |INT(b) -> if(b == 0) then ERROR::svlst
                                                                else (match sv2 with
                                                                    |INT(c) -> INT(c/b)::res_sv2
                                                                    |NAME(c) -> (match findVal c hashlst with
                                                                                |INT(d) -> INT(d/b)::res_sv2
                                                                                |_ -> ERROR::svlst)
                                                                    |_ -> ERROR::svlst)
                                                    |_ -> ERROR::svlst)
                                        |_ -> ERROR::svlst)
    in

    let remOP (svlst : stackValue list) (hashlst : (string * stackValue) list)=
        match svlst with
        |[] -> ERROR::svlst
        |sv::res_sv -> match res_sv with
                       |[] -> ERROR::svlst
                       |sv2::res_sv2 -> (match sv with
                                        |INT(a) -> if(a == 0) then ERROR::svlst
                                                   else (match sv2 with
                                                        |INT(b)-> INT(b mod a)::res_sv2
                                                        |NAME(b) -> (match (findVal b hashlst) with
                                                                    |INT(c) -> INT(c mod a)::res_sv2
                                                                    |_ -> ERROR::svlst)
                                                        |_ -> ERROR::svlst)
                                        |NAME(a) -> (match findVal a hashlst with
                                                    |INT(b) -> if(b == 0) then ERROR::svlst
                                                                else (match sv2 with
                                                                    |INT(c) -> INT(c mod b)::res_sv2
                                                                    |NAME(c) -> (match findVal c hashlst with
                                                                                |INT(d) -> INT(d mod b)::res_sv2
                                                                                |_ -> ERROR::svlst)
                                                                    |_ -> ERROR::svlst)
                                                    |_ -> ERROR::svlst)
                                        |_ -> ERROR::svlst)
    in
    let negOP (svlst : stackValue list) (hashlst : (string * stackValue) list) =
        match svlst with
        |[] -> ERROR::svlst
        |sv::res_sv -> match sv with
                      |INT(a) -> if(a = 0) then svlst
                                 else INT(-1*a)::res_sv
                      |NAME(a) -> (match findVal a hashlst with
                                  |INT(b) -> if(b = 0) then INT(0)::res_sv
                                             else INT(-1*b)::res_sv
                                  |_ -> ERROR::svlst)
                      |_ -> ERROR::svlst
    in

    let swapOP (svlst : stackValue list) =
        match svlst with
        |[] -> ERROR::svlst
        |sv::res_sv -> match res_sv with
                       |[] -> ERROR::svlst
                       |sv2::res_sv2 -> sv2::sv::res_sv2
    in

    let toStringOP (svlst : stackValue list) (hashlst : (string * stackValue) list)=
        match svlst with
        |[] -> ERROR::svlst
        |sv::res_sv -> match sv with
                       |NAME(a) -> STRING(a)::res_sv
                       |INT(a) -> STRING(string_of_int a)::res_sv
                       |ERROR -> STRING(":error:")::res_sv
                       |UNIT -> STRING(":unit:")::res_sv
                       |BOOL(a) -> STRING(":"^string_of_bool a^":")::res_sv
                       |_ -> svlst
    in

    let printlnOP (svlst : stackValue list) =
        match svlst with
        |[] -> ERROR::svlst
        |sv::res_sv -> match sv with
                       |STRING(a) -> file_write(a);
                                     res_sv
                       |_ -> ERROR::svlst
    in

    let catOP (svlst : stackValue list) (hashlst : (string * stackValue) list ) =
        match svlst with
        |[] -> ERROR::svlst
        |sv::res_sv -> match sv with
                       |STRING(a) -> (match res_sv with
                                     |[] -> ERROR::svlst
                                     |sv2::res_sv2 -> (match sv2 with
                                                      |STRING(b) -> STRING(b^a)::res_sv2
                                                      |NAME(b) -> (match findVal b hashlst with
                                                                  |STRING(c) -> STRING(c^a)::res_sv2
                                                                  |_ -> ERROR::svlst)
                                                      |_ -> ERROR::svlst))
                       |NAME(a) -> (match findVal a hashlst with
                                    |STRING(b) -> (match res_sv with
                                                  |[] -> ERROR::svlst
                                                  |sv2::res_sv2 -> (match sv2 with
                                                                    |STRING(c) -> STRING(c^b)::res_sv2
                                                                    |NAME(c) -> (match findVal c hashlst with
                                                                                 |STRING(d) -> (STRING(d^b))::res_sv2
                                                                                 |_ -> ERROR::svlst)
                                                                    |_ -> ERROR::svlst))
                                    |_ -> ERROR::svlst)
                       |_ -> ERROR::svlst
    in
    let catwithBind (svlst : stackValue list) (hashlst : (string * stackValue) list) =
        match svlst with
        |[] -> hashlst
        |sv::res_sv -> match sv with
                       |NAME(a) -> (match findVal a hashlst with
                                    |STRING(b) -> (match res_sv with
                                                   |[] -> hashlst
                                                   |sv2::res_sv2 -> (match sv2 with
                                                                    |NAME(c) -> (match findVal c hashlst with
                                                                                |STRING(d) -> (a, STRING(d^b))::hashlst
                                                                                |_ -> hashlst)
                                                                    |STRING(c) -> (a, STRING(c^b))::hashlst
                                                                    |_ -> hashlst))
                                    |_-> hashlst)
                        |STRING(a) -> (match res_sv with
                                      |[] -> hashlst
                                      |sv2::res_sv2 -> (match sv2 with
                                                       |NAME(b) -> (match findVal b hashlst with
                                                                   |STRING(c) -> hashlst
                                                                   |_ -> hashlst)
                                                       |_-> hashlst))
                        |_ -> hashlst
    in

    let andOP (svlst : stackValue list) (hashlst : (string * stackValue) list)=
        match svlst with
        |[] -> ERROR::svlst
        |sv::res_sv -> match sv with
                       |NAME(a) -> (match findVal a hashlst with
                                   |BOOL(b) -> (match res_sv with
                                               |[] -> ERROR::svlst
                                               |sv2::res_sv2 -> (match sv2 with
                                                                |BOOL(c) -> BOOL(b&&c)::res_sv2
                                                                |NAME(c) -> (match findVal c hashlst with
                                                                            |BOOL(d) -> BOOL(b&&d)::res_sv2
                                                                            |_ -> ERROR::res_sv2)
                                                                |_ -> ERROR::res_sv2))
                                   |_ -> ERROR::svlst)
                       |BOOL(a) -> (match res_sv with
                                    |[] -> ERROR::svlst
                                    |sv2::res_sv2 -> (match sv2 with
                                                    |BOOL(b) -> BOOL(a&&b)::res_sv2
                                                    |NAME(b) -> (match findVal b hashlst with
                                                                |BOOL(c) -> BOOL(a&&c)::res_sv2
                                                                |_ -> ERROR::svlst)
                                                    |_ -> ERROR::svlst))
                       |_ -> ERROR::svlst
    in

    let orOP (svlst : stackValue list) (hashlst : (string * stackValue) list)=
        match svlst with
        |[] -> ERROR::svlst
        |sv::res_sv -> match sv with
                       |NAME(a) -> (match findVal a hashlst with
                                   |BOOL(b) -> (match res_sv with
                                               |[] -> ERROR::svlst
                                               |sv2::res_sv2 -> (match sv2 with
                                                                |BOOL(c) -> BOOL(b||c)::res_sv2
                                                                |NAME(c) -> (match findVal c hashlst with
                                                                            |BOOL(d) -> BOOL(b||d)::res_sv2
                                                                            |_ -> ERROR::res_sv2)
                                                                |_ -> ERROR::res_sv2))
                                   |_ -> ERROR::svlst)
                       |BOOL(a) -> (match res_sv with
                                    |[] -> ERROR::svlst
                                    |sv2::res_sv2 -> (match sv2 with
                                                    |BOOL(b) -> BOOL(a||b)::res_sv2
                                                    |NAME(b) -> (match findVal b hashlst with
                                                                |BOOL(c) -> BOOL(a||c)::res_sv2
                                                                |_ -> ERROR::svlst)
                                                    |_ -> ERROR::svlst))
                       |_ -> ERROR::svlst
    in
    let notwithBind (svlst : stackValue list) (hashlst : (string * stackValue) list ) =
        match svlst with
        |[] -> hashlst
        |sv::res_sv -> match sv with
                       |NAME(a) -> (match findVal a hashlst with
                                   |BOOL(b) -> (a, BOOL(not b))::hashlst
                                   |_ -> hashlst)
                       |_ -> hashlst
    in
    let notOP (svlst : stackValue list) (hashlst : (string * stackValue) list) =
        match svlst with
        |[] -> ERROR::svlst
        |sv::res_sv -> match sv with
                       |BOOL(a) -> BOOL(not a)::res_sv
                       |NAME(a) -> (match findVal a hashlst with
                                   |BOOL(b) -> BOOL(not b)::res_sv
                                   |_ -> ERROR::svlst)
                       |_ -> ERROR::svlst
    in

    let equalOP (svlst : stackValue list) (hashlst : (string * stackValue)list)=
        match svlst with
        |[] -> ERROR::svlst
        |sv::res_sv -> match sv with
                       |NAME(a) -> (match findVal a hashlst with
                                   |INT(b) -> (match res_sv with
                                              |[] -> ERROR::svlst
                                              |sv2::res_sv2 -> (match sv2 with
                                                               |INT(c) -> BOOL(b=c)::res_sv2
                                                               |NAME(c) -> (match findVal c hashlst with
                                                                           |INT(d) -> BOOL(b=d)::res_sv2
                                                                           |_ -> ERROR::svlst)
                                                               |_ -> ERROR::svlst))
                                   |_ -> ERROR::svlst)
                       |INT(a) -> (match res_sv with
                                  |[] -> ERROR::svlst
                                  |sv2::res_sv2 -> (match sv2 with
                                                   |INT(b) -> BOOL(a=b)::res_sv2
                                                   |NAME(b) -> (match findVal b hashlst with
                                                               |INT(c) -> BOOL(a=c)::res_sv2
                                                               |_ -> ERROR::svlst)
                                                   |_ -> ERROR::svlst))
                       |_ -> ERROR::svlst
    in

    let lessOP (svlst : stackValue list) (hashlst : (string * stackValue)list)=
        match svlst with
        |[] -> ERROR::svlst
        |sv::res_sv -> match sv with
                       |NAME(a) -> (match findVal a hashlst with
                                   |INT(b) -> (match res_sv with
                                              |[] -> ERROR::svlst
                                              |sv2::res_sv2 -> (match sv2 with
                                                               |INT(c) -> BOOL(c<b)::res_sv2
                                                               |NAME(c) -> (match findVal c hashlst with
                                                                           |INT(d) -> BOOL(d<b)::res_sv2
                                                                           |_ -> ERROR::svlst)
                                                               |_ -> ERROR::svlst))
                                   |_ -> ERROR::svlst)
                       |INT(a) -> (match res_sv with
                                  |[] -> ERROR::svlst
                                  |sv2::res_sv2 -> (match sv2 with
                                                   |INT(b) -> BOOL(b<a)::res_sv2
                                                   |NAME(b) -> (match findVal b hashlst with
                                                               |INT(c) -> BOOL(c<a)::res_sv2
                                                               |_ -> ERROR::svlst)
                                                   |_ -> ERROR::svlst))
                       |_ -> ERROR::svlst
    in

    let bindOP (svlst : stackValue list) (hashlst : (string * stackValue) list) =
        match svlst with
        |[] -> ERROR::svlst
        |sv::res_sv -> (match res_sv with
                       |[] -> ERROR::svlst
                       |sv2::res_sv2 -> (match sv2 with
                                        |NAME(a) -> (match sv with
                                                    |INT(b) -> UNIT::res_sv2
                                                    |STRING(b) -> UNIT::res_sv2
                                                    |BOOL(b) -> UNIT::res_sv2
                                                    |UNIT -> UNIT::res_sv2
                                                    |NAME(b) -> (match findVal b hashlst with
                                                                |NONE -> ERROR::svlst
                                                                |_ -> UNIT::res_sv2)
                                                    |_ -> ERROR::svlst)
                                        |_ -> ERROR::svlst))
    in
    let bindMAP (svlst : stackValue list) (hashlst : (string*stackValue) list ) =
        match svlst with
        |[] -> hashlst
        |sv::res_sv -> (match res_sv with
                       |[] -> hashlst
                       |sv2::res_sv2 -> (match sv2 with
                                        |NAME(a) -> (match sv with
                                                    |INT(b) -> (a,sv)::hashlst
                                                    |STRING(b) -> (a,sv)::hashlst
                                                    |BOOL(b) -> (a,sv)::hashlst
                                                    |NAME(b) -> (match findVal b hashlst with
                                                                |NONE -> hashlst
                                                                |_ -> (a, findVal b hashlst)::hashlst)
                                                    |UNIT -> (a, sv)::hashlst
                                                    |_ -> hashlst)
                                        |_-> hashlst))
    in

    let ifOP (svlst : stackValue list) (hashlst : (string * stackValue) list) =
        if(List.length svlst < 3) then ERROR::svlst
        else match svlst with
             |[]-> svlst
             |sv::res_sv -> match res_sv with
                            |[] -> svlst
                            |sv2::res_sv2 -> match res_sv2 with
                                             |[] -> svlst
                                             |sv3::res_sv3 -> match sv3 with
                                                              |NAME(a) -> (match findVal a hashlst with
                                                                          |BOOL(b) -> (match b with
                                                                                      |true -> sv::res_sv3
                                                                                      |false -> sv2::res_sv3)
                                                                          |_ -> ERROR::svlst)
                                                              |BOOL(a) -> (match a with
                                                                          |true -> sv::res_sv3
                                                                          |false -> sv2::res_sv3)
                                                              |_ -> ERROR::svlst
    in
    let endOP (biglst : (stackValue list) list) =
        match biglst with
        |[] -> biglst
        |l1::res_lst -> match res_lst with
                  |[] -> biglst
                  |l2::res_lst2 -> match l1 with
                                   |[] -> res_lst
                                   |sv::res_sv -> (sv::l2)::res_lst2
    in
    let endwithBind (bighashlst : ((string * stackValue) list) list) =
        match bighashlst with
        |[] -> bighashlst
        |l1::res_hashlst -> res_hashlst
    in

    let rec inter (strlst : string list) (bighashlst : ((string * stackValue) list) list) (bigsvlst : (stackValue list) list) =
        match strlst with
        |[] -> strlst
        |s::res_s -> if(List.length bigsvlst = 0 && List.length bighashlst = 0) then inter strlst ([]::bighashlst) ([]::bigsvlst)
                     else
                     (match bigsvlst with
                     |[] -> strlst
                     |svlst::res_lst -> (match bighashlst with
                                        |[] -> strlst
                                        |hashlst::res_hashlst ->

                    if(String.length s <= 2) then
                     (match s with
                     |"or" -> inter res_s bighashlst ((orOP svlst hashlst)::res_lst)
                     |"if" -> inter res_s bighashlst ((ifOP svlst hashlst)::res_lst)
                     |_ -> inter res_s bighashlst ((ERROR::svlst)::res_lst))
                     else let ss = String.sub s 0 3 in
                     match ss with
                     |"pus" -> (let value = String.sub s 5 ((String.length s) - 5) in
                                let ch = String.get value 0 in
                                match ch with
                                |'"' -> let str = String.sub value 1 ((String.length value) - 2) in
                                        inter res_s bighashlst ((STRING(str)::svlst)::res_lst)

                                |'_' -> inter res_s bighashlst ((NAME(value)::svlst)::res_lst)

                                |':' -> (let strLen = String.length value in
                                        let ch2 = String.sub value 1 ((String.length value) - 1) in
                                        if(strLen = 6) then
                                        match ch2 with
                                        |"true:" -> inter res_s bighashlst ((BOOL(true)::svlst)::res_lst)
                                        |"unit:" -> inter res_s bighashlst ((UNIT::svlst)::res_lst)
                                        |_ -> inter res_s bighashlst ((ERROR::svlst)::res_lst)

                                        else if (strLen = 7) then
                                        match ch2 with
                                        |"false:" -> inter res_s bighashlst ((BOOL(false)::svlst)::res_lst)
                                        |"error:" -> inter res_s bighashlst ((ERROR::svlst)::res_lst)
                                        |_ -> inter res_s bighashlst ((ERROR::svlst)::res_lst)

                                        else inter res_s bighashlst ((ERROR::svlst)::res_lst))

                                 |'-' -> let zero = String.get value 1 in
                                         if(zero == '0') then let z = String.sub value 1 ((String.length value) - 1) in
                                                              (try int_of_string z; inter res_s bighashlst ((INT(int_of_string z)::svlst)::res_lst)
                                                              with _ -> inter res_s bighashlst ((ERROR::svlst)::res_lst))
                                         else (try int_of_string value; inter res_s bighashlst ((INT(int_of_string value)::svlst)::res_lst)
                                              with _ -> inter res_s bighashlst ((ERROR::svlst)::res_lst))

                                | _ -> let asc = Char.code ch in
                                        if((asc >= 65 && asc <= 90) || (asc >= 97 && asc <= 122)) then
                                            inter res_s bighashlst ((NAME(value)::svlst)::res_lst)
                                        else try int_of_string value; inter res_s bighashlst ((INT(int_of_string value)::svlst)::res_lst)
                                             with _ -> inter res_s bighashlst ((ERROR::svlst)::res_lst))

                     |"pop" -> inter res_s bighashlst ((popOP(svlst))::res_lst)
                     |"add" -> inter res_s bighashlst ((addOP svlst hashlst)::res_lst)
                     |"sub" -> inter res_s bighashlst ((subOP svlst hashlst)::res_lst)
                     |"mul" -> inter res_s bighashlst ((mulOP svlst hashlst)::res_lst)
                     |"div" -> inter res_s bighashlst ((divOP svlst hashlst)::res_lst)
                     |"rem" -> inter res_s bighashlst ((remOP svlst hashlst)::res_lst)
                     |"neg" -> inter res_s bighashlst ((negOP svlst hashlst)::res_lst)
                     |"swa" -> inter res_s bighashlst ((swapOP(svlst)::res_lst))
                     |"toS" -> inter res_s bighashlst ((toStringOP svlst hashlst::res_lst))
                     |"pri" -> inter res_s bighashlst ((printlnOP(svlst)::res_lst))
                     |"qui" -> inter [] [] []
                     |"cat" -> inter res_s bighashlst ((catOP svlst hashlst)::res_lst)
                     |"and" -> inter res_s bighashlst ((andOP svlst hashlst)::res_lst)
                     |"not" -> inter res_s bighashlst ((notOP svlst hashlst::res_lst))
                     |"equ" -> inter res_s bighashlst ((equalOP svlst hashlst)::res_lst)
                     |"les" -> inter res_s bighashlst ((lessOP svlst hashlst)::res_lst)
                     |"bin" -> inter res_s ((bindMAP svlst hashlst)::res_hashlst) ((bindOP svlst hashlst::res_lst))
                     |"let" -> inter res_s (hashlst::bighashlst) ([]::bigsvlst)
                     |"end" -> inter res_s (endwithBind bighashlst) (endOP(bigsvlst))
                     |_ -> inter res_s bighashlst ((ERROR::svlst)::res_lst)))
    in
    let stack = inter strList [] [] in
    ();;
    (*let rec a st =
        match st with
        |[] -> ()
        |s::res_s -> print_string(s^"\n");
                     a res_s;
    in

    a stack;
    ();;