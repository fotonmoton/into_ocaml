(* 
    how the hell this function is invoked?! 
    (seems like it is not a function but "unit" on a left side?) 
*)
let () = print_endline "hello world!"

(* defining a function *)
let average a b = 
    (a +. b) /. 2.0


(* defining recursive functions *)
let rec factorial a = 
    if a > 0 then a * (factorial (a - 1)) 
    else 1 

let rec range a b = 
    if a > b then []
    else a :: range (a + 1) b

(* 
    Printing result of function call. 
    Still do not understand what 'let () = ' syntax meansgit .
*)
let () = print_endline (string_of_float (average 12.0 2.0))


(* converting int to string *)
let () = print_endline (string_of_int (factorial 5))

(* passing anonymous function to "functional" function iter *)
let () = List.iter 
    (fun x -> print_endline (string_of_int x))  
    (range 5 10)  


(* declaring reference *)
let reference = ref 0

(* reference reasign *)
let () = reference := 100

(* dereferencing *)
let () = print_endline (string_of_int !reference)


(* local "variable" (really local expression) *)
let average_new a b =
    let sum = a +. b in
    sum /. 2.

let () = print_endline (string_of_float (average_new 20. 30.))
