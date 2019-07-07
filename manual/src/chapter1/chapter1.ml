let () = print_endline "it works"

(* recursive function *)
let rec fib n =
    if n < 2 then n else fib (n - 1) + fib (n - 2)

let () = print_endline (string_of_int (fib 10))

(* higher order function *)
let deriv f dx = fun x -> (f (x +. dx) -. f x) /. dx

let sin' = deriv sin 1e-6

let pi = 4.0 *. atan 1.0

let () = print_endline (string_of_float (sin' pi))

let () = print_endline (string_of_bool (1 < 2))

(* list *)
let list = ["some"; "list"]

(* append / prepend to the list *)
let list = list @ ["at the end"]

let list = "at the start" :: list

let () = List.iter (fun x -> print_endline x) list

(* record *)
type ratio = { num : int; denom : int; }

let add_ratio r1 r2 = 
    {
        num = r1.num * r2.denom + r2.num * r1.denom;
        denom = r1.denom * r2.denom
    }

let print_ratio r = Format.printf "num: %d, denom: %d" r.num r.denom

let () = print_ratio (
    add_ratio 
        {num = 1; denom = 2} 
        {num = 2; denom = 2}
)

(* variant: two constructors and one constant *)
type number = Int of int | Float of float | Error

(* enumerated variant where all varaiants are constants *)
type sign = Positive | Negative

(* builtin "option type", this is definition of such type? *)
(* type 'q option = Some of 'q | None *)

(* generic binary tree *)
type 'q btree = Empty | Node of 'q * 'q btree * 'q btree

let rec member x tree = 
    match tree with
        | Empty -> false
        | Node(y, left, right) ->
            if x = y then true else
            if x < y then member x left else member x right

let rec insert x tree =
    match tree with
        | Empty -> Node(x, Empty, Empty)
        | Node(y, left, right) ->
            if x <= y 
            then Node(y, insert x left, right) 
            else Node(y, left, insert x right)

let rec print tree printer = 
    match tree with
        | Empty -> print_endline "empty node"
        | Node(n, left, right) ->
            printer n;
            print left printer;
            print right printer

let int_printer a = print_string (string_of_int a)

let float_priner a = print_string (string_of_float a)

let new_tree = insert 2 (insert 20 (insert 10 Empty))

let () = print new_tree int_printer

(* symbolic processing of math expression *)

type expression = 
    | Const of float
    | Var of string
    | Summ of expression * expression (* e1 +. e2 *)
    | Diff of expression * expression (* e1 -. e2 *)
    | Mult of expression * expression (* e1 *. e2 *)
    | Quot of expression * expression (* e1 /. e2 *)


(* exeception for not defined variable *)
exception Unbound_variable of string

(* expression evaluator *)
let rec eval env exp =
    match exp with
        | Const c -> c
        | Var v -> 
            (try List.assoc v env with Not_found -> raise (Unbound_variable v))
        | Summ(a, b) -> eval env a +. eval env b
        | Diff(a, b) -> eval env a -. eval env b
        | Mult(a, b) -> eval env a *. eval env b
        | Quot(a, b) -> eval env a /. eval env b

let result = eval 
    [("x", 3.14); ("y", 0.0)] 
    (Mult(Summ(Var "x", Var "y"), Const 10.0))

let () = print_endline (string_of_float result)