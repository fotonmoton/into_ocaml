module MyStringMap = Map.Make (String)

let data = MyStringMap.empty

let data = MyStringMap.add "key" "value" data

let data = MyStringMap.add "another key" "another value" data

let data = MyStringMap.add "k" "v" data

let print_key_value key value = 
    print_string (key ^ " " ^ value ^ "\n")

let () = MyStringMap.iter print_key_value data

let () = print_endline (MyStringMap.find "kk" data)