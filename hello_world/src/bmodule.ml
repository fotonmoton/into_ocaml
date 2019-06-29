open Amodule
open Printf

module String_set = Set.Make(String)


let () = Amodule.Hello.hello_world ()

let () = Amodule.hello_goodbye ()

let data = ["my"; "beautiful"; "data"]

let () = List.iter (fun item -> printf "%s, " item) data