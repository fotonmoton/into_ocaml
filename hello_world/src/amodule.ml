module Hello = struct
    let message = "hello, World!"
    let hello_world () = print_endline message 
end

let goodbye () = print_endline "Goodbye"

let hello_goodbye () =
    Hello.hello_world ();
    goodbye ()