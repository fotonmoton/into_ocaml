(* class declaration *)
class point initial =

  (* this let binding is evaluated before object construction *)
  let origin = (initial / 10) * 10 in


  object (self)
    val mutable x = origin
    method get_x = x
    method move d = x <- x + d
    method get_offest = x - origin
    method print_point = print_endline ("point value: " ^ (string_of_int x))
    method print_offset = print_endline ("point offest: " ^ string_of_int self#get_offest)

    (* this method is called when object has been built *)
    initializer print_endline "new point"
  end

(* object instantiation *)
let p = new point 20

(* method call *)
let () = p#move 10
let () = p#print_point
let () = p#print_offset
let () = p#move (-20)
let () = p#print_point
let () = p#print_offset

(* function that creates object *)
let point_creator initial =
  object (self)
    val mutable x = initial
    method get_x = x
    method move d = x <- x + d
    method get_offest = x - initial
    method print_point = print_endline ("point value: " ^ (string_of_int x))
    method print_offset = print_endline ("point offest: " ^ string_of_int self#get_offest)
    initializer print_endline "new point"
  end

let p2 = point_creator 20

let () = p2#move 10
let () = p2#print_point
let () = p2#print_offset
let () = p2#move (-20)
let () = p2#print_point
let () = p2#print_offset


type position = int * int

(* class type *)
class type car_type =
  object
    method go : unit
    method get_position : position
  end

class car : car_type =
  object
    method go = ()
    method get_position = (1, 2)
  end

(* polymorphic class *)
class ['a] oref (init : 'a) =
  object
    val mutable x = init
    method get = x
    method set y = x <- y
  end