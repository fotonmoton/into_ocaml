
type priority = int

type 'a queue = Empty | Node of priority * 'a * 'a queue * 'a queue

let priv () = print_endline "private func"

let rec insert queue prio elm =
  priv ();
  match queue with
    | Empty -> Node(prio, elm, Empty, Empty)
    | Node(p, e, left, right) ->
        if (prio <= p)
        then Node(prio, elm, insert right p e, left)
        else Node(p, e, insert right prio elm, left)