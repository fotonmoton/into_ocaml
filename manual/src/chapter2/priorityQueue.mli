type priority = int

type 'a queue = Empty | Node of priority * 'a * 'a queue * 'a queue

val insert : 'a queue -> priority -> 'a -> 'a queue