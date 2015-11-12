open Async.Std

type 'a t = {
  back: 'a list;
  front: 'a list
}

let create () =
  {back=[] ; front=[]}

let push q x =
  (* q <- q with {q@back} *)
  failwith "TODO"

let pop  q =
  failwith "TODO"

let is_empty q =
  match q.back, q.front with
  | [],[] -> true
  | _ -> false
