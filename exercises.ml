open Async.Std


let job name t =
  (* printing starting statement *)
  printf "Starting %s\n%!" name;
  (* creates a deferred based on the time input and binds it to the
  a function which returns a dummy string and prints the finish message *)
  (after (Core.Std.Time.Span.of_sec t)) >>=
  (fun p -> printf "Finishing %s\n%!" name; return name)

let both d1 d2 =
  (* binds d1 to ... *)
  d1 >>= (fun val1 ->
  (* binds d2 to ... *)
  d2 >>= (fun val2 ->
  (* returns the values of d1 and d2 after deferred is resolved *)
  return (val1, val2)
  ))

let fork d f1 f2 =
  (* when d is determined, concurrently runs f1 and f2 with d,
  but ignores return value *)
  upon d (fun d -> ignore(f1 d); ignore(f2 d))

(* TO-DO *)
let parallel_map f l =
  (* calls f on every element of l immediately *)
  let new_list = List.map (fun x -> f x) l in
  (*second step: make recursive function that loops through new_l to make sure
    all elements are evaluated*)
  let rec check l nl = (
    match l with
    | [] -> return nl
    | h::t -> ( h >>=
      fun x -> check t (nl@[x])
    )
  ) in
  check new_list []
  (* failwith "TODO" *)

let sequential_map f l =
  (*loop through all elements of l and wait till f x is evaluated for each
  member x of l before moving on to next element recursively*)
  let rec transform l nl = (
    match l with
    | [] -> return nl
    | h::t -> ( f h >>=
      fun x -> transform t (nl@[x])
    )
  ) in
  transform l []

(* TO-DO *)
let any ds =
  (*create ivar*)
  (* let ivar = Ivar.create in *)
  (*iterate through ds and check if any values are evaluated*)
  (*fill ivar if elmt is evaluated*)
  (* upon *)
  (*return elmt*)
  failwith "TODO"

