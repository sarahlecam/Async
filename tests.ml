open Async.Std
open Exercises

let test_async_eq (d : 'a Deferred.t) (v : 'a) : bool =
  Thread_safe.block_on_async (fun () -> d) = Core.Std.Result.Ok v

(******************************************************************************)
(** Unit tests for Exercises **************************************************)
(******************************************************************************)

(** Note: you do not need to write unit tests for job. *)

(* Both *)
TEST "Both w/ job and same delay" =
  test_async_eq (both (job "hi" 0.3) (job "hey" 0.3)) ("hi", "hey")
TEST "Both w/ job and different delay" =
  test_async_eq (both (job "hi" 1.) (job "hey" 5.)) ("hi", "hey")
TEST "Both w/ return" =
  test_async_eq (both (return 2) (return "hey")) (2, "hey")

(* Fork *)
TEST "fork has unit tests" =
  (* test_async_eq (fork (return 0.4)
    (fun x -> job "hi" x)
    (fun x -> job "hey" x))
  () *)
  failwith "TODO"

TEST "parallel_map has unit tests" = failwith "TODO"

TEST "sequential_map has unit tests" = failwith "TODO"

TEST "any has unit tests" = failwith "TODO"

(******************************************************************************)
(** Unit tests for AQueue *****************************************************)
(******************************************************************************)

(** Note: you do not have to write tests for create *)

TEST "push has unit tests" = failwith "TODO"

TEST "pop has unit tests" = failwith "TODO"

TEST "is_empty has unit tests" = failwith "TODO"


