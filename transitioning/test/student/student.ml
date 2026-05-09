open TestUtils
open OUnit2
open Fsm.Nfa
open Fsm.Regexp
open Fsm.Utils

let student_test1 _ = 
  assert_equal 42 42 ~msg:"student_test1 (1)"
;;

let suite = "student" >::: [ 
  "student_test1" >:: student_test1 
]

let _ = run_test_tt_main suite
