open OUnit2

let test1 ctxt =
  ()

let suite = "test1" >::: [
    "test1"   >:: test1
  ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()
