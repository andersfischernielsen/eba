open OUnit2
open Libeba 

module P = CfgPrinter.MakeT (CfgPrinterDoubleLock.Spec)
module O = OUnitAssert

(** A trivial test, sanity check that infrastructore works *)
let test_extract_regions _ = 
  match O.raises (fun _ -> P.extract_regions []) with
  | Some (Assert_failure _)  -> 
      ()
  | Some (_) -> 
      failwith "should reject empty list with an Assert_failure" 
  | None -> 
      failwith "should reject an empty list" 


let cfgPrinter_tests = "cfgPrinter" >::: [
  "extract_regions" >:: test_extract_regions; ]

let _ = run_test_tt_main cfgPrinter_tests
