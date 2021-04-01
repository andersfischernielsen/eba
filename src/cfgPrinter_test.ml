open Batteries
open OUnit2
open Libeba
open Type

module M = CfgPrinterDoubleLock.SpecT
module P = CfgPrinter.MakeT (M)
module O = OUnitAssert

(** A trivial test, sanity check that infrastructore works *)
let test_extract_regions _ =
  match O.raises (fun _ -> P.extract_regions []) with
  | Some (OUnitTest.OUnit_failure _)  ->
      ()
  | Some (_) ->
      failwith "should reject empty list with an Assert_failure"
  | None ->
      failwith "should reject an empty list" ;;

let cmp_colors (a: M.state P.set P.for_region) (b: M.state P.set P.for_region): bool =
  Map.equal Set.equal a b;;


let test_merge_colors _ =
  let r1 = Region.meta () in
  let r2 = Region.meta () in
  let n = Set.of_list [M.Red; M.Black] in
  let o = Set.of_list [M.Black] in
  let nm1, om1 = Map.singleton r1 n, Map.singleton r1 o in
  let nm2, om2 = Map.singleton r1 n, Map.singleton r2 o in
  let nm3, om3 = Map.singleton r2 n, Map.singleton r1 o in
  let nm4, om4 = Map.union nm2 om2, Map.union nm3 om3 in
    O.assert_equal (P.merge_colors nm1 om1) nm1;
    O.assert_equal (P.merge_colors nm2 om2 |> Map.find r1) (Map.union nm2 om2 |> Map.find r1);
    O.assert_equal (P.merge_colors nm2 om2 |> Map.find r2) (Map.union nm2 om2 |> Map.find r2);
    O.assert_equal (P.merge_colors nm2 om2 |> Map.cardinal) (Map.union nm2 om2 |> Map.cardinal);
    O.assert_equal ~cmp:cmp_colors (P.merge_colors nm2 om2) (Map.union nm2 om2);
    O.assert_equal ~cmp:cmp_colors (P.merge_colors nm4 om4) ([r1, n; r2, n] |> List.enum |> Map.of_enum) ;;


let cfgPrinter_tests = "cfgPrinter" >::: [
  "extract_regions" >:: test_extract_regions;
  "merge_colors" >:: test_merge_colors;
  ]

let _ = run_test_tt_main cfgPrinter_tests
