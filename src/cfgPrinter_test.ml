open Batteries
open OUnit2
open Libeba
open Type

module M = CfgPrinterDoubleLock.SpecT
module P = CfgPrinter.MakeT (M)
module O = OUnitAssert

module RegionMap = P.RegionMap

(** A trivial test, sanity check that infrastructore works *)
let test_extract_regions _ =
  match O.raises (fun _ -> P.extract_regions []) with
  | Some (OUnitTest.OUnit_failure _)  ->
      ()
  | Some (_) ->
      failwith "should reject empty list with an Assert_failure"
  | None ->
      failwith "should reject an empty list" ;;

let cmp_colors (a: M.state P.set RegionMap.t)
               (b: M.state P.set RegionMap.t): bool =
  RegionMap.equal Set.equal a b;;

let cunion _ (s1: 'a Set.t) (s2: 'b Set.t) = Some (Set.union s1 s2) ;;

let test_conf_union _ =
  let r1 = Region.meta () in
  let r2 = Region.meta () in
  let n = Set.of_list [M.Orange; M.Black] in
  let o = Set.of_list [M.Black] in
  let nm1, om1 = RegionMap.singleton r1 n, RegionMap.singleton r1 o in
  let nm2, om2 = RegionMap.singleton r1 n, RegionMap.singleton r2 o in
  let nm3, om3 = RegionMap.singleton r2 n, RegionMap.singleton r1 o in
  let nm4, om4 = RegionMap.union cunion nm2 om2,  RegionMap.union cunion nm3 om3 in
    O.assert_equal (P.conf_union nm1 om1) nm1;
    O.assert_equal (P.conf_union nm2 om2 |> RegionMap.find r1) (RegionMap.union cunion nm2 om2 |> RegionMap.find r1);
    O.assert_equal (P.conf_union nm2 om2 |> RegionMap.find r2)  (RegionMap.union cunion nm2 om2 |> RegionMap.find r2);
    O.assert_equal (P.conf_union nm2 om2 |> RegionMap.cardinal) (RegionMap.union cunion nm2 om2 |> RegionMap.cardinal);
    O.assert_equal ~cmp:cmp_colors (P.conf_union nm2 om2) (RegionMap.union cunion nm2 om2);
    O.assert_equal ~cmp:cmp_colors (P.conf_union nm4 om4) ([r1, n; r2, n] |> List.enum |> RegionMap.of_enum) ;;


let cfgPrinter_tests = "cfgPrinter" >::: [
  "extract_regions" >:: test_extract_regions;
  "conf_union" >:: test_conf_union;
  ]

let _ = run_test_tt_main cfgPrinter_tests
