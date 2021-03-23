open OUnit2
open Libeba 

module O = OUnitAssert
module P = CfgPrinter.MakeT (CfgPrinterDoubleLock.Spec)
module Region = Libeba.Type.Region

let bound_region_unification _ = 

  let r1, r2 = Region.meta (), Region.meta () in
  let sr1, sr2 = Region.to_string r1, Region.to_string r2 in

  let pre_msg = Printf.sprintf "Fresh regions before unification differ! (%s, %s)" sr1 sr2 in
  let _ = assert_bool pre_msg (Region.equal r1 r2 |> not) in

  let _ = Region.(r1 =~ r2) in
  let ur1, ur2 = Region.to_string r1, Region.to_string r2 in
  let pre_msg = Printf.sprintf "Regions after unification still differ! (%s, %s)" ur1 ur2 in
  let _ = assert_bool pre_msg (Region.equal r1 r2 |> not) in

  let zr1, zr2 = Region.to_string (Region.zonk r1), Region.to_string (Region.zonk r2) in
  let post_msg = Printf.sprintf "Region zonks after unification equal! (%s, %s)" zr1 zr2 in
  let _ = assert_bool post_msg (Region.equal (Region.zonk r1) (Region.zonk r2)) in

  let id1, id2 = P.region_id r1, P.region_id r2 in
  let sid1, sid2 =  string_of_int id1, string_of_int id2 in
  let uniq_msg = Printf.sprintf "Region.uniq_of after unification differ (%s, %s)" sid1 sid2  in
    assert_bool uniq_msg (id1 <> id2)


let tests = "type" >::: [
  "unification equates regions, but not uniq ids" >:: bound_region_unification; 
]

let run = run_test_tt_main tests
