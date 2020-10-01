
open Batteries

open Type
open Abs
open PathTree

open Utils.Option

module CE = CilExtra

module L = LazyList

module Spec = struct
	let name = "Flow-2 double-free checker"

	type st = {
		fna  : AFun.t;
		reg  : Region.t;
		free : Cil.exp option;
		kreg : Regions.t;
	}

	let init_st fna r = { fna; reg = r; free = None; kreg = Regions.empty; }

	let select _fla _ _ fna =
		let feffects = AFun.sum fna in
		let freed = E.(regions(filter is_frees feffects)) in
		L.of_enum (Enum.map (init_st fna) (Regions.enum freed))

	let trace st ef =
		let ef_rs = E.(regions (filter (not % is_reads) ef)) in
		Regions.(mem st.reg ef_rs)

	let not_allocs r ef :bool = not E.(mem (allocs ~r) ef)

	let find_free_object = find_in_stmt CE.find_linux_free_in_call
	
	let testP1 st _ = Some st

	let testQ1 st step =
		let frees_and_not_allocs r ef = 
			let frees r ef :bool = E.(mem_must (frees ~r) ef) in
			frees r ef && not_allocs r ef in

		frees_and_not_allocs st.reg step.effs =>?
			let free_opt = find_free_object step in
			let krs = Option.Infix.(
				(free_opt >>= Lenv.kregions_of st.fna)
				|? Regions.empty
			) in
			{st with free = free_opt; kreg = krs}

	let testP2 st step =
		(* let not_writes_all rs ef :bool =
			let not_writes r ef :bool = not E.(mem (writes ~r) ef) in
			if Opts.ignore_writes() then true
			else Regions.for_all (fun r -> not_writes r ef) rs in  *)
			
		not_allocs st.reg step.effs 
			(* && not_writes_all st.kreg step.effs)  *)
			=>? st

	let testQ2_weak st step =
		let may_free r ef :bool = E.(mem (frees ~r) ef) in
		if may_free st.reg step.effs
		then
			let free_obj = find_free_object step in
			match st.free, free_obj with
			| Some lo1, Some lo2
			when Opts.match_free_exp()
			  (* If this step satisfies "Q2-strong". *)
			  && Option.is_some (testP2 st step)
			  (* Then, we compare the two lock object expressions involved
			   * and heuristically determine if, despiste aliasing information,
			   * may not denote the same lock object.
			   *)
			  && not (CE.equal_offset lo1 lo2)
			  ->
				None
			| _, _ ->
				Some st
		else None

	type bug = Region.t

	let bug_of_st st = st.reg

	let doc_of_report ~fn r ~loc1 ~loc2 ~trace =
		let open PP in
		brackets (!^ name) + newline +
		words "Double free" ++ parens(Region.pp r) + newline
		++ words "first at" ++ (Utils.Location.pp loc1) + newline
		++ words "second at" ++ (Utils.Location.pp loc2) + newline
		+ !^ "In" ++ !^ Cil.(fn.vname) ++ words "defined at"
		++ (Utils.Location.pp Cil.(fn.vdecl)) + colon + newline
		+ PathTree.pp_path trace

end

module Checker = Flow2Checker.Make(Spec)

include Checker
