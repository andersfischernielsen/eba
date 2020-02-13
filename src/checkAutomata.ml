
open Batteries

open Type
open Abs
open PathTree

open Utils.Option

module AutomataChecker = struct
	let name = "Double Unlock Automata Checker"
	type st = {
		fna  : AFun.t;
		reg  : Region.t;
		unlock : Cil.exp option;
		kreg : Regions.t;
	}

    let init_st fna r = { fna; reg = r; unlock = None; kreg = Regions.empty; }

	type state = Locked | Unlocked | Error of mem_kind
    type accepting_state = Error of mem_kind

	(** Test *)
	val transition : st -> step -> st option
    
    let transition input previous = 
        match previous with 
        | Unlocked  -> (match input with 
                        | Lock      -> Locked
                        | Unlock    -> Error (input)
                        | _         -> previous)
        | Locked    -> (match input with 
                        | Unlock    -> Unlocked
                        | _         -> previous)
        | Error e   -> Error e

    let initial_state = Unlocked

    let to_string state = match state with 
        | Locked -> "Locked" 
        | Unlocked -> "Unlocked" 
        | Error e -> "Error (on " ^ (mem_to_string e) ^ ")"


	let select _fla _ _ fna =
        let feffects = AFun.sum fna in
		let unlocked = E.(regions(filter is_unlocks feffects)) in
		L.of_enum (Enum.map (init_st fna) (Regions.enum unlocked))

	let trace st ef =
		let ef_rs = E.(regions (filter (not % is_reads) ef)) in
		Regions.(mem st.reg ef_rs)

	type bug = Region.t
	let bug_of_st st = st.reg
	let doc_of_report ~fn r ~loc1 ~loc2 ~trace =
		let open PP in
		brackets (!^ name) + newline +
		words "Double unlock" ++ parens(Region.pp r) + newline
		++ words "first at" ++ (Utils.Location.pp loc1) + newline
		++ words "second at" ++ (Utils.Location.pp loc2) + newline
		+ !^ "In" ++ !^ Cil.(fn.vname) ++ words "defined at"
		++ (Utils.Location.pp Cil.(fn.vdecl)) + colon + newline
		+ PathTree.pp_path trace
end

module Checker = AutomataChecker.Make(Spec)

include Checker
