
open Batteries

open Type
open Abs
open PathTree
open Effects

open Format

open Utils.Option

module L = LazyList

module AutomataSpec = struct
	let name = "Double Unlock Automata Checker"
	type st = {
		fna  : AFun.t;
		reg  : Region.t;
		effects : effects;
		kreg : Regions.t;
	}

    let init_st fna effects r = { fna; reg = r; effects = effects; kreg = Regions.empty; }

	type state = Locked | Unlocked | Error of st

	(** Test *)
    let transition previous input = 
		let locks = E.(mem (locks ~r:input.reg) input.effects) in
		let unlocks = E.(mem (unlocks ~r:input.reg) input.effects) in
		match previous with 
        | Unlocked  -> (match (locks, unlocks) with 
                        | true, _	-> Locked
                        | _, true   -> Error input
                        | _         -> previous)
        | Locked    -> (match (locks, unlocks) with 
                        | _, true   -> Unlocked
                        | _         -> previous)
        | Error e   -> Error e

    let initial_state = Unlocked

    let state_to_string state = match state with 
        | Locked -> "Locked" 
        | Unlocked -> "Unlocked" 
        | Error _ -> "Error"

	let select _fla _ _ fna =
        let feffects = AFun.sum fna in
		let all = E.(regions (feffects)) in
		L.of_enum (Enum.map (init_st fna feffects) (Regions.enum all))

	let trace st ef = Regions.(mem st.reg E.(regions ef))

	type bug = Region.t
	let bug_of_st st = st.reg
	let doc_of_report func region location trace =
		let open PP in
		brackets (!^ name) + newline +
		words "Double unlock" ++ parens(Region.pp region) + newline
		++ words "first at" ++ (Utils.Location.pp location) + newline
		(* ++ words "second at" ++ (Utils.Location.pp loc2) + newline *)
		+ !^ "In" ++ !^ Cil.(func.vname) ++ words "defined at"
		++ (Utils.Location.pp Cil.(func.vdecl)) + colon + newline
		+ PathTree.pp_path trace
end

module Checker = AutomataChecker.Make(AutomataSpec)

include Checker
