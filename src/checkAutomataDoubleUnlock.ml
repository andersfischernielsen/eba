
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
	type checker_state = {
		fna  : AFun.t;
		reg  : Region.t;
		effects : effects;
	}

    let init_st fna effects region = { fna; reg = region; effects = effects; }

	type state = Locked | Unlocked | Accept of Effects.e

	(** Test *)

    let transition previous input = 
		match previous with 
        | Unlocked  -> (match input with 
                        | Mem(Lock, _)		-> Locked
                        | Mem(Unlock, _)	-> Accept input
                        | _         		-> previous)
        | Locked    -> (match input with 
                        | Mem(Unlock, _)   	-> Unlocked
                        | _         		-> previous)
        | Accept _   -> previous

    let initial_state = Unlocked

    let state_to_string state = match state with 
        | Locked -> "Locked" 
        | Unlocked -> "Unlocked" 
        | Accept _ -> "Error"

	(* Generate states containing the region, all effects and  for a given function. *)
	let select fna =
        let feffects = AFun.sum fna in
		let all = E.(regions (feffects)) in
		L.of_enum (Enum.map (init_st fna feffects) (Regions.enum all))

	let trace state effects = Regions.(mem state.reg E.(regions effects))

	type bug = Region.t
	let bug_of_st state = state.reg
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
