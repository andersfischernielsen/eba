
open Batteries

open Type
open Abs
open PathTree
open Effects

open Format

open Utils.Option

module L = LazyList

module AutomataSpec = struct
	type state = 
		| Locked of Region.t
		| Unlocked of Region.t
		| Error of Effects.e * Region.t

	let accepted_labels = [Lock; Unlock] 

	let name = "Double Unlock Automata Checker"
	
	type checker_state = {
		previous_state : state;
		current_state: state;
		trace: step list;
		matches: step list;
	}

	let state_to_string state = match state with 
        | Locked r 	-> "Locked" ^ PP.to_string (Region.pp r)
        | Unlocked r -> "Unlocked" ^ PP.to_string (Region.pp r)
        | Error (e, r) -> "Error" ^ PP.to_string (Effects.pp_e e) ^ PP.to_string (Region.pp r)

	let initial_state = 
		(* TODO: Fix! This leads to bugs never being discovered due to fresh region !== actual regions. *)
		let empty_meta = Unlocked (Region.meta ()) in 
		{ 
			previous_state=empty_meta; 
			current_state=empty_meta; 
			trace=[]; 
			matches=[] 
		}

	let init_state previous_state trace current_state matches = 
		{ previous_state; trace; current_state; matches}

	

	let with_previous state _new step = 
		Format.printf "%s" "with_previous set: \n{\n";
		Format.printf "previous_state = %s\n" (state_to_string state.current_state);
		Format.printf "current_state = %s\n" (state_to_string _new);
		Format.printf "%s" "}\n\n";
		{ 
			previous_state=state.current_state; 
			trace=step::state.trace; 
			current_state=_new; 
			matches=step::state.matches
		}
		
	(** Test *)

	let is_same_region f s = Region.compare f s = 0

    let transition previous input step = 
		let next new_current = with_previous previous new_current step in
		let previous_state = previous.current_state in 
		match previous_state with 
        | Unlocked r1 -> 
			(match input with 
			| Mem(Lock, r2)	when is_same_region r1 r2 	-> next (Locked r2)
			| Mem(Unlock, r2) when is_same_region r1 r2 -> next (Error (input, r2))
			| _         								-> next previous_state
			)
        | Locked r1 -> 
			(match input with 
			| Mem(Unlock, r2) when is_same_region r1 r2 -> next (Unlocked r2)
			| _         								-> next previous_state
			)
        | Error _   -> next previous_state
	
	let is_accepting state = 
		match state.current_state with 
		| Error _ 	-> true
		| _ 		-> false

	let compare_states first second =
		match first, second with
		| Locked f, Locked s 					-> is_same_region f s
		| Unlocked f, Unlocked s				-> is_same_region f s
		| Error (f, fr), Error (s, sr)			-> f =. s && is_same_region fr sr
		| _ 									-> false

	(* Generate states containing the region, all effects and  for a given function. *)
	(* 
	let select fna =
        let feffects = AFun.sum fna in
		let all = E.(regions (feffects)) in
		L.of_enum (Enum.map (init_st fna feffects) (Regions.enum all)) 
	*)

	(* 
	let trace state effects = Regions.(mem state.reg E.(regions effects)) 
	*)

	let pp_checker_state (state:checker_state) = 
		let open PP in 
		let locations = List.fold_left (fun acc m -> acc + words (string_of_step m) + newline) newline state.matches in
		
		brackets (!^ name) + newline +
		words "Double unlock" + newline
		++ words "at" ++ locations + newline
		(* + !^ "In" ++ !^ Cil.(func.vname) ++ words "defined at" *)
		(* ++ (Utils.Location.pp Cil.(func.vdecl)) + colon + newline *)
		(* + PathTree.pp_path trace *)

	type bug = Region.t
	(* let bug_of_st state = state.reg *)
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
