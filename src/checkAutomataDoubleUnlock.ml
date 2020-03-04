
open Batteries

open Type
open PathTree
open Effects
open Random

module L = LazyList

module AutomataSpec = struct
	(* TODO: Remove "of Region.t" since it is technically unused *)
	type state = 
		| Initial
		| Locked
		| Unlocked
		| Error of Effects.e

	let transition_labels = [Lock; Unlock] 

	let name = "Double Unlock Automata Checker"
	
	type checker_state = {
		current_state: state;
		trace: step list;
		matches: step list;
		id: int;
	}

	let state_to_string state = 
		let open PP in
		let st = match state with 
		| Initial -> words "Initial"
        | Locked 	-> words "Locked"
        | Unlocked -> words "Unlocked"
        | Error e -> words "Error" ++ brackets (Effects.pp_e e) in
		st |> PP.to_string

	let checker_state_to_string state = 
		Format.sprintf "{\n  id=%i\n  current_state=%s\n}\n" 
			(state.id) (state_to_string state.current_state)

	let initial_state = 
		let init = Initial in
		let r = Random.int 1000000 in
		{ 
			current_state = init; 
			trace = []; 
			matches = [];
			id = r;
		}
	
	(* 
	let copy_state state = 
		let r = Random.int 1000000 in
		let new_state = { 
			current_state = state.current_state; 
			trace = state.trace; 
			matches = state.matches;
			id = r;
		} in 
		(* Format.printf "%s" (checker_state_to_string new_state); *)
		new_state 
	*)

	(* 
	let reset_state state = 
		let s = match state.current_state with
			| Unlocked _ -> state.current_state
			| _ -> Initial in
		let new_state = {
			current_state = s; 
			trace = [];
			matches = [];
			id = Random.int 1000000;
		} in
		(* Format.printf "%s" (checker_state_to_string new_state); *)
		new_state 
	*)

	let with_previous state _new step = 
		let matches = match _new with | Error _ -> step::state.matches | _ -> state.matches in
		let new_state = { 
			trace=step::state.trace; 
			current_state=_new; 
			matches=matches;
			id=state.id
		} in
		(* Format.printf "%s" (checker_state_to_string new_state); *)
		new_state
		
	(** Test *)
	let is_same_region f s = 
		let compare = Region.compare f s in 
		compare = 0

    let transition previous input step = 
		let next new_current = with_previous previous new_current step in
		let previous_state = previous.current_state in 
		match previous_state with 
        | Initial -> 
			(match input with 
			| Mem(Lock, _)		-> next Locked
			| Mem(Unlock, _)	-> next Unlocked
			| _					-> next previous_state
			)
		| Unlocked ->
			(match input with 
			| Mem(Lock, _)		-> next Locked
			| Mem(Unlock, _)	-> next (Error input)
			| _         		-> next previous_state
			)
        | Locked ->
			(match input with 
			| Mem(Unlock, _)	-> next Unlocked
			| _         		-> next previous_state
			)
        | Error _	-> next previous_state
	
	let is_accepting state = 
		match state.current_state with 
		| Error _ 	-> true
		| _ 		-> false

	let compare_states first second =
		match first, second with
		| Initial, Initial 		-> true
		| Locked, Locked 		-> true
		| Unlocked, Unlocked	-> true
		| Error fe, Error se	-> fe =. se
		| _						-> false

	let compare_checker_states first second = 
		if compare_states (snd first).current_state (snd second).current_state
		then Region.compare (fst first) (fst second)
		else -1

	let pp_checker_state (state:checker_state) = 
		let open PP in 
		let matches = List.rev state.matches in 
		let trace = List.rev state.trace in 
		let match_locations = List.fold_left (fun acc m -> acc ++ Utils.Location.pp m.sloc ++ words (string_of_step m) + newline) newline matches in
		let trace_locations = List.fold_left (fun acc m -> acc ++ Utils.Location.pp m.sloc ++ words (string_of_step m) + newline) newline trace in
		
		brackets (!^ name) + newline + newline
		++ words "at:"
		(* ++ words (Printf.sprintf "%d" state.id) *)
		++ match_locations + newline
		++ words "trace:" ++ trace_locations + newline

	type bug = Region.t
end

module Checker = AutomataChecker.Make(AutomataSpec)

include Checker
