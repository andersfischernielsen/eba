
open Batteries

open Type
open PathTree
open Effects
open Random

module L = LazyList

module AutomataSpec = struct
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
	}

	let state_to_string state = 
		let open PP in
		let st = match state with 
        | Initial 	-> words "Initial"
		| Locked 	-> words "Locked"
        | Unlocked -> words "Unlocked"
        | Error e -> words "Error" ++ brackets (Effects.pp_e e) in
		st |> PP.to_string

	let checker_state_to_string state = 
		Format.sprintf "{ current_state=%s }" (state_to_string state.current_state)

	let initial_state = 
		{ 
			current_state = Unlocked; 
			trace = []; 
			matches = [];
		}

	let with_previous state _new step = 
		let matches = match _new with | Error _ -> step::state.matches | _ -> state.matches in
		let new_state = { 
			trace=step::state.trace; 
			current_state=_new; 
			matches=matches;
		} in
		new_state
				
    let transition previous input step = 
		let next new_state = with_previous previous new_state step in
		let previous_state = previous.current_state in 
		match previous_state with 
		| Initial ->
			(match input with 
			| Mem(Lock, _)		-> next Locked
			| Mem(Unlock, _)	-> next Unlocked
			| _         		-> next previous_state
			)
		| Unlocked ->
			(match input with 
			| Mem(Lock, _)		-> (*pp_e input |> PP.to_string |> Format.printf "%s\t\t Unlocked -> Locked\n";*) next Locked
			| Mem(Unlock, _)	-> (*pp_e input |> PP.to_string |> Format.printf "%s\t\t Unlocked -> Error\n";*) next (Error input)
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

	let pp_checker_state (state:checker_state) function_name = 
		let open PP in 
		let matches = List.rev state.matches in 
		let trace = List.rev state.trace in 
		let match_locations = List.fold_left (fun acc m -> acc ++ Utils.Location.pp m.sloc ++ words (string_of_step m) + newline) newline matches in
		let trace_locations = List.fold_left (fun acc m -> acc ++ Utils.Location.pp m.sloc ++ words (string_of_step m) + newline) newline trace in
		
		brackets (!^ name) + newline + newline
		++ words "in: " + newline + (!^ function_name) + newline
		++ words "at:" ++ match_locations + newline
		++ words "trace:" ++ trace_locations + newline
end

module Checker = AutomataChecker.Make(AutomataSpec)

include Checker
