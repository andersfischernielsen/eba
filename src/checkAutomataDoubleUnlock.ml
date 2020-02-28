
open Batteries

open Type
open PathTree
open Effects
open Random

module L = LazyList

module AutomataSpec = struct
	type state = 
		| Initial
		| Locked of Region.t
		| Unlocked of Region.t
		| Error of Effects.e * Region.t

	let transition_labels = [Lock; Unlock] 

	let name = "Double Unlock Automata Checker"
	
	type checker_state = {
		previous_state : state;
		current_state: state;
		trace: step list;
		matches: step list;
		id: int;
	}

	let state_to_string state = 
		let open PP in
		let st = match state with 
		| Initial -> words "Initial"
        | Locked r 	-> words "Locked" ++ brackets (Region.pp r)
        | Unlocked r -> words "Unlocked" ++ brackets (Region.pp r)
        | Error (e, r) -> words "Error" ++ brackets (Effects.pp_e e + comma ++ (Region.pp r)) in
		st |> PP.to_string

	let checker_state_to_string state = 
		Format.sprintf "{\n  id=%i\n  previous_state=%s\n  current_state=%s\n}\n" 
			(state.id) (state_to_string state.previous_state) (state_to_string state.current_state)

	let initial_state = 
		let init = Initial in
		let r = Random.int 2000000 in
		{ 
			previous_state=init; 
			current_state=init; 
			trace=[]; 
			matches=[];
			id=r;
		}
	
	let copy_state state = 
		let r = Random.int 2000000 in
		{ 
			previous_state=state.previous_state; 
			current_state=state.current_state; 
			trace=state.trace; 
			matches=state.matches;
			id=r;
		}

	let with_previous state _new step = 
		let matches = match _new with | Error _ -> step::state.matches | _ -> state.matches in
		let new_state = { 
			previous_state=state.current_state; 
			trace=step::state.trace; 
			current_state=_new; 
			matches=matches;
			id=state.id
		} in
		(* Format.printf "%s" (checker_state_to_string new_state); *)
		new_state
		
	(** Test *)
	let is_same_region f s = Region.compare f s = 0

    let transition previous input step = 
		let next new_current = with_previous previous new_current step in
		let previous_state = previous.current_state in 
		match previous_state with 
        | Initial -> 
			(match input with 
			| Mem(Lock, r)		-> next (Locked r)
			| Mem(Unlock, r)	-> next (Error (input, r))
			| _					-> next previous_state
			)
		| Unlocked r1 ->
			(match input with 
			| Mem(Lock, r2)		when is_same_region r1 r2 	-> next (Locked r2)
			| Mem(Unlock, r2) 	when is_same_region r1 r2 	-> next (Error (input, r2))
			| _         									-> next previous_state
			)
        | Locked r1 ->
			(match input with 
			| Mem(Unlock, r2) when is_same_region r1 r2 -> next (Unlocked r2)
			| _         								-> next previous_state
			)
        | Error _	-> next previous_state
	
	let is_accepting state = 
		match state.current_state with 
		| Error _ 	-> true
		| _ 		-> false

	let compare_states first second =
		match first, second with
		| Initial, Initial 						-> true
		| Locked f, Locked s 					-> is_same_region f s
		| Unlocked f, Unlocked s				-> is_same_region f s
		| Error (f, fr), Error (s, sr)			-> f =. s && is_same_region fr sr
		| _ 									-> false

	let pp_checker_state (state:checker_state) = 
		let open PP in 
		let matches = List.rev state.matches in 
		let trace = List.rev state.trace in 
		let match_locations = List.fold_left (fun acc m -> acc ++ Utils.Location.pp m.sloc ++ words (string_of_step m) + newline) newline matches in
		let trace_locations = List.fold_left (fun acc m -> acc ++ Utils.Location.pp m.sloc ++ words (string_of_step m) + newline) newline trace in
		
		brackets (!^ name) + newline +
		words "Double unlock" + newline
		++ match_locations + newline
		++ words "trace:" ++ trace_locations + newline

	type bug = Region.t
end

module Checker = AutomataChecker.Make(AutomataSpec)

include Checker
