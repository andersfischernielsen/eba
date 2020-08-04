
open Batteries

open Type
open PathTree
open Effects
open Random

module L = LazyList

module AutomataSpec = struct
	type state = 
		| Unlocked
		| Locked
		| Error of Effects.e

	let transition_labels = [Lock; Unlock] 

	let name = "Double Unlock Automata Checker"
	
	type checker_state = {
		current_state: state;
		trace: step list;
		matches: step list;
		kill_region: Regions.t
	}

	type result = Okay of checker_state | Uncertain of checker_state

	let extract_state result = 
		match result with
		| Okay s -> s
		| Uncertain s -> s 

	let should_permute = true

	let state_to_string state = 
		let open PP in
		let st = match state with 
		| Locked 	-> words "Locked"
        | Unlocked -> words "Unlocked"
        | Error e -> words "Error" ++ brackets (Effects.pp_e e) in
		st |> PP.to_string

	let checker_state_to_string result = 
		let certainty = match result with Uncertain _ -> "Uncertain" | Okay _ -> "Okay" in
		let state = extract_state result in 
		Format.sprintf "%s { current_state=%s }" certainty (state_to_string state.current_state)

	let initial_state step func = 
		{ 
			current_state = Unlocked; 
			trace = []; 
			matches = [];
			kill_region = Option.Infix.(
				find_in_stmt CilExtra.find_linux_lock_in_call step >>= Lenv.kregions_of func |? Regions.empty);
		}

	let does_write effects state = 
		Regions.exists (fun r -> (E.(mem (writes ~r) effects))) state.kill_region
	
	let with_previous state _new step = 
		let matches = match _new with | Error _ -> step::state.matches | _ -> state.matches in
		let new_state = { 
			trace=step::state.trace; 
			current_state=_new; 
			matches=matches;
			kill_region=state.kill_region;
		} in
		new_state
	
	let print_es es text = 
		Format.printf "%s from:\n" text;
		List.iter (fun e -> pp_e e |> PP.to_string |> Format.printf "%s, ") es;
		Format.printf "%s\n" ""

    let transition previous input step = 
		let previous_checker_state = (extract_state previous) in
		let previous_state = (extract_state previous).current_state in
		let add_step_with new_state = with_previous previous_checker_state new_state step in
		match previous_state with 
		| Unlocked ->
			(match input with 
			| [Mem(Lock, _)]					-> (* print_es input "Unlock -> Lock"; *) Okay (add_step_with Locked)
			| [Mem(Unlock, _) as a]				-> Okay (add_step_with (Error a))
			| [Mem(Unlock, _); Mem(Lock, _)] 	-> Uncertain (add_step_with previous_state)
			| [Mem(Lock, _); Mem(Unlock, _)] 	-> Uncertain (add_step_with previous_state)
			| _         						-> previous
			)
        | Locked ->
			(match input with 
			| [Mem(Unlock, _)]					-> Okay (add_step_with Unlocked)
			| [Mem(Unlock, _); Mem(Lock, _)] 	-> Uncertain (add_step_with previous_state)
			| [Mem(Lock, _); Mem(Unlock, _)] 	-> Uncertain (add_step_with previous_state)
			| _         						-> previous
			)
        | Error _								-> previous
	
	let is_accepting state = 
		match state.current_state with 
		| Error _ 	-> true
		| _ 		-> false

	let is_error state = is_accepting state

	let pp_checker_state (result:result) = 
		let open PP in 
		let state = extract_state result in 
		let matches = List.rev state.matches in 
		let trace = List.rev state.trace in 
		let match_locations = List.fold_left (fun acc m -> acc ++ Utils.Location.pp m.sloc ++ words (string_of_step m) + newline) newline matches in
		let trace_locations = List.fold_left (fun acc m -> acc ++ Utils.Location.pp m.sloc ++ words (string_of_step m) + newline) newline trace in
		
		brackets (!^ name) + newline + newline
		++ words "at:" ++ match_locations + newline
		++ words "trace:" ++ trace_locations + newline

	let filter_results (results: result list) = 
		let sorted = List.sort 
			(fun a b -> Int.compare (List.length (extract_state a).trace) (List.length (extract_state b).trace)) 
			results in 

		let trace_repeats l = match l with
			| [] | [_] -> false
			| (head::tail) -> List.exists (fun t -> Cil.compareLoc head.sloc t.sloc = 0) tail in

		let no_duplicate_regions = List.fold_right (fun r acc -> 
			let current = extract_state r in
			if List.exists (fun e -> Set.mem e (snd acc)) current.matches || trace_repeats current.trace
			then acc (* If a step has already been detected, skip it. *) 
			else (* Otherwise, include it. *)
				let union = Set.union (Set.of_list current.matches) (snd acc) in
				(r::(fst acc), union))
		sorted ([], Set.empty) in 

		(* TODO: Fix filtering issue. *)
		fst no_duplicate_regions
end

module Checker = AutomataChecker.Make(AutomataSpec)

include Checker
