
open Batteries

open Type
open PathTree
open Effects
open Random
open Dolog

module L = LazyList

module AutomataSpec = struct
	type state = 
		| Freed
		| Allocated
		| Error of Effects.e

	let transition_labels = [Alloc; Free; Read] 

	let name = "Use-after-free Automata Checker"
	
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

	let should_permute = false

	let state_to_string state = 
		let open PP in
		let st = match state with 
		| Freed 	-> words "Freed"
        | Allocated -> words "Allocated"
        | Error e -> words "Error" ++ brackets (Effects.pp_e e) in
		st |> PP.to_string

	let checker_state_to_string result = 
		let state = extract_state result in 
		Format.sprintf "{ current_state=%s }" (state_to_string state.current_state)

	let initial_state step func = 
		{ 
			current_state = Allocated; 
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
	
	let print_es es step text = 
		Log.info "%s at %s from:" text (Utils.Location.pp step.sloc |> PP.to_string);
		List.iter (fun e -> pp_e e |> PP.to_string |> Log.info "%s, ") es

	let transition previous input step = 
		let previous_checker_state = (extract_state previous) in
		let previous_state = (extract_state previous).current_state in
		let add_step_with new_state = with_previous previous_checker_state new_state step in
		match previous_state with 
		| Allocated ->
			(match input with 
			| [Mem(Free, _)]					-> print_es input step "Allocated -> Freed"; Okay (add_step_with Freed)
			| [Mem(Free, _); Mem(Read, _)] 		-> print_es input step "Allocated -> Uncertain"; Uncertain (add_step_with previous_state)
			| [Mem(Read, _); Mem(Free, _)] 		-> print_es input step "Allocated -> Uncertain"; Uncertain (add_step_with previous_state)
			| [Mem(Free, _); Mem(Alloc, _)] 	-> print_es input step "Allocated -> Uncertain"; Uncertain (add_step_with previous_state)
			| [Mem(Alloc, _); Mem(Free, _)] 	-> print_es input step "Allocated -> Uncertain"; Uncertain (add_step_with previous_state)
			| _         						-> previous
			)
        | Freed ->
			(match input with 
			| [Mem(Read, _) as a]				-> print_es input step "Freed -> Error"; Okay (add_step_with (Error a))
			| [Mem(Alloc, _)]					-> print_es input step "Freed -> Allocated"; Okay (add_step_with Allocated)
			| [Mem(Read, _); Mem(Alloc, _)] 	-> print_es input step "Freed -> Uncertain"; Uncertain (add_step_with previous_state)
			| [Mem(Alloc, _); Mem(Read, _)] 	-> print_es input step "Freed -> Uncertain"; Uncertain (add_step_with previous_state)
			| _         						-> previous
			)
		| Error _								-> previous
	

	let is_error state = 
		match state.current_state with 
		| Error _ 			-> true
		| _ 				-> false

	let is_accepting state = is_error state

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

		fst no_duplicate_regions

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
end

module Checker = AutomataChecker.Make(AutomataSpec)

include Checker
