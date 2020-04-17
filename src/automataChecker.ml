open Batteries

module L = LazyList

open Type
open Abs
open PathTree
open Effects
open PathTree

module type AutomataSpec = sig
	(** A name to identify the checker *)
	val name : string
	type state

	(** Checker's internal state, eg. memory regions to track. *)
	type checker_state = {
		current_state: state;
		trace: step list;
		matches: step list;
	}

	(** States *)
	val initial_state : checker_state
	val is_accepting : checker_state -> bool
	val transition_labels : mem_kind list
	val pp_checker_state : checker_state -> SmartPrint.t
	val checker_state_to_string : checker_state -> string
	val filter_results : checker_state list -> checker_state list

	(** Test *)
	val transition : checker_state -> Effects.e -> step -> checker_state
end

module type S = sig
	type result
	val check : AFile.t -> Cil.fundec -> result list
	val filter_results : result list -> result list
	val stringify_results : result list -> string list
end

module Make (A : AutomataSpec) : S = struct
	type result = A.checker_state

	let is_in_transition_labels effect = 
		match effect with 
		| Mem(kind, _) -> List.mem kind A.transition_labels
		| _ -> false 

	let rec permute l = 
		let insert_all_positions x l = 
			let rec aux prev acc l = 
				match l with
				| [] -> (prev @ [x]) :: acc |> List.rev
				| hd::tl as l -> aux (prev @ [hd]) ((prev @ [x] @ l) :: acc) tl in 
				aux [] [] l in
		match l with 
		| [] -> []
		| [hd] -> [[hd]]
		| hd::tl -> List.fold_left (fun acc p -> acc @ insert_all_positions hd p) [] (permute tl)

	let get_region e = 
		match e with 
		| Mem(_, r) -> Some r
		(* 	If the given effect isn't a Mem, then we're not interested in it, 
			since it doesn't contain relevant information. *)
		| _ -> None

	let print_map m s = 
		if Map.is_empty m then Format.printf "%s %s" s "None\n"
		else
			let gen_names ss = List.fold_right (fun s acc -> Format.sprintf "%s " (A.checker_state_to_string s) ^ acc ) ss "" in
			Format.printf "%s\n" s;
			BatMap.iter (fun k v -> Format.printf "%s: %s" (Region.pp k |> PP.to_string) (v |> gen_names) ) m;
			Format.printf "%s" "\n"

	let rec explore_paths (func:AFun.t) path map (should_inline:bool)= 
		let p = path() in
		match p with
		| Seq(step, remaining) -> 
			let apply_transition effect map_to_add_to = 
				let region = get_region effect in 
				match region with 
				| Some r -> 
					(* Find the previous result if present, then determine new checker_state. *)
					let result = Map.find_default [A.initial_state] r map_to_add_to in 
					(* Format.printf "%s" "Hit innermost map\n"; *)
					let applied = List.map (fun s -> (A.transition s effect step)) result in
					(* Format.printf "%s" "Exited innermost map\n"; *)
					Map.add r applied map_to_add_to
				| None -> map_to_add_to
			in

			
			let input = EffectSet.filter is_in_transition_labels step.effs.must |> EffectSet.to_list in
			(* Skip step if the effects are uninteresting *)
			if List.is_empty input 
			then explore_paths func remaining map true
			else 
				let inlined_result = if not should_inline then map else 
					let inlined = inline func step in
					match inlined with
					| Some (_, t) -> explore_paths func t map false
					| None -> map
				in

				(* 	Find all permutations of effects e.g. {{lock, unlock} -> {{lock, unlock}, {unlock, lock}} 
					in order to evaluate all effect orders. *)
				let permutations = permute input in 
				let changed_map = List.fold_right (fun effects map_to_change ->
						(* 	For each effect in a given permutation, apply the transition function, 
							and add the result to the (region, checker_state) -> [checker_state] map. 
							
							This expresses that a given region has multiple state machines monitoring it 
							if multiple evaluation orders are possible for the effects of that region. *)
						List.fold_right (fun effect acc -> apply_transition effect acc) effects map_to_change
					) permutations inlined_result in
				explore_paths func remaining changed_map true
		| Assume(_, _, remaining) -> 
			explore_paths func remaining map true
		| If(true_path, false_path) -> 
			let true_branch = explore_paths func true_path map true in
			let false_branch = explore_paths func false_path map true in 
			let union = Map.union true_branch false_branch in 
			union
		| Nil -> map

	let check file declaration =
		let variable_info = Cil.(declaration.svar) in
		match variable_info.vstorage with | Static -> []
		| _ ->
			let _, global_function = Option.get(AFile.find_fun file variable_info) in
			let path_tree = paths_of global_function in
			let results = explore_paths global_function path_tree Map.empty true in 
			let states = Map.values results in
			let matches = Enum.fold (fun acc m -> (List.filter A.is_accepting m) @ acc) [] states in
			let matches_reversed = List.rev matches in 
			matches_reversed

	let filter_results matches = matches |> A.filter_results

	let stringify_results matches = 
		let pp = List.map (fun m -> A.pp_checker_state m) matches in
		let pp_list = List.map (fun m -> PP.to_string m) pp in
		pp_list
end