open Batteries

module L = LazyList

open Type
open Abs
open PathTree
open Effects
open PathTree
open Dolog

module type AutomataSpec = sig
	(** A name to identify the checker *)
	val name : string
	type state

	(** Checker's internal state, eg. memory regions to track. *)
	type checker_state = {
		current_state: state;
		trace: step list;
		matches: step list;
		kill_region: Regions.t
	}

	type result = Okay of checker_state | Uncertain of checker_state

	(** States *)
	val initial_state : step -> AFun.t -> checker_state
	val is_accepting : checker_state -> bool
	val does_write : effects -> checker_state -> bool
	val should_permute : bool
	val is_error : checker_state -> bool
	val transition_labels : mem_kind list
	val pp_checker_state : result -> SmartPrint.t
	val checker_state_to_string : result -> string
	val filter_results : result list -> result list

	(** Test *)
	val transition : result -> Effects.e list -> step -> result
end

module type S = sig
	type checker_result
	val check : AFile.t -> Cil.fundec -> bool -> checker_result list
	val filter_results : checker_result list -> checker_result list
	val stringify_results : checker_result list -> string list
end

module Make (A : AutomataSpec) : S = struct
	type checker_result = A.result
	type checking = Must | May
	type inlined = Not_Inlined | Inlined

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

	let apply_to_region (e_r: region * e list) success = 
		match e_r with 
		| _, e -> success e

	let extract_regions r_es = 
		let split = List.split r_es in
		(List.hd (fst split), (snd split))

	let extract_checker_state (state:checker_result) = 
		match state with
		| Okay s -> s
		| Uncertain s -> s

	let print_map m step string = 
		if Map.is_empty m then Log.debug "Reached %s %s %s" (Utils.Location.pp step.sloc |> PP.to_string) string "Empty"
		else
			let gen_names ss = List.fold_right (fun s acc -> Format.sprintf "%s " (A.checker_state_to_string s) ^ acc ) ss "" in
			Log.info "Reached %s %s" (Utils.Location.pp step.sloc |> PP.to_string) string;
			BatMap.iter (fun k v -> Log.debug "%s: %s" (Region.pp k |> PP.to_string) (v |> gen_names)) m

	let stringify_effects effects = 
		List.fold_right (fun e acc -> Format.sprintf "%s %s " (pp_e e |> PP.to_string) acc) effects ""

	let get_region e = 
		match e with
		| Mem(_, region) -> Some (region, e)
		| _ 			 -> None

	let is_uncertain_result results = 
		List.exists 
		(fun r -> match (r:checker_result) with | Uncertain _ -> true | Okay _ -> false) 
		results 

	let rec explore_paths func path map check_type inlined = 
		let p = path() in
		match p with
		| Seq(step, remaining) -> 
			let input = (match check_type with
				| May 	-> EffectSet.filter is_in_transition_labels step.effs.may
				| Must 	-> EffectSet.filter is_in_transition_labels step.effs.must) 
			|> EffectSet.to_list
			in

			let region_options = List.map get_region input in
			let regions = List.fold_right (fun e acc -> (match e with Some r -> r::acc | None -> acc)) region_options [] in 
			
			let grouped = List.group (fun r r' -> Region.compare (fst r) (fst r')) regions 
				|> List.map extract_regions 
			in

			(match inlined with 
			| Inlined when not (List.is_empty input) -> Log.debug "Inlined effects are %s" (stringify_effects input);
			| _ -> ());

			(* Skip step if the effects are uninteresting *)
			if List.is_empty input 
			then explore_paths func remaining map check_type inlined
			else
				let apply_transition effects (map_to_add_to:(region, checker_result list) BatMap.t) = 
					let result = List.fold_right (fun (r_e:region * e list) map -> 
						match r_e with | r, es -> 
							(* Find the previous result if present, then determine new checker_state. *)
							let initial : checker_result list = [(Okay (A.initial_state step func))] in 
							let result : A.result list = Map.find_default initial r map_to_add_to in 
							let applied = 
								List.map (fun (s: checker_result) -> 
									match s with 
									| Okay state -> 
										if A.does_write step.effs state || A.is_accepting state
										then s 
										else A.transition s es step
									| Uncertain _ -> s
								) result 
							in
							Map.add r applied map) effects map_to_add_to in
					result
				in

				let without_accepting_short_term_checkers = 
					Map.filter_map (fun _ value -> 
						let filtered : checker_result list = List.filter (fun state -> 
							let s = (extract_checker_state state) in 
							not (A.is_accepting s) || A.is_error s) value 
						in
						if List.is_empty filtered then None else Some filtered)
					map
				in

				(* 	For each effect in a given permutation, apply the transition function, 
					and add the result to the (region, checker_state) -> [checker_state] map. *)
				let without_short_term = apply_transition grouped without_accepting_short_term_checkers in

				let uncertainty_check map = 
					match inlined with 
					| Not_Inlined -> (
						let is_uncertain = Map.exists (fun r results -> 
							List.exists (fun t -> Region.compare (fst t) r = 0) grouped 
							&& is_uncertain_result results) 
							map 
						in
						if not is_uncertain then map
						else
							let inline_result = inline func step in
							match inline_result with 
							| Some (_, t) -> 
								Log.info "Performed inline, will explore inlined tree";
								explore_paths func t map Must Inlined 
							| None -> 
								Log.info "Could not inline, will not try again";
								map
					)
					(* Give up on eliminating uncertainty if we have already inlined. *)
					| Inlined -> 
						Log.info "Already inlined, will not try again"; 
						map
				in
				
				let checked = uncertainty_check without_short_term in 
				(* print_map checked "Map contains: "; *)
				explore_paths func remaining checked check_type inlined
		| Assume(_, _, remaining) -> 
			explore_paths func remaining map check_type inlined
		| If(true_path, false_path) -> 
			let true_branch = explore_paths func true_path map check_type inlined in
			let false_branch = explore_paths func false_path map check_type inlined in
			(* Format.printf "Merging maps... %s\n" ""; *)
			let merge = 
				Map.merge (fun _ a b -> 
					(match a, b with 
					| Some aa, Some bb -> Some (aa @ bb)
					| None, Some _ -> b
					| Some _, None -> a
					| None, None -> None)
				) true_branch false_branch in 
			merge
		| Nil -> map

	let check file declaration nonstatic_only =
		let variable_info = Cil.(declaration.svar) in
		match variable_info.vstorage with 
		| Static when nonstatic_only -> []
		| _ ->
			let _, global_function = Option.get(AFile.find_fun file variable_info) in
			let path_tree = paths_of global_function in
			let results = explore_paths global_function path_tree Map.empty May Not_Inlined in 
			let okay_only = results |> Map.map (fun matches -> 
				List.filter (fun (m: checker_result) -> match m with | Okay _ -> true | Uncertain _ -> false) 
				matches)
			in
			let error_only = okay_only |> Map.map (fun matches -> List.filter (fun m -> A.is_error (extract_checker_state m)) matches) in
			let first_only = error_only |> Map.map (fun matches -> match matches with | x::_ -> [x] | [] -> []) in

			let states = Map.values first_only in
			let matches = Enum.fold (fun acc matches -> matches @ acc) [] states in
			let matches_reversed = List.rev matches in 
			matches_reversed

	let filter_results matches = A.filter_results matches

	let stringify_results matches = 
		let pp = List.map (fun m -> A.pp_checker_state m) matches in
		let pp_list = List.map (fun m -> PP.to_string m) pp in
		pp_list
end