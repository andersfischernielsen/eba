open Batteries

module L = LazyList

open Type
open Abs
open PathTree
open Effects

module type AutomataSpec = sig
	(** A name to identify the checker *)
	val name : string
	type state

	(** Checker's internal state, eg. memory regions to track. *)
	type checker_state = {
		current_state: state;
		trace: step list;
		matches: step list;
		id: int;
	}

	(** States *)
	val initial_state : checker_state
	val compare_checker_states : region * checker_state -> region * checker_state -> int
	val is_accepting : checker_state -> bool
	val transition_labels : mem_kind list
	val pp_checker_state : checker_state -> SmartPrint.t

	(** Test *)
	val transition : checker_state -> Effects.e -> step -> checker_state

	(** Bug data *)
	type bug
end

module type S = sig
	val check : AFile.t -> Cil.fundec -> string L.t
end



module Make (A : AutomataSpec) : S = struct
	module RegionMonitorKey = 
	struct
		type t = region * A.checker_state
		let compare = fun f s -> A.compare_checker_states f s
	end
	module RegionMonitorMap = Map.Make(RegionMonitorKey)

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
		| hd::[] -> [[hd]]
		| hd::tl -> List.fold_left (fun acc p -> acc @ insert_all_positions hd p) [] (permute tl)

	let get_region e = 
		match e with 
		| Mem(_, r) -> Some r
		(* 	If the given effect isn't a Mem, then we're not interested in it, 
			since it doesn't contain relevant information. *)
		| _ -> None

	let rec explore_paths path map = 
		let p = path() in
		match p with
		| Seq(step, remaining) -> 
			let shown_step = pp_step step |> PP.to_string in 
			let shown_path = remaining() in
			let apply_transition effect map = 
				let region = get_region effect in 
				match region with 
				| Some r -> 
					(* Find the previous result if present, then determine new checker_state. *)
					let result = Map.find_default [A.initial_state] r map in 
					let m = List.fold_left (fun acc s -> A.transition s effect step :: acc) [] result in
					Map.add r m map
				| None -> map
			in

			let input = EffectSet.filter is_in_transition_labels step.effs.may |> EffectSet.to_list in
			(* Skip step if the effects are uninteresting *)
			if List.is_empty input 
			then explore_paths remaining map 
			else 
				(* 	Find all permutations of effects e.g. {{lock, unlock} -> {{lock, unlock}, {unlock, lock}} 
					in order to evaluate all effect orders. *)
				let permutations = permute input in 
				let map = List.fold_left (fun map effects ->
						(* 	For each effect in a given permutation, apply the transition function, 
							and add the result to the (region, checker_state) -> [checker_state] map. 
							
							This expresses that a given region has multiple state machines monitoring it 
							if multiple evaluation orders are possible for the effects of that region. *)
						List.fold_left (fun map effect -> apply_transition effect map) map effects
					) map permutations in
				explore_paths remaining map
		| Assume(_, _, remaining) -> 
			explore_paths remaining map
		| If(true_path, false_path) -> 
			let true_branch = explore_paths true_path map in
			let false_branch = explore_paths false_path map in 
			Map.union true_branch false_branch
		| Nil -> map
	
	let check file declaration =
		let variable_info = Cil.(declaration.svar) in
		let _, global_function = Option.get(AFile.find_fun file variable_info) in
		let path_tree = paths_of global_function in
		let results = explore_paths path_tree Map.empty in 
		let states = Map.values results in
		let matches = Enum.fold (fun acc m -> (List.filter A.is_accepting m) @ acc) [] states in
		let matches_reversed = List.rev matches in 
		let pp = List.map (fun m -> A.pp_checker_state m) matches_reversed in
		let pp_list = List.map (fun m -> PP.to_string m) pp in
		L.of_list pp_list
end