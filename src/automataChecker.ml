open Batteries
open Dolog

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
		previous_state : state;
		current_state: state;
		trace: step list;
		matches: step list;
		id: int;
	}

	(** States *)
	val state_to_string : state -> string
	val checker_state_to_string : checker_state -> string
	val initial_state : checker_state
	val copy_state : checker_state -> checker_state
	(* val init_state : state -> step list -> state -> step list -> checker_state *)
	val compare_states : state -> state -> bool
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
	type report = {
		func    	: Cil.varinfo;
		bug   		: A.bug;
		location  	: Cil.location;
		trace		: path;
	}

	type cfg_state = Entry
	type cfg = {initial_state: A.state; transition: A.state -> Effects.e -> A.state}

	let is_in_transition_labels effect = 
		match effect with 
		| Mem(kind, _) -> List.mem kind A.transition_labels
		| _ -> false

	let rec explore_paths path states = 
		match path() with
		| Seq(step, remaining) -> 
			let apply_transition state = 
				Format.printf "%s" (A.checker_state_to_string state);
				let accepted_input = EffectSet.filter is_in_transition_labels step.effs.may |> EffectSet.to_list in
				let results = List.fold_left (fun acc e -> (A.transition state e step)::acc) [] accepted_input in
				let split = 
					(if (List.length results > 1) 
					then (List.hd results)::(List.map (fun s -> A.copy_state s) (List.tl results))
					else results)
				in
				if List.exists A.is_accepting split 
				then split
				else
				explore_paths remaining split
			in
			let mapped = List.map apply_transition states |> List.concat in
			mapped
		| Assume(_, _, remaining) -> 
			explore_paths remaining states
		| If(true_path, false_path) -> 
			let copied = List.map (fun s -> A.copy_state s) states in
			let true_branch = explore_paths true_path states in
			let false_branch = explore_paths false_path copied in 
			let branch_states = true_branch @ false_branch in
			branch_states
		| _ -> states

	let product initials transitions input = 
		match initials, transitions with 
		| (a, a2), (t1, t2) -> (t1 a input, t2 a2 input)

	let check file declaration =
		let variable_info = Cil.(declaration.svar) in
		let _, global_function = Option.get(AFile.find_fun file variable_info) in

		let path_tree = paths_of global_function in
		(* TODO: Convert paths to automata *)
		let results = explore_paths path_tree [A.initial_state] in 
		let matches = List.filter (fun m -> A.is_accepting m) results in 
		let pp = List.map (fun m -> A.pp_checker_state m) matches in
		let pp_list = List.map (fun m -> PP.to_string m) pp in
		L.of_list pp_list
end

	(** Might be needed later for duplicate elimination
	let same_loc (_,s1,_,_) (_,s2,_,_) = Cil.compareLoc s1.sloc s2.sloc = 0
	let cmp_match (_,s1,p1,_) (_,s2,p2,_) :int =
		let l1 = s1.sloc in
		let l2 = s2.sloc in
		let lc = Cil.compareLoc l1 l2 in
		(* Order reversed so that L.unique_eq will take the simplest match *)
		if lc = 0
		then
			let pc = Int.compare (List.length p1) (List.length p2) in
			if pc = 0
			then -(compare p1 p2)
			else -pc
		else -lc 

	(* Remove redundant traces keeping the shortest one (wrt [cmp_match]). *)
	let nodup = L.(unique_eq ~eq:same_loc % (sort ~cmp:cmp_match)) 
	**)
