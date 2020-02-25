open Batteries
open Dolog

module Opts = Opts.Get
module L = LazyList

open Type
open Abs
open PathTree
open Format
open Effects

module type AutomataSpec = sig
	(** A name to identify the checker *)
	val name : string

	(** Checker's internal state, eg. memory regions to track. *)
	type checker_state = {
		fna  : AFun.t;
		reg  : Region.t;
		effects : effects;
	}

	(** States *)
	type state
	val state_to_string : state -> string
	val initial_state : state
	val compare_states : state -> state -> bool
	val is_accepting : state -> bool
	val accepted_labels : mem_kind list

	(** Selects initial contexts *)
	val select : AFun.t -> checker_state L.t
	(** Flags steps of interest for triaging. *)
	val trace : checker_state -> Effects.t -> bool
	(** Test *)
	val transition : state -> Effects.e -> state

	(** Bug data *)
	type bug
	val bug_of_st : checker_state -> bug
	val doc_of_report : Cil.varinfo -> bug -> Cil.location -> path -> PP.doc
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

	let string_of_report r = A.doc_of_report r.func r.bug r.location [] |> PP.to_string

	let generate_report declaration state  = 
		{
			func = Cil.(declaration.svar);
			bug = A.bug_of_st state;
			location = {line = 0; file = ""; byte = 0}; (* step.sloc; *)
			trace = []; (* TODO: Implement getting traces. *)
		}

	type cfg_state = Entry
	type cfg = {initial_state: A.state; transition: A.state -> Effects.e -> A.state}

	let is_in_accepted_labels effect = 
		match effect with 
		| Mem(kind, _) -> List.mem kind A.accepted_labels
		| _ -> false

	let rec explore_paths path previous_states = 
		match path() with
		| Seq(step, remaining) -> 
			let apply_transition state = 
				let accepted_input = EffectSet.filter is_in_accepted_labels step.effs.may |> EffectSet.to_list in
				let results = List.fold_left (fun acc e -> (A.transition state e)::acc) [] accepted_input in
				if List.exists A.is_accepting results then results else
				explore_paths remaining results
			in
			let mapped = List.map apply_transition previous_states |> List.flatten in
			mapped
		| Assume(_, _, c) -> 
			explore_paths c previous_states
		| If(true_path, false_path) -> 
			let true_branch = explore_paths true_path previous_states in
			let false_branch = explore_paths false_path previous_states in 
			let branch_states = List.append true_branch false_branch in
			branch_states
		| _ -> previous_states

	let search declaration (state:A.checker_state) =		
		let product initials transitions input = 
    		match initials, transitions with 
    		| (a, a2), (t1, t2) -> (t1 a input, t2 a2 input)
		in

		(* let cil_cfg = snd (CFGGeneration.create_cfg declaration) in *)
		let cfg = { initial_state = A.initial_state; transition = fun a _ -> a } in

		let partial_product = product (A.initial_state, cfg.initial_state) (A.transition, cfg.transition) in
		let automata_result = EffectSet.fold (fun e previous -> A.transition previous e) state.effects.may A.initial_state in

		let result = A.state_to_string automata_result in
		if (result = "Error") then
			Some (generate_report declaration state)
		else
			None

	let check file declaration =
		let variable_info = Cil.(declaration.svar) in
		let _, global_function = Option.get(AFile.find_fun file variable_info) in
		(* let seeds = A.select global_function in *)

		let path_tree = paths_of global_function in
		(* TODO: Convert paths to automata *)
		let results = explore_paths path_tree [A.initial_state] in 
		let pp = List.fold_left (fun acc s -> (A.state_to_string s) ^ ";" ^ acc) "" results in
		if List.exists A.is_accepting results
		then 
			L.from (fun _ -> pp)
		else L.nil

		(* 
		let bugs = seeds |> L.map (search declaration) in
		bugs |> L.filter Option.is_some |> L.map Option.get |> L.map string_of_report 
		*)



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
end
