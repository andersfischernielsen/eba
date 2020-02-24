open Batteries
open Dolog

module Opts = Opts.Get
module L = LazyList

open Type
open Abs
open PathTree
open Format
open Effects

module CFGGeneration = struct
	type eba_cfg_node =
	{
		id : int list;
		predc : int list;
		succ : int list;
	}
	(*A CIL statement and its labeling in shapes & effects & regions *)
	type eba_cfg_stmtreg =
	{s : Cil.stmt list; (* The statement *)
	regs : Cil.stmt list; (*The s&e&r*)
	}

	(* The data inside each basic block *)
	type eba_cfg_data =
	{ident : int;
	stmts : Cil.stmt * Cil.stmt; (*Instructions inside the basic block*)
	}

	type eba_cfg =
	{
		(* The CFG as an adjacency list *)
		cfg : eba_cfg_node list;
		(* The data of each node in the CFG *)
		data : eba_cfg_data list;
	}

	let create_cfg (fd:Cil.fundec) =
		let basic_cfg = List.map (fun (stmt:Cil.stmt) ->
							let sid_proj (x:Cil.stmt) = x.sid in
							{id = stmt.sid::[]; predc = List.map sid_proj stmt.preds;
							succ = List.map sid_proj stmt.succs}) fd.sallstmts in

		let eq_ x y = x=y in
		let rec find_node n_id ccfg  = match ccfg with
				| hd::xs -> begin
					if List.exists (eq_ n_id) hd.id then hd else find_node n_id xs
					end
				| [] -> assert false (*Should not happen *) in
		(* Merge the cfg nodes *)
		let rec merge_cfg (cfg:eba_cfg_node list) =
			let branch_nodes = List.map (fun n -> List.hd n.id)
								(List.filter (fun (node:eba_cfg_node ) -> (List.length node.succ) > 1) cfg) in
			(**** Mergable nodes fulfill:
				1. They have only one predecessor.
				2. They have only one successor.
				3. The predecessor is not a branch node *****)
			(**** Fix point merging ****)
			try let mergable = List.find (fun (node:eba_cfg_node) ->
								(List.length node.predc) = 1 &&
									(List.length node.succ) = 1 &&
									not (List.exists (eq_ (List.hd node.predc)) branch_nodes))cfg in
				let pred_node = find_node (List.hd mergable.predc) cfg in
				let new_node = {id = List.append pred_node.id mergable.id; predc = pred_node.predc;
								succ = ((List.hd mergable.succ) ::
										(List.filter (fun n -> not (n = List.hd mergable.id)) pred_node.succ))} in
				let new_cfg = new_node :: (List.filter (fun (node:eba_cfg_node) ->
											not (List.exists (fun n -> (n = List.hd mergable.id) || (n = List.hd pred_node.id))
													node.id)) cfg) in
				merge_cfg new_cfg
			with Not_found -> cfg in
		(* Collapse long names *)
		let collapse_names (cfg:eba_cfg_node list) =
			(* Main re-label of CFG nodes but keep the original tag list so we can then add the relevant instructions from CIL nodes *)
			let dictionary = List.mapi (fun i (tags,_) -> (tags,i))
							(List.sort (fun (_,id1) (_,id2) -> if id1 < id2 then -1 else if id1 > id2 then 1 else 0)
								(List.map (fun (node:eba_cfg_node) -> (node.id, List.hd node.id)) cfg)) in
			(* Lookup final tag, aux function *)
			let rec find_tag dictionary id = match dictionary with
			| (merged, label)::xs -> if List.exists (fun merged_id -> merged_id = id) merged then label else find_tag xs id
			| [] -> assert false (*Should not happen*) in
			(* Actual re-labeling *)
			(dictionary, List.map (fun (node:eba_cfg_node) -> {id = [find_tag dictionary (List.hd node.id)];
															predc = List.map (find_tag dictionary) node.predc;
															succ = List.map (find_tag dictionary) node.succ;}) cfg )in
		let rec fill_cil dictionary = match dictionary with
			|(tags, i)::xs -> (i,List.map (fun nid -> List.find (fun (stmt:Cil.stmt) -> stmt.sid = nid)
														fd.sallstmts) tags)::fill_cil xs
			|[] -> [] in

		let (dictionary, graph) = collapse_names (merge_cfg basic_cfg) in
		(graph, fill_cil dictionary)
end

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

	let generate_report declaration state = 
		{
			func = Cil.(declaration.svar);
			bug = A.bug_of_st state;
			location = {line = 0; file = ""; byte = 0}; (* TODO: Implement getting locations. *)
			trace = []; (* TODO: Implement getting traces. *)
		}

	type cfg_state = Entry
	type cfg = {initial_state: A.state; transition: A.state -> Effects.e -> A.state}
	
	let rec explore_paths path previous_state = 
		match path() with
		| Seq(step, remaining) -> 
			let result = (EffectSet.fold (fun effect previous -> A.transition previous effect) step.effs.must A.initial_state) in 
			let tail = result |> explore_paths remaining in
			let pp = A.state_to_string tail in
			tail
		| Assume(condition, flag, c) -> 
			let explored = previous_state |> explore_paths c in 
			let pp = A.state_to_string explored in 
			explored
		| If(first_path, second_path) -> 
			let first = previous_state |> explore_paths first_path in
			let second = previous_state |> explore_paths second_path in 
			let test1 = A.state_to_string first in 
			let test2 = A.state_to_string second in 
			first
		| _ -> previous_state

	let search declaration (state:A.checker_state) =		
		let product initials transitions input = 
    		match initials, transitions with 
    		| (a, a2), (t1, t2) -> (t1 a input, t2 a2 input)
		in

		let cil_cfg = snd (CFGGeneration.create_cfg declaration) in
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
		let seeds = A.select global_function in

		let path_tree = paths_of global_function in
		(* TODO: Iterate over paths and instantiate new automata when reaching branching (like reachable in PathThree.ml) *)
		(* TODO: Convert paths to automata *)
		let result_state = explore_paths path_tree A.initial_state in 
		let state_pp = A.state_to_string result_state in
		L.from (fun _ -> state_pp)

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
