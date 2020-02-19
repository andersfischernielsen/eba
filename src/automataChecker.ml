open Batteries
open Dolog

module Opts = Opts.Get
module L = LazyList

open Type
open Abs
open PathTree

module type AutomataSpec = sig
	(** A name to identify the checker *)
	val name : string

	(** Checker's internal state, eg. memory regions to track. *)
	type st

	(** States *)
	type state
	val state_to_string : st -> string
	val initial_state : state

	(** Selects initial contexts *)
	val select : AFile.t -> Cil.fundec -> shape scheme -> AFun.t -> st L.t
	(** Flags steps of interest for triaging. *)
	val trace : st -> Effects.t -> bool
	(** Test *)
	val transition : state -> st -> step -> st option

	(** Bug data *)
	type bug
	val bug_of_st : st -> bug
	val doc_of_report : Cil.varinfo -> bug -> Cil.location -> path -> PP.doc
end

module type S = sig
	val in_func : AFile.t -> Cil.fundec -> string L.t
end

module Make (A : AutomataSpec) : S = struct
	type report = {
		func    	: Cil.varinfo;
		bug   		: A.bug;
		location  	: Cil.location;
		trace		: path;
	}

	let string_of_report r = A.doc_of_report r.func r.bug r.location [] |> PP.to_string

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

	let generate_report declaration st step = 
		match st with 
		| None 		-> None
		| Some st' 	-> Some {
				func = Cil.(declaration.svar);
				bug = A.bug_of_st st';
				location = step.sloc;
				trace = [];
			}

	(* CFG *)
	(*     *)
	type cfg_state = Entry
	type cfg = {initial_state: A.state; transition: A.state -> A.st -> A.st option}
	(*         *)
	(* CFG end *)
	
	let search global_functions declaration st step =
		

		let product initials transitions input = 
    		match initials, transitions with 
    		| (a, a2), (t1, t2) -> (t1 a input, t2 a2 input)
		in

		let partial_product = product (A.initial_state, cfg.initial_state) (A.transition, cfg.transition)
		in

		let (_, automata_result) = partial_product st
		in

		let forced = Option.get automata_result in 
		generate_report declaration automata_result step

	let in_func fileAbs declaration =
		let fn = Cil.(declaration.svar) in
		let shape, global_functions = Option.get(AFile.find_fun fileAbs fn) in
		let seeds = A.select fileAbs declaration shape global_functions in
		(* let paths = paths_of global_functions in *)
		
		(**	
			TODO: Implement:
			- Generate automata for the control flow from `fnAbs`
			- Return `Some state` if accepting state is reached in product by folding over CFG/product
		*)

		(* CFG TODO begin *)
		(*                *)
		let full_cfg_test = []
		in

		let cfg : cfg = { initial_state = A.initial_state; transition = fun _ b -> Some b }
		in
		(*         *)
		(* CFG TODO end *)

		(* TODO: Convert to fold to apply previous state along with input *)
		(* TODO: Add step from CFG *)
		let bugs = seeds |> L.map (search global_functions declaration) in
		bugs |> L.filter Option.is_some |> L.map Option.get |> L.map string_of_report
end
